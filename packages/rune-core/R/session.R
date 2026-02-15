# =============================================================================
# Rune Session Model
# =============================================================================
# Each WebSocket connection creates a session. A session holds:
#   - A unique session ID
#   - The reactive signals for that user's page
#   - Registered event handlers (on_click, on_input, on_submit)
#   - Island render functions for DOM patching
#
# When a signal changes, the session re-renders affected islands and sends
# a DOM patch message over WebSocket.
# =============================================================================

#' Session store: maps session_id -> session environment.
.rune_sessions <- new.env(parent = emptyenv())

#' Create a new session.
#' @param session_id Unique ID (usually from WS connection).
#' @param ws The WebSocket connection object (httpuv ws).
#' @return A session environment.
create_session <- function(session_id, ws = NULL) {
  session <- new.env(parent = emptyenv())
  session$id <- session_id
  session$ws <- ws
  session$signals <- list()       # named list of signals
  session$handlers <- list()      # named list: element_id -> list(event_type -> handler_fn)
  session$islands <- list()       # named list: island_id -> render function
  session$island_html <- list()   # named list: island_id -> last rendered HTML
  session$route <- NULL           # current route info
  session$params <- list()        # current route params

  assign(session_id, session, envir = .rune_sessions)
  session
}

#' Get a session by ID.
get_session <- function(session_id) {
  if (exists(session_id, envir = .rune_sessions)) {
    get(session_id, envir = .rune_sessions)
  } else {
    NULL
  }
}

#' Remove a session (on disconnect).
remove_session <- function(session_id) {
  if (exists(session_id, envir = .rune_sessions)) {
    rm(list = session_id, envir = .rune_sessions)
  }
}

# =============================================================================
# Event handler registration (called from page.R during render)
# =============================================================================
# These use a session-scoped handler registry.
# The .current_session variable is set during page rendering.

.current_session_env <- new.env(parent = emptyenv())
.current_session_env$session <- NULL

#' Set the current session context (called by the server during rendering).
set_current_session <- function(session) {
  .current_session_env$session <- session
}

#' Get the current session context.
get_current_session <- function() {
  .current_session_env$session
}

#' Register a click handler for an element ID.
#' @param id The HTML element ID.
#' @param handler Function(event, session) called when the element is clicked.
on_click <- function(id, handler) {
  session <- get_current_session()
  if (is.null(session)) {
    warning("on_click called outside of session context")
    return(invisible(NULL))
  }
  if (is.null(session$handlers[[id]])) session$handlers[[id]] <- list()
  session$handlers[[id]][["click"]] <- handler
  invisible(NULL)
}

#' Register an input handler for an element ID.
#' @param id The HTML element ID.
#' @param handler Function(event, session) called when input value changes.
#'        event$value contains the current input value.
on_input <- function(id, handler) {
  session <- get_current_session()
  if (is.null(session)) {
    warning("on_input called outside of session context")
    return(invisible(NULL))
  }
  if (is.null(session$handlers[[id]])) session$handlers[[id]] <- list()
  session$handlers[[id]][["input"]] <- handler
  invisible(NULL)
}

#' Register a form submit handler.
#' @param form_id The form element ID.
#' @param handler Function(event, session) called on form submission.
#'        event$data contains form field values as a named list.
on_submit <- function(form_id, handler) {
  session <- get_current_session()
  if (is.null(session)) {
    warning("on_submit called outside of session context")
    return(invisible(NULL))
  }
  if (is.null(session$handlers[[form_id]])) session$handlers[[form_id]] <- list()
  session$handlers[[form_id]][["submit"]] <- handler
  invisible(NULL)
}

# =============================================================================
# Island registration and rendering
# =============================================================================

#' Create an interactive island.
#' Islands are regions of the page that can be independently re-rendered
#' when reactive signals change.
#'
#' @param id A stable, unique ID for this island.
#' @param ... Children (rune_nodes, text).
#' @return A rune_node wrapping the island in a div with data-rune-island attribute.
island <- function(id, ...) {
  session <- get_current_session()

  # Capture the children-producing expression for later re-rendering.
  # We wrap it so that on re-render we can call it again.
  children <- list(...)

  # Wrap in a div with the island ID
  node <- div(`data-rune-island` = id, id = paste0("rune-island-", id), ...)

  # Store current HTML for diffing
  if (!is.null(session)) {
    html <- render_html(node)
    session$island_html[[id]] <- html
  }

  node
}

#' Register an island render function for re-rendering on signal changes.
#' @param id The island ID.
#' @param render_fn A function that returns island content (rune_nodes).
register_island <- function(id, render_fn) {
  session <- get_current_session()
  if (!is.null(session)) {
    session$islands[[id]] <- render_fn
  }
  invisible(NULL)
}

#' Re-render an island and send a DOM patch if HTML changed.
#' Called when signals that an island depends on are updated.
rerender_island <- function(session, island_id) {
  if (is.null(session$islands[[island_id]])) return(invisible(NULL))

  render_fn <- session$islands[[island_id]]
  # Re-render
  set_current_session(session)
  new_content <- render_fn()
  new_node <- div(`data-rune-island` = island_id,
                  id = paste0("rune-island-", island_id),
                  new_content)
  new_html <- render_html(new_node)

  old_html <- session$island_html[[island_id]]

  if (!identical(new_html, old_html)) {
    session$island_html[[island_id]] <- new_html
    send_patch(session, island_id, new_html)
  }
}

#' Send a DOM patch message over WebSocket.
send_patch <- function(session, island_id, html) {
  if (is.null(session$ws)) return(invisible(NULL))

  msg <- jsonlite::toJSON(list(
    type = "patch",
    island = island_id,
    html = html
  ), auto_unbox = TRUE)

  tryCatch(
    session$ws$send(msg),
    error = function(e) {
      warning("Failed to send WS patch: ", e$message)
    }
  )
}

#' Toggle a CSS class on the document body.
#' @param session The session environment.
#' @param class_name The CSS class to add or remove.
#' @param add Logical. TRUE to add, FALSE to remove.
send_body_class <- function(session, class_name, add = TRUE) {
  if (is.null(session$ws)) return(invisible(NULL))
  msg <- jsonlite::toJSON(list(
    type = "body_class",
    className = class_name,
    action = if (add) "add" else "remove"
  ), auto_unbox = TRUE)
  tryCatch(session$ws$send(msg), error = function(e) {
    warning("Failed to send body_class: ", e$message)
  })
}

#' Send a Chart.js chart config over WebSocket.
#' @param session The session environment.
#' @param chart_id The canvas element ID to render the chart into.
#' @param config A list representing a Chart.js config (type, data, options).
send_chart <- function(session, chart_id, config) {
  if (is.null(session$ws)) return(invisible(NULL))

  msg <- jsonlite::toJSON(list(
    type = "chart",
    id = chart_id,
    config = config
  ), auto_unbox = TRUE, null = "null")

  tryCatch(
    session$ws$send(msg),
    error = function(e) {
      warning("Failed to send chart: ", e$message)
    }
  )
}

#' Dispatch an event received from the client.
#' @param session The session environment.
#' @param event A list with: id (element ID), type (event type), and payload data.
dispatch_event <- function(session, event) {
  element_id <- event$id
  event_type <- event$type

  if (is.null(session$handlers[[element_id]])) {
    return(invisible(NULL))
  }

  handler <- session$handlers[[element_id]][[event_type]]
  if (is.null(handler)) {
    return(invisible(NULL))
  }

  set_current_session(session)
  tryCatch(
    handler(event, session),
    error = function(e) {
      warning("Event handler error (", element_id, "/", event_type, "): ", e$message)
    }
  )
}
