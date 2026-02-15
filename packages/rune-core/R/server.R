# =============================================================================
# Rune HTTP Server
# =============================================================================
# Uses httpuv to serve HTTP requests and WebSocket connections.
# Ties together: routing, rendering, sessions, and the reactive system.
# =============================================================================

#' Start the Rune development server.
#' @param app_dir Path to the app directory (containing app/, components/, etc.).
#' @param host Host to bind to. Default "127.0.0.1".
#' @param port Port to listen on. Default 3000.
#' @param verbose Whether to log requests. Default TRUE.
rune_serve <- function(app_dir = ".", host = "127.0.0.1", port = 3000, verbose = TRUE) {
  if (!requireNamespace("httpuv", quietly = TRUE)) {
    stop("httpuv is required. Install with: install.packages('httpuv')")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite is required. Install with: install.packages('jsonlite')")
  }

  app_dir <- normalizePath(app_dir, mustWork = TRUE)
  routes_dir <- file.path(app_dir, "app")

  if (!dir.exists(routes_dir)) {
    stop("No app/ directory found in: ", app_dir)
  }

  # Source all component files
  components_dir <- file.path(app_dir, "components")
  if (dir.exists(components_dir)) {
    comp_files <- list.files(components_dir, pattern = "\\.R$", full.names = TRUE)
    for (f in comp_files) {
      source(f, local = FALSE)
    }
  }

  # Build route table
  routes <- build_route_table(routes_dir)

  if (verbose) {
    cat("Rune dev server starting...\n")
    cat("Routes discovered:\n")
    for (r in routes) {
      cat(sprintf("  %s %s -> %s\n", r$type, r$pattern,
                  basename(dirname(r$file))))
    }
    cat(sprintf("\nListening on http://%s:%d\n\n", host, port))
  }

  # Collect CSS files from components
  css_files <- list()
  if (dir.exists(components_dir)) {
    css_files <- list.files(components_dir, pattern = "\\.css$", full.names = TRUE)
  }
  # Also check for a theme file
  theme_file <- file.path(app_dir, "public", "theme.css")

  # Get client JS
  client_js <- get_client_js()

  # Build httpuv app
  app <- list(
    call = function(req) {
      tryCatch(
        handle_request(req, routes, routes_dir, app_dir, css_files,
                        theme_file, client_js, verbose),
        error = function(e) {
          if (verbose) cat("ERROR:", e$message, "\n")
          list(
            status = 500L,
            headers = list("Content-Type" = "text/html; charset=utf-8"),
            body = render_error_page(500L, e$message)
          )
        }
      )
    },
    onWSOpen = function(ws) {
      # Create a session for this WebSocket connection
      session_id <- paste0("sess_", as.integer(as.numeric(Sys.time()) * 1000), "_",
                            sample.int(100000, 1))
      session <- create_session(session_id, ws)

      if (verbose) cat("WS connected:", session_id, "\n")

      ws$onMessage(function(binary, message) {
        tryCatch({
          data <- jsonlite::fromJSON(message)

          if (data$type == "event") {
            dispatch_event(session, data)
            # After dispatching, re-render all islands for this session
            rerender_all_islands(session)
          } else if (data$type == "init") {
            # Client sends init with the current path so we can set up the session
            session$current_path <- data$path
            setup_session_page(session, routes, routes_dir, app_dir)
          }
        }, error = function(e) {
          if (verbose) cat("WS message error:", e$message, "\n")
        })
      })

      ws$onClose(function() {
        if (verbose) cat("WS disconnected:", session_id, "\n")
        remove_session(session_id)
      })
    }
  )

  server <- httpuv::startServer(host, port, app)

  on.exit(httpuv::stopServer(server))

  # Keep the server running
  tryCatch({
    while (TRUE) {
      httpuv::service(100)
      Sys.sleep(0.01)
    }
  }, interrupt = function(e) {
    cat("\nShutting down Rune server.\n")
  })
}

#' Handle an HTTP request.
handle_request <- function(req, routes, routes_dir, app_dir, css_files,
                            theme_file, client_js, verbose) {
  path <- req$PATH_INFO
  method <- req$REQUEST_METHOD

  if (verbose) cat(sprintf("[%s] %s %s\n", Sys.time(), method, path))

  # Serve static files from public/
  public_dir <- file.path(app_dir, "public")
  if (dir.exists(public_dir)) {
    static_file <- file.path(public_dir, sub("^/", "", path))
    if (file.exists(static_file) && !dir.exists(static_file)) {
      return(serve_static(static_file))
    }
  }

  # Serve component CSS at /_rune/components.css
  if (path == "/_rune/components.css") {
    css_content <- ""
    if (file.exists(theme_file)) {
      css_content <- paste0(css_content, readLines(theme_file, warn = FALSE) |>
                              paste(collapse = "\n"), "\n")
    }
    for (f in css_files) {
      css_content <- paste0(css_content, readLines(f, warn = FALSE) |>
                              paste(collapse = "\n"), "\n")
    }
    return(list(
      status = 200L,
      headers = list("Content-Type" = "text/css; charset=utf-8"),
      body = css_content
    ))
  }

  # Serve client JS at /_rune/client.js
  if (path == "/_rune/client.js") {
    return(list(
      status = 200L,
      headers = list("Content-Type" = "application/javascript; charset=utf-8"),
      body = client_js
    ))
  }

  # Match route
  result <- match_route(path, routes)

  if (is.null(result)) {
    return(list(
      status = 404L,
      headers = list("Content-Type" = "text/html; charset=utf-8"),
      body = render_error_page(404L, "Page not found")
    ))
  }

  route <- result$route
  params <- result$params

  # API route: source file, call handle() function
  if (route$type == "api") {
    return(handle_api_route(route, params, req))
  }

  # Page route: source file, call page() function, wrap in layouts
  return(handle_page_route(route, params, req, client_js))
}

#' Handle an API route.
handle_api_route <- function(route, params, req) {
  env <- new.env(parent = globalenv())
  source(route$file, local = env)

  if (!exists("handle", envir = env)) {
    return(list(
      status = 500L,
      headers = list("Content-Type" = "application/json"),
      body = jsonlite::toJSON(list(error = "API route must export handle()"),
                               auto_unbox = TRUE)
    ))
  }

  result <- env$handle(list(
    params = params,
    method = req$REQUEST_METHOD,
    headers = req$HEADERS,
    query = parse_query_string(req$QUERY_STRING %||% ""),
    body = get_request_body(req)
  ))

  # If result is already a response list, return it
  if (is.list(result) && !is.null(result$status)) {
    return(result)
  }

  # Otherwise, JSON-serialize the result
  list(
    status = 200L,
    headers = list("Content-Type" = "application/json; charset=utf-8"),
    body = jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE)
  )
}

#' Handle a page route.
handle_page_route <- function(route, params, req, client_js) {
  # Source the page file
  env <- new.env(parent = globalenv())
  source(route$file, local = env)

  if (!exists("page", envir = env)) {
    return(list(
      status = 500L,
      headers = list("Content-Type" = "text/html; charset=utf-8"),
      body = render_error_page(500L, "Page must export a page() function")
    ))
  }

  # Create a temporary session for SSR (no WS yet)
  tmp_session <- create_session(paste0("ssr_", sample.int(100000, 1)))
  set_current_session(tmp_session)

  # Render the page
  page_content <- env$page(params, tmp_session)

  # Apply layouts (innermost first, so we reverse and nest)
  content <- page_content
  for (layout_file in rev(route$layouts)) {
    layout_env <- new.env(parent = globalenv())
    source(layout_file, local = layout_env)
    if (exists("layout", envir = layout_env)) {
      content <- layout_env$layout(content, params)
    }
  }

  # Wrap in full HTML document
  page_html <- render_html(content)

  full_html <- paste0(
    '<!DOCTYPE html>\n<html lang="en">\n<head>\n',
    '<meta charset="utf-8" />\n',
    '<meta name="viewport" content="width=device-width, initial-scale=1" />\n',
    '<title>Rune App</title>\n',
    '<link rel="stylesheet" href="/_rune/components.css" />\n',
    '<script src="https://cdn.jsdelivr.net/npm/chart.js@4"></script>\n',
    '</head>\n<body>\n',
    page_html,
    '\n<script src="/_rune/client.js"></script>\n',
    '</body>\n</html>'
  )

  # Clean up temporary SSR session
  remove_session(tmp_session$id)

  list(
    status = 200L,
    headers = list("Content-Type" = "text/html; charset=utf-8"),
    body = full_html
  )
}

#' Set up a session's page after WS init message.
#' Re-sources the page file and re-renders with the real session.
setup_session_page <- function(session, routes, routes_dir, app_dir) {
  path <- session$current_path
  result <- match_route(path, routes)
  if (is.null(result)) return(invisible(NULL))

  route <- result$route
  params <- result$params
  session$route <- route
  session$params <- params

  # Source components
  components_dir <- file.path(app_dir, "components")
  if (dir.exists(components_dir)) {
    comp_files <- list.files(components_dir, pattern = "\\.R$", full.names = TRUE)
    for (f in comp_files) {
      source(f, local = FALSE)
    }
  }

  # Source and render page (this registers handlers and islands)
  env <- new.env(parent = globalenv())
  source(route$file, local = env)

  if (exists("page", envir = env)) {
    set_current_session(session)
    # Render page to register handlers and islands
    page_content <- env$page(params, session)
  }
}

#' Re-render all islands for a session.
rerender_all_islands <- function(session) {
  for (island_id in names(session$islands)) {
    rerender_island(session, island_id)
  }
}

#' Render a simple error page.
render_error_page <- function(status, message) {
  sprintf(
    '<!DOCTYPE html><html><head><title>%d</title>
    <style>body{font-family:system-ui;display:flex;align-items:center;justify-content:center;
    min-height:100vh;margin:0;color:#333}div{text-align:center}h1{font-size:4rem;margin:0;
    opacity:.3}p{margin-top:1rem}</style></head>
    <body><div><h1>%d</h1><p>%s</p></div></body></html>',
    status, status, escape_html(message)
  )
}

#' Parse a query string into a named list.
parse_query_string <- function(qs) {
  if (is.null(qs) || qs == "" || qs == "?") return(list())
  qs <- sub("^\\?", "", qs)
  pairs <- strsplit(qs, "&")[[1]]
  result <- list()
  for (pair in pairs) {
    kv <- strsplit(pair, "=", fixed = TRUE)[[1]]
    if (length(kv) == 2) {
      result[[utils::URLdecode(kv[1])]] <- utils::URLdecode(kv[2])
    }
  }
  result
}

#' Get request body as a string.
get_request_body <- function(req) {
  if (is.null(req$rook.input)) return(NULL)
  tryCatch({
    req$rook.input$read_lines()
  }, error = function(e) NULL)
}

#' Serve a static file.
serve_static <- function(filepath) {
  ext <- tolower(tools::file_ext(filepath))
  content_types <- list(
    html = "text/html",
    css  = "text/css",
    js   = "application/javascript",
    json = "application/json",
    png  = "image/png",
    jpg  = "image/jpeg",
    jpeg = "image/jpeg",
    gif  = "image/gif",
    svg  = "image/svg+xml",
    ico  = "image/x-icon",
    woff = "font/woff",
    woff2 = "font/woff2",
    ttf  = "font/ttf"
  )
  ct <- content_types[[ext]] %||% "application/octet-stream"

  list(
    status = 200L,
    headers = list("Content-Type" = ct),
    body = readBin(filepath, "raw", file.info(filepath)$size)
  )
}

# Null-coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a
