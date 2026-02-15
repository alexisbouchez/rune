# =============================================================================
# Rune UI: Dialog
# =============================================================================
# A modal dialog overlay. Rendered as an island for interactivity.
# Toggle visibility via a signal.
# =============================================================================

#' Create a Dialog component.
#' @param id Dialog ID.
#' @param open Logical, whether the dialog is currently open.
#' @param title Dialog title.
#' @param ... Dialog body content.
Dialog <- function(id = "dialog", open = FALSE, title = "Dialog", ...) {
  if (!open) {
    return(div(id = id, class = "rune-dialog-hidden"))
  }

  div(id = id, class = "rune-dialog-overlay",
    div(class = "rune-dialog",
      div(class = "rune-dialog-header",
        h3(class = "rune-dialog-title", title),
        button(id = paste0(id, "-close"), class = "rune-dialog-close",
               raw_html("&times;"))
      ),
      div(class = "rune-dialog-body", ...)
    )
  )
}
