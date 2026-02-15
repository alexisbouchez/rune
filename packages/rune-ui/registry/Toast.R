# =============================================================================
# Rune UI: Toast
# =============================================================================
# A toast notification component. Render in an island and toggle via signal.
# =============================================================================

#' Create a Toast component.
#' @param id Toast ID.
#' @param message Toast message text.
#' @param variant One of "default", "success", "error", "warning".
#' @param visible Logical, whether the toast is currently visible.
Toast <- function(id = "toast", message = "", variant = "default",
                  visible = FALSE) {
  if (!visible) {
    return(div(id = id, class = "rune-toast-hidden"))
  }

  div(id = id, class = paste("rune-toast", paste0("rune-toast--", variant)),
    span(class = "rune-toast-message", message),
    button(id = paste0(id, "-dismiss"), class = "rune-toast-dismiss",
           raw_html("&times;"))
  )
}
