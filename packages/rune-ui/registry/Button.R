# =============================================================================
# Rune UI: Button
# =============================================================================
# A styled button component with variant support.
# Copy this into your app with: rune add Button
# Then use: Button(id = "my-btn", label = "Click me")
# =============================================================================

#' Create a Button component.
#' @param id Element ID (required for event handling).
#' @param label Button text.
#' @param variant One of "default", "primary", "outline", "ghost", "destructive".
#' @param size One of "default", "sm", "lg".
#' @param disabled Logical.
#' @param ... Additional HTML attributes.
Button <- function(id = NULL, label = "Button", variant = "default",
                   size = "default", disabled = FALSE, ...) {
  classes <- paste("rune-btn",
                   paste0("rune-btn--", variant),
                   paste0("rune-btn--", size))

  attrs <- list(class = classes, ...)
  if (!is.null(id)) attrs$id <- id
  if (disabled) attrs$disabled <- TRUE

  do.call(button, c(attrs, list(label)))
}
