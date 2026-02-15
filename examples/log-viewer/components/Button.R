# Rune UI: Button
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
