# =============================================================================
# Rune UI: Input
# =============================================================================
# A styled text input with optional label.
# =============================================================================

#' Create an Input component.
#' @param id Element ID (required for on_input handling).
#' @param label_ Optional label text.
#' @param placeholder Placeholder text.
#' @param type Input type (text, email, password, number, etc.).
#' @param value Initial value.
#' @param name Form field name.
#' @param class Additional CSS classes.
Input <- function(id = NULL, label_text = NULL, placeholder = "",
                  type = "text", value = "", name = NULL, class = "") {
  children <- list()

  if (!is.null(label_text)) {
    children <- c(children, list(
      rune_node("label", class = "rune-input-label", `for` = id %||% "", label_text)
    ))
  }

  input_attrs <- list(
    class = paste("rune-input", class),
    type = type,
    placeholder = placeholder,
    value = value
  )
  if (!is.null(id)) input_attrs$id <- id
  if (!is.null(name)) input_attrs$name <- name

  children <- c(children, list(do.call(input, input_attrs)))

  div(class = "rune-input-group", children)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
