# Rune UI: Input
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
