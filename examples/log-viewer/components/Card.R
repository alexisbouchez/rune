# Rune UI: Card
Card <- function(..., class = "") {
  div(class = paste("rune-card", class), ...)
}
CardHeader <- function(..., class = "") {
  div(class = paste("rune-card-header", class), ...)
}
CardBody <- function(..., class = "") {
  div(class = paste("rune-card-body", class), ...)
}
CardFooter <- function(..., class = "") {
  div(class = paste("rune-card-footer", class), ...)
}
CardTitle <- function(text, class = "") {
  h3(class = paste("rune-card-title", class), text)
}
CardDescription <- function(text, class = "") {
  p(class = paste("rune-card-desc", class), text)
}
