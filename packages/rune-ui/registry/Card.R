# =============================================================================
# Rune UI: Card
# =============================================================================
# A container card with optional header, body, and footer sections.
# =============================================================================

#' Create a Card component.
#' @param ... Card body content.
#' @param class Additional CSS classes.
Card <- function(..., class = "") {
  div(class = paste("rune-card", class), ...)
}

#' Card header section.
CardHeader <- function(..., class = "") {
  div(class = paste("rune-card-header", class), ...)
}

#' Card body/content section.
CardBody <- function(..., class = "") {
  div(class = paste("rune-card-body", class), ...)
}

#' Card footer section.
CardFooter <- function(..., class = "") {
  div(class = paste("rune-card-footer", class), ...)
}

#' Card title.
CardTitle <- function(text, class = "") {
  h3(class = paste("rune-card-title", class), text)
}

#' Card description.
CardDescription <- function(text, class = "") {
  p(class = paste("rune-card-desc", class), text)
}
