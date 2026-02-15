# =============================================================================
# Rune HTML Node Model
# =============================================================================
# A minimal HTML builder. Each tag function returns a "rune_node" list.
# Nodes are rendered to HTML strings via render_html().
# No dependency on htmltools or Shiny.
# =============================================================================

#' Create an HTML node (the core primitive).
#' @param tag Character tag name (e.g. "div").
#' @param ... Children: rune_nodes, character strings, or named attrs.
#' @return A rune_node list.
rune_node <- function(tag, ...) {
  args <- list(...)
  attrs <- list()
  children <- list()

  for (i in seq_along(args)) {
    nm <- names(args)[i]
    val <- args[[i]]
    if (!is.null(nm) && nzchar(nm)) {
      # Named argument -> HTML attribute
      attrs[[nm]] <- val
    } else if (is.null(val)) {
      # skip NULLs
    } else if (is.list(val) && !is.null(val$tag)) {
      # A rune_node child
      children <- c(children, list(val))
    } else if (is.list(val) && is.null(val$tag)) {
      # A list of children (e.g. from lapply)
      for (child in val) {
        if (!is.null(child)) {
          children <- c(children, list(child))
        }
      }
    } else {
      # Text or other scalar
      children <- c(children, list(as.character(val)))
    }
  }

  structure(list(
    tag = tag,
    attrs = attrs,
    children = children
  ), class = "rune_node")
}

#' Render a rune_node tree to an HTML string.
render_html <- function(node) {
  if (is.character(node)) {
    return(escape_html(node))
  }
  if (!inherits(node, "rune_node")) {
    return(escape_html(as.character(node)))
  }

  # Void elements (self-closing)
  void_tags <- c("area", "base", "br", "col", "embed", "hr", "img",
                  "input", "link", "meta", "source", "track", "wbr")

  # Build attribute string
  attr_str <- ""
  if (length(node$attrs) > 0) {
    parts <- vapply(names(node$attrs), function(nm) {
      val <- node$attrs[[nm]]
      if (is.logical(val) && val) {
        return(nm)  # boolean attribute
      } else if (is.logical(val) && !val) {
        return("")  # omit false boolean
      }
      sprintf('%s="%s"', nm, escape_attr(as.character(val)))
    }, character(1))
    parts <- parts[nzchar(parts)]
    if (length(parts) > 0) {
      attr_str <- paste0(" ", paste(parts, collapse = " "))
    }
  }

  if (node$tag %in% void_tags) {
    return(sprintf("<%s%s />", node$tag, attr_str))
  }

  # Render children
  children_html <- paste(vapply(node$children, render_html, character(1)),
                          collapse = "")

  sprintf("<%s%s>%s</%s>", node$tag, attr_str, children_html, node$tag)
}

#' Escape HTML text content.
escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

#' Escape HTML attribute values.
escape_attr <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

#' Insert raw HTML (no escaping). Use with care.
raw_html <- function(html_string) {
  node <- list(tag = "__raw__", attrs = list(), children = list(html_string))
  class(node) <- "rune_node"
  node
}

# Override render_html for raw nodes
render_html_original <- render_html
render_html <- function(node) {
  if (inherits(node, "rune_node") && identical(node$tag, "__raw__")) {
    return(node$children[[1]])
  }
  render_html_original(node)
}

# =============================================================================
# Convenience tag functions
# =============================================================================
# Generate common HTML tags as functions.

html  <- function(...) rune_node("html", ...)
head_ <- function(...) rune_node("head", ...)  # head conflicts with base R
body  <- function(...) rune_node("body", ...)
div   <- function(...) rune_node("div", ...)
span  <- function(...) rune_node("span", ...)
p     <- function(...) rune_node("p", ...)
a     <- function(...) rune_node("a", ...)
h1    <- function(...) rune_node("h1", ...)
h2    <- function(...) rune_node("h2", ...)
h3    <- function(...) rune_node("h3", ...)
h4    <- function(...) rune_node("h4", ...)
ul    <- function(...) rune_node("ul", ...)
ol    <- function(...) rune_node("ol", ...)
li    <- function(...) rune_node("li", ...)
img   <- function(...) rune_node("img", ...)
form  <- function(...) rune_node("form", ...)
input <- function(...) rune_node("input", ...)
label_ <- function(...) rune_node("label", ...)  # label conflicts with base R
button <- function(...) rune_node("button", ...)
textarea <- function(...) rune_node("textarea", ...)
table_ <- function(...) rune_node("table", ...)
thead <- function(...) rune_node("thead", ...)
tbody <- function(...) rune_node("tbody", ...)
tr    <- function(...) rune_node("tr", ...)
th    <- function(...) rune_node("th", ...)
td    <- function(...) rune_node("td", ...)
br    <- function(...) rune_node("br", ...)
hr    <- function(...) rune_node("hr", ...)
link  <- function(...) rune_node("link", ...)
meta  <- function(...) rune_node("meta", ...)
title_ <- function(...) rune_node("title", ...)
style <- function(...) rune_node("style", ...)
script <- function(...) rune_node("script", ...)
section <- function(...) rune_node("section", ...)
nav   <- function(...) rune_node("nav", ...)
footer <- function(...) rune_node("footer", ...)
header_ <- function(...) rune_node("header", ...)
main  <- function(...) rune_node("main", ...)
select_ <- function(...) rune_node("select", ...)
option <- function(...) rune_node("option", ...)
