# Root layout - wraps all pages in the hello-rune demo
layout <- function(content, params) {
  div(class = "rune-app",
    nav(class = "rune-nav",
      a(href = "/", "Home"),
      span(" | "),
      a(href = "/about", "About"),
      span(" | "),
      a(href = "/users/42", "User 42"),
      span(" | "),
      a(href = "/api/hello", "API")
    ),
    main(class = "rune-main", content),
    footer(class = "rune-footer",
      p("Built with Rune - a modern web framework for R")
    )
  )
}
