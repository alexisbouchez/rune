# About page - static content (no islands needed)
page <- function(params, session) {
  div(
    h1("About Rune"),
    Card(
      CardBody(
        h2("What is Rune?"),
        p("Rune is a modern web framework for R that provides:"),
        ul(
          li("File-based routing (like Next.js)"),
          li("Server-side rendering with a composable HTML builder"),
          li("Reactive signals for state management"),
          li("WebSocket-powered live updates (islands architecture)"),
          li("A shadcn-inspired component system (user-owned code)")
        ),
        hr(),
        h2("Architecture"),
        p("Rune uses an islands architecture: pages are server-rendered by default, ",
          "and interactive regions (islands) are updated over WebSocket when reactive ",
          "signals change. No full page reloads needed."),
        hr(),
        p("Built with base R, httpuv, and jsonlite. No Shiny dependency.")
      )
    )
  )
}
