# =============================================================================
# Home page - demonstrates reactive counter and live input echo
# =============================================================================

page <- function(params, session) {
  # --- Reactive state ---
  count <- signal(0L)
  text <- signal("")

  # Derived value
  doubled <- computed(function() count$get() * 2L)

  # --- Event handlers ---
  on_click("inc-btn", function(event, session) {
    count$set(count$get() + 1L)
  })

  on_click("dec-btn", function(event, session) {
    count$set(count$get() - 1L)
  })

  on_click("reset-btn", function(event, session) {
    count$set(0L)
  })

  on_input("echo-input", function(event, session) {
    text$set(event$value)
  })

  # --- Register island renderers for live updates ---
  register_island("counter", function() {
    list(
      Card(
        CardHeader(CardTitle("Counter")),
        CardBody(
          p(class = "counter-display", paste("Count:", count$get())),
          p(class = "counter-derived", paste("Doubled:", doubled$get())),
          div(class = "counter-actions",
            Button(id = "dec-btn", label = "-1", variant = "outline"),
            Button(id = "reset-btn", label = "Reset", variant = "ghost"),
            Button(id = "inc-btn", label = "+1")
          )
        )
      )
    )
  })

  register_island("echo", function() {
    list(
      Card(
        CardHeader(CardTitle("Live Echo")),
        CardBody(
          Input(id = "echo-input", label_text = "Type something:",
                placeholder = "Hello, Rune!"),
          div(class = "echo-output",
            p(if (nzchar(text$get())) {
                paste("You typed:", text$get())
              } else {
                "Start typing to see live updates..."
              }
            )
          )
        )
      )
    )
  })

  # --- Page content ---
  div(
    h1("Hello, Rune!"),
    p(class = "subtitle",
      "A modern web framework for R. No Shiny required."),

    div(class = "demo-grid",
      # Counter island - interactive, updates via WebSocket
      island("counter",
        Card(
          CardHeader(CardTitle("Counter")),
          CardBody(
            p(class = "counter-display", paste("Count:", count$get())),
            p(class = "counter-derived", paste("Doubled:", doubled$get())),
            div(class = "counter-actions",
              Button(id = "dec-btn", label = "-1", variant = "outline"),
              Button(id = "reset-btn", label = "Reset", variant = "ghost"),
              Button(id = "inc-btn", label = "+1")
            )
          )
        )
      ),

      # Echo island - live input mirroring
      island("echo",
        Card(
          CardHeader(CardTitle("Live Echo")),
          CardBody(
            Input(id = "echo-input", label_text = "Type something:",
                  placeholder = "Hello, Rune!"),
            div(class = "echo-output",
              p("Start typing to see live updates...")
            )
          )
        )
      )
    )
  )
}
