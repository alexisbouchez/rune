# Rune

A web framework for R. Server-rendered pages, reactive signals, WebSocket-driven updates, and copy-paste components. **Not based on Shiny.**

```
app/
  layout.R    # wraps all pages
  page.R      # route: /
  blog/
    [slug]/
      page.R  # route: /blog/:slug
```

```r
page <- function(params, session) {
  count <- signal(0L)

  on_click("btn", function(e, s) {
    count$set(count$get() + 1L)
  })

  register_island("counter", function() {
    p(paste("Clicked", count$get(), "times"))
  })

  div(
    h1("Hello"),
    island("counter", p("Clicked 0 times")),
    Button(id = "btn", label = "Click me")
  )
}
```

That's a complete interactive page. The island re-renders server-side on each signal change and patches the DOM over WebSocket.

## Getting started

```bash
# Install dependencies
Rscript -e 'install.packages(c("httpuv", "jsonlite"), repos="https://cloud.r-project.org")'

# Create a new app
Rscript packages/rune-cli/rune.R init my-app
cd my-app

# Start the dev server
Rscript ../packages/rune-cli/rune.R dev
# http://localhost:3000
```

## How it works

1. **HTTP request** comes in. Rune matches the URL to a `page.R` file, calls `page(params, session)`, renders the returned node tree to HTML, and sends the response.

2. **Browser connects** via WebSocket and sends `init`. The server re-sources the page to set up signals, event handlers, and island render functions for that session.

3. **User interacts** (clicks a button, types in an input). The client sends the event over WebSocket. The server dispatches it to the registered handler, which updates signals. All islands re-render and only changed HTML is patched back.

No client-side JavaScript framework. No virtual DOM diffing. The server owns all state.

## Reactive primitives

Three primitives, same model as SolidJS/Preact Signals:

```r
count <- signal(0L)                              # mutable state
doubled <- computed(function() count$get() * 2L)  # derived value
effect(function() cat(count$get(), "\n"))          # side effect

count$set(5L)
# effect prints: 5
# doubled$get() returns: 10
```

Use `batch()` to defer effects until multiple signals are updated:

```r
batch(function() {
  x$set(1)
  y$set(2)
  # effects only run once, after both updates
})
```

## Event handlers

```r
on_click("my-button", function(event, session) {
  count$set(count$get() + 1L)
})

on_input("my-input", function(event, session) {
  query$set(event$value)  # event$value has the current input value
})

on_submit("my-form", function(event, session) {
  name <- event$data$name  # named form fields
})
```

Any element with an `id` attribute can be a click target. Inputs, textareas, and selects fire `on_input`. Forms fire `on_submit`.

## Islands

Pages are static HTML by default. `island()` marks a region as interactive:

```r
island("todo-list",
  ul(lapply(todos$get(), function(t) li(t)))
)
```

`register_island()` tells the server how to re-render it when signals change:

```r
register_island("todo-list", function() {
  ul(lapply(todos$get(), function(t) li(t)))
})
```

After an event, the server re-runs every registered island function, diffs the HTML, and sends patches for islands that changed. Focus and input selection are preserved across patches.

## File-based routing

```
app/
  layout.R              # root layout (wraps all pages)
  page.R                # /
  about/
    page.R              # /about
  users/
    [id]/
      page.R            # /users/:id
  api/
    data.R              # /api/data (JSON endpoint)
```

Dynamic segments use `[param]` directories. The value is available as `params$id`.

Layouts wrap page content:

```r
# app/layout.R
layout <- function(content, params) {
  div(class = "app",
    nav(a(href = "/", "Home"), a(href = "/about", "About")),
    main(content)
  )
}
```

## Components

Rune ships a component registry inspired by shadcn/ui. Components are copied into your app -- you own the source.

```bash
Rscript packages/rune-cli/rune.R add Button    # copies Button.R + Button.css
Rscript packages/rune-cli/rune.R add Card
Rscript packages/rune-cli/rune.R list          # show all available components
```

Available components: **Button**, **Card**, **Input**, **Table**, **Dialog**, **Toast**.

```r
Button(id = "save", label = "Save", variant = "primary", size = "sm")

Card(
  CardHeader(CardTitle("Users")),
  CardBody(Table(my_dataframe))
)

Input(id = "email", label_text = "Email", type = "email", placeholder = "you@example.com")
```

## Charts

Rune includes Chart.js support. Place a `<canvas>` element and send chart configs from the server:

```r
# In your page
div(class = "chart-container",
  rune_node("canvas", id = "my-chart")
)

# In a register_island callback or effect
send_chart(session, "my-chart", list(
  type = "bar",
  data = list(
    labels = c("A", "B", "C"),
    datasets = list(list(
      label = "Values",
      data = c(10, 20, 30)
    ))
  )
))
```

Chart configs are standard [Chart.js](https://www.chartjs.org/) JSON. Build them as R lists.

## Dark mode

Toggle CSS classes on `<body>` from the server:

```r
send_body_class(session, "dark", add = TRUE)   # adds class
send_body_class(session, "dark", add = FALSE)  # removes class
```

## HTML builder

All standard HTML tags are available as R functions:

```r
div(class = "card",
  h2("Title"),
  p("Some text with ", a(href = "/link", "a link"), "."),
  ul(
    li("Item 1"),
    li("Item 2")
  ),
  input(id = "name", type = "text", placeholder = "Your name"),
  button(id = "go", "Submit")
)
```

Named arguments become HTML attributes. Unnamed arguments become children. `TRUE` boolean attributes render as bare attributes (e.g. `checked`, `disabled`). `NULL` children are skipped. Lists are flattened, so `lapply()` works naturally.

Use `raw_html("<b>bold</b>")` to insert unescaped HTML.

## Project structure

```
packages/
  rune-core/       # server, router, HTML builder, reactive system, client JS
  rune-cli/        # CLI (init, dev, build, add, list)
  rune-ui/         # component registry and base CSS tokens
examples/
  hello-rune/      # counter + live echo demo
  log-viewer/      # nginx log analyzer (charts, tables, tabs, dark mode, geo)
tests/
```

## CLI reference

| Command | Description |
|---------|-------------|
| `init <name>` | Scaffold a new app |
| `dev` | Start dev server (default port 3000, `--port` to change) |
| `build` | Bundle for production |
| `add <component>` | Copy a component into `./components/` |
| `list` | List available components |

## Dependencies

- R >= 4.0
- [httpuv](https://github.com/rstudio/httpuv)
- [jsonlite](https://github.com/jeroen/jsonlite)

## Running tests

```bash
Rscript tests/run_tests.R
```
