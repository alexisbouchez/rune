# Rune

A modern web app framework for R. File-based routing, server rendering, reactive signals, and a shadcn-like component system. **Not based on Shiny.**

## Quick start

```bash
# Prerequisites: R with httpuv and jsonlite installed
Rscript -e 'install.packages(c("httpuv", "jsonlite"), repos="https://cloud.r-project.org")'

# Create a new app
Rscript packages/rune-cli/rune.R init my-app

# Or run the included demo
cd examples/hello-rune
Rscript ../../packages/rune-cli/rune.R dev
# Open http://localhost:3000
```

## Architecture

```
packages/
  rune-core/    # Server, routing, rendering, reactive core
  rune-cli/     # CLI commands (init, dev, build, add, list)
  rune-ui/      # Component registry + base CSS tokens
examples/
  hello-rune/   # Demo app
tests/          # Unit and integration tests
```

## File-based routing

```
app/
  layout.R          # Root layout (wraps all pages)
  page.R            # /
  about/
    page.R          # /about
  users/
    [id]/
      page.R        # /users/:id (params$id available)
  api/
    hello.R         # /api/hello (returns JSON)
```

## Reactive signals

```r
count <- signal(0L)
doubled <- computed(function() count$get() * 2)

effect(function() {
  cat("Count is now:", count$get(), "\n")
})

count$set(1L)  # triggers effect
```

## Islands (interactive regions)

Pages are server-rendered by default. Wrap interactive parts in `island()`:

```r
page <- function(params, session) {
  count <- signal(0L)

  on_click("inc-btn", function(event, session) {
    count$set(count$get() + 1L)
  })

  div(
    h1("Hello Rune"),
    island("counter",
      p(paste("Count:", count$get())),
      Button(id = "inc-btn", label = "+1")
    )
  )
}
```

When signals change, only the island HTML is re-rendered and patched over WebSocket.

## Components (shadcn-like)

```bash
# List available components
Rscript packages/rune-cli/rune.R list

# Add a component to your app (copies source into ./components/)
Rscript packages/rune-cli/rune.R add Button
Rscript packages/rune-cli/rune.R add Card
```

Components are user-owned. Edit them freely.

## Dependencies

- R (>= 4.0)
- httpuv
- jsonlite

## Running tests

```bash
Rscript tests/run_tests.R
```
