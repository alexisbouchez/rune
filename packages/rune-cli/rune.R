#!/usr/bin/env Rscript
# =============================================================================
# Rune CLI
# =============================================================================
# Command-line interface for Rune framework.
#
# Usage:
#   Rscript rune.R init <appname>    Create a new Rune app
#   Rscript rune.R dev               Start dev server (from app dir)
#   Rscript rune.R build             Build for production
#   Rscript rune.R add <component>   Copy a component into ./components/
#   Rscript rune.R list              List available components
#   Rscript rune.R help              Show help
# =============================================================================

# --- Resolve paths ---
# Find the monorepo root relative to this CLI script
cli_script <- if (length(sys.frames()) > 0) {
  # When sourced
  tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NULL)
} else {
  NULL
}

# When run via Rscript, get the script path from command args
if (is.null(cli_script)) {
  args_all <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args_all, value = TRUE)
  if (length(file_arg) > 0) {
    cli_script <- normalizePath(sub("^--file=", "", file_arg[1]))
  }
}

if (is.null(cli_script)) {
  cli_dir <- getwd()
} else {
  cli_dir <- dirname(cli_script)
}

# Monorepo root is two levels up from packages/rune-cli/
monorepo_root <- normalizePath(file.path(cli_dir, "..", ".."), mustWork = FALSE)
core_dir <- file.path(monorepo_root, "packages", "rune-core")
ui_dir <- file.path(monorepo_root, "packages", "rune-ui")

# --- Parse CLI args ---
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  args <- c("help")
}

command <- args[1]
rest_args <- args[-1]

# --- Commands ---

cmd_help <- function() {
  cat("
Rune - A modern web framework for R

Usage:
  Rscript rune.R <command> [args]

Commands:
  init <name>       Create a new Rune app
  dev               Start development server
  build             Build for production
  add <component>   Add a component to your app
  list              List available components
  help              Show this help message

Examples:
  Rscript rune.R init my-app
  cd my-app && Rscript ../packages/rune-cli/rune.R dev
  Rscript rune.R add Button

")
}

cmd_init <- function(app_name) {
  if (missing(app_name) || is.null(app_name) || !nzchar(app_name)) {
    cat("Error: app name required.\nUsage: rune init <appname>\n")
    quit(status = 1)
  }

  if (dir.exists(app_name)) {
    cat("Error: directory", app_name, "already exists.\n")
    quit(status = 1)
  }

  cat("Creating Rune app:", app_name, "\n")

  # Create directory structure
  dirs <- c(
    file.path(app_name, "app"),
    file.path(app_name, "app", "about"),
    file.path(app_name, "app", "api"),
    file.path(app_name, "components"),
    file.path(app_name, "public")
  )
  for (d in dirs) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  # Create root layout
  writeLines(c(
    '# Root layout - wraps all pages',
    'layout <- function(content, params) {',
    '  div(class = "rune-app",',
    '    nav(class = "rune-nav",',
    '      a(href = "/", "Home"),',
    '      span(" | "),',
    '      a(href = "/about", "About")',
    '    ),',
    '    main(class = "rune-main", content),',
    '    footer(class = "rune-footer", p("Built with Rune"))',
    '  )',
    '}'
  ), file.path(app_name, "app", "layout.R"))

  # Create home page
  writeLines(c(
    '# Home page',
    'page <- function(params, session) {',
    '  div(',
    '    h1("Welcome to Rune"),',
    '    p("Edit app/page.R to get started.")',
    '  )',
    '}'
  ), file.path(app_name, "app", "page.R"))

  # Create about page
  writeLines(c(
    '# About page',
    'page <- function(params, session) {',
    '  div(',
    '    h1("About"),',
    '    p("This app was built with the Rune framework.")',
    '  )',
    '}'
  ), file.path(app_name, "app", "about", "page.R"))

  # Create API route
  writeLines(c(
    '# API endpoint',
    'handle <- function(req) {',
    '  list(',
    '    message = "Hello from Rune API!",',
    '    timestamp = as.character(Sys.time())',
    '  )',
    '}'
  ), file.path(app_name, "app", "api", "hello.R"))

  # Create theme CSS
  writeLines(c(
    ':root {',
    '  --rune-bg: #ffffff;',
    '  --rune-fg: #111111;',
    '  --rune-primary: #2563eb;',
    '  --rune-primary-fg: #ffffff;',
    '  --rune-muted: #f3f4f6;',
    '  --rune-muted-fg: #6b7280;',
    '  --rune-border: #e5e7eb;',
    '  --rune-radius: 0.5rem;',
    '  --rune-font: system-ui, -apple-system, sans-serif;',
    '}',
    '',
    '* { box-sizing: border-box; margin: 0; padding: 0; }',
    '',
    'body {',
    '  font-family: var(--rune-font);',
    '  background: var(--rune-bg);',
    '  color: var(--rune-fg);',
    '  line-height: 1.6;',
    '}',
    '',
    '.rune-app { max-width: 48rem; margin: 0 auto; padding: 1rem; }',
    '.rune-nav { padding: 1rem 0; border-bottom: 1px solid var(--rune-border); margin-bottom: 2rem; }',
    '.rune-nav a { color: var(--rune-primary); text-decoration: none; }',
    '.rune-nav a:hover { text-decoration: underline; }',
    '.rune-main { min-height: 60vh; }',
    '.rune-footer { margin-top: 3rem; padding: 1rem 0; border-top: 1px solid var(--rune-border); color: var(--rune-muted-fg); font-size: 0.875rem; }'
  ), file.path(app_name, "public", "theme.css"))

  cat("Done! Created:\n")
  cat("  ", app_name, "/app/layout.R\n")
  cat("  ", app_name, "/app/page.R\n")
  cat("  ", app_name, "/app/about/page.R\n")
  cat("  ", app_name, "/app/api/hello.R\n")
  cat("  ", app_name, "/public/theme.css\n")
  cat("\nNext steps:\n")
  cat("  cd", app_name, "\n")
  cat("  Rscript", file.path("..", "packages", "rune-cli", "rune.R"), "dev\n")
}

cmd_dev <- function() {
  app_dir <- getwd()

  # Check that we're in a Rune app (has app/ directory)
  if (!dir.exists(file.path(app_dir, "app"))) {
    cat("Error: no app/ directory found. Are you in a Rune app directory?\n")
    quit(status = 1)
  }

  # Load rune-core
  cat("Loading Rune core...\n")
  Sys.setenv(RUNE_CORE_DIR = core_dir)
  source(file.path(core_dir, "R", "rune.R"), local = FALSE)
  rune_load(core_dir)

  # Parse port from args
  port <- 3000L
  for (a in rest_args) {
    if (grepl("^--port=", a)) {
      port <- as.integer(sub("^--port=", "", a))
    }
    if (grepl("^-p$", a)) {
      idx <- which(rest_args == a) + 1
      if (idx <= length(rest_args)) {
        port <- as.integer(rest_args[idx])
      }
    }
  }

  # Start server
  rune_serve(app_dir = app_dir, port = port)
}

cmd_build <- function() {
  cat("Build command: preparing production artifacts...\n")
  app_dir <- getwd()

  if (!dir.exists(file.path(app_dir, "app"))) {
    cat("Error: no app/ directory found.\n")
    quit(status = 1)
  }

  build_dir <- file.path(app_dir, "build")
  dir.create(build_dir, recursive = TRUE, showWarnings = FALSE)

  # Copy app files
  file.copy(file.path(app_dir, "app"), build_dir, recursive = TRUE)
  if (dir.exists(file.path(app_dir, "components"))) {
    file.copy(file.path(app_dir, "components"), build_dir, recursive = TRUE)
  }
  if (dir.exists(file.path(app_dir, "public"))) {
    file.copy(file.path(app_dir, "public"), build_dir, recursive = TRUE)
  }

  # Create a start script
  writeLines(c(
    '#!/usr/bin/env Rscript',
    '# Production server start script',
    sprintf('Sys.setenv(RUNE_CORE_DIR = "%s")', core_dir),
    sprintf('source("%s")', file.path(core_dir, "R", "rune.R")),
    sprintf('rune_load("%s")', core_dir),
    'rune_serve(app_dir = ".", host = "0.0.0.0", port = as.integer(Sys.getenv("PORT", "3000")))'
  ), file.path(build_dir, "start.R"))

  cat("Build complete. Output in:", build_dir, "\n")
  cat("Run with: cd build && Rscript start.R\n")
}

cmd_add <- function(component_name) {
  if (missing(component_name) || is.null(component_name) || !nzchar(component_name)) {
    cat("Error: component name required.\nUsage: rune add <component>\n")
    cat("Run 'rune list' to see available components.\n")
    quit(status = 1)
  }

  # Read registry
  registry_file <- file.path(ui_dir, "registry.json")
  if (!file.exists(registry_file)) {
    cat("Error: component registry not found at:", registry_file, "\n")
    quit(status = 1)
  }

  registry <- jsonlite::fromJSON(registry_file, simplifyVector = FALSE)

  # Find the component
  comp <- NULL
  for (c in registry$components) {
    if (tolower(c$name) == tolower(component_name)) {
      comp <- c
      break
    }
  }

  if (is.null(comp)) {
    cat("Error: component '", component_name, "' not found.\n", sep = "")
    cat("Run 'rune list' to see available components.\n")
    quit(status = 1)
  }

  # Ensure components/ dir exists
  dest_dir <- file.path(getwd(), "components")
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  # Copy R file
  r_source <- file.path(ui_dir, "registry", comp$file)
  r_dest <- file.path(dest_dir, paste0(comp$name, ".R"))
  if (file.exists(r_source)) {
    file.copy(r_source, r_dest, overwrite = TRUE)
    cat("Added:", r_dest, "\n")
  } else {
    cat("Warning: R source file not found:", r_source, "\n")
  }

  # Copy CSS file if exists
  if (!is.null(comp$css) && nzchar(comp$css)) {
    css_source <- file.path(ui_dir, "registry", comp$css)
    css_dest <- file.path(dest_dir, paste0(comp$name, ".css"))
    if (file.exists(css_source)) {
      file.copy(css_source, css_dest, overwrite = TRUE)
      cat("Added:", css_dest, "\n")
    }
  }

  # Copy JS file if exists
  if (!is.null(comp$js) && nzchar(comp$js)) {
    js_source <- file.path(ui_dir, "registry", comp$js)
    js_dest <- file.path(dest_dir, paste0(comp$name, ".js"))
    if (file.exists(js_source)) {
      file.copy(js_source, js_dest, overwrite = TRUE)
      cat("Added:", js_dest, "\n")
    }
  }

  # Check and add dependencies
  if (!is.null(comp$dependencies)) {
    for (dep in comp$dependencies) {
      dep_dest <- file.path(dest_dir, paste0(dep, ".R"))
      if (!file.exists(dep_dest)) {
        cat("Note: component '", comp$name, "' depends on '", dep,
            "'. Run: rune add ", dep, "\n", sep = "")
      }
    }
  }

  cat("\nComponent '", comp$name, "' added to ./components/\n", sep = "")
}

cmd_list <- function() {
  registry_file <- file.path(ui_dir, "registry.json")
  if (!file.exists(registry_file)) {
    cat("Error: component registry not found.\n")
    quit(status = 1)
  }

  registry <- jsonlite::fromJSON(registry_file, simplifyVector = FALSE)

  cat("\nAvailable Rune UI Components:\n")
  cat(strrep("-", 50), "\n")
  for (comp in registry$components) {
    cat(sprintf("  %-12s  %s\n", comp$name, comp$description %||% ""))
  }
  cat("\nAdd with: rune add <name>\n\n")
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# --- Dispatch ---
switch(command,
  "init"  = cmd_init(rest_args[1]),
  "dev"   = cmd_dev(),
  "build" = cmd_build(),
  "add"   = cmd_add(rest_args[1]),
  "list"  = cmd_list(),
  "help"  = cmd_help(),
  {
    cat("Unknown command:", command, "\n")
    cmd_help()
    quit(status = 1)
  }
)
