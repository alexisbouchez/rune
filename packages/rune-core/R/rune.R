# =============================================================================
# Rune Core - Main entry point
# =============================================================================
# Sources all core modules. This file is the single entry point for loading
# the entire Rune framework.
# =============================================================================

#' Load all Rune core modules.
#' @param core_dir Path to the rune-core package directory.
rune_load <- function(core_dir = NULL) {
  if (is.null(core_dir)) {
    # Auto-detect: look relative to this file
    core_dir <- Sys.getenv("RUNE_CORE_DIR", "")
    if (!nzchar(core_dir)) {
      stop("RUNE_CORE_DIR environment variable not set. ",
           "Pass core_dir explicitly or set the env var.")
    }
  }

  r_dir <- file.path(core_dir, "R")

  # Source order matters: html first (no deps), then router, reactive, session, server
  source(file.path(r_dir, "html.R"), local = FALSE)
  source(file.path(r_dir, "router.R"), local = FALSE)
  source(file.path(r_dir, "reactive.R"), local = FALSE)
  source(file.path(r_dir, "client_js.R"), local = FALSE)
  source(file.path(r_dir, "session.R"), local = FALSE)
  source(file.path(r_dir, "server.R"), local = FALSE)

  # Set the client JS path
  js_path <- file.path(core_dir, "inst", "js", "client.js")
  if (file.exists(js_path)) {
    Sys.setenv(RUNE_CLIENT_JS = js_path)
  }

  invisible(TRUE)
}
