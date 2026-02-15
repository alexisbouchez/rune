# =============================================================================
# Test Helpers
# =============================================================================
# Shared utilities for Rune tests.
# =============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Find the monorepo root directory.
#' Works whether run via `Rscript tests/run_tests.R` or `source("test_foo.R")`.
find_rune_root <- function() {
  # Try RUNE_ROOT env var first (can be set by runner)
  root <- Sys.getenv("RUNE_ROOT", "")
  if (nzchar(root) && dir.exists(root)) return(normalizePath(root))

  # Try walking up from the current working directory
  candidates <- c(
    getwd(),
    file.path(getwd(), ".."),
    file.path(getwd(), "..", "..")
  )

  for (d in candidates) {
    if (dir.exists(file.path(d, "packages", "rune-core"))) {
      return(normalizePath(d))
    }
  }

  stop("Cannot find Rune monorepo root. Run from the repo root or set RUNE_ROOT.")
}

rune_root <- find_rune_root()
core_dir <- file.path(rune_root, "packages", "rune-core")

pass <- 0L
fail <- 0L

assert <- function(desc, condition) {
  if (isTRUE(condition)) {
    cat("  PASS:", desc, "\n")
    pass <<- pass + 1L
  } else {
    cat("  FAIL:", desc, "\n")
    fail <<- fail + 1L
  }
}

report <- function(suite_name) {
  cat(sprintf("\n%s: %d passed, %d failed\n\n", suite_name, pass, fail))
  if (fail > 0) quit(status = 1)
}
