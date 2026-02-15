#!/usr/bin/env Rscript
# =============================================================================
# Rune Test Runner
# =============================================================================
# Runs all test files and reports results.
# Usage: Rscript tests/run_tests.R  (from the monorepo root)
# =============================================================================

cat("======================================\n")
cat("       Rune Framework Tests           \n")
cat("======================================\n\n")

# Determine monorepo root
# This script should be run from the monorepo root: Rscript tests/run_tests.R
# Or from anywhere if RUNE_ROOT is set.
if (nzchar(Sys.getenv("RUNE_ROOT", ""))) {
  rune_root <- Sys.getenv("RUNE_ROOT")
} else {
  # Assume we're run from the monorepo root
  if (dir.exists(file.path(getwd(), "packages", "rune-core"))) {
    rune_root <- getwd()
  } else if (dir.exists(file.path(getwd(), "..", "packages", "rune-core"))) {
    rune_root <- normalizePath(file.path(getwd(), ".."))
  } else {
    stop("Cannot find monorepo root. Run from the repo root or set RUNE_ROOT.")
  }
}

Sys.setenv(RUNE_ROOT = rune_root)
cat("RUNE_ROOT:", rune_root, "\n\n")

test_dir <- file.path(rune_root, "tests")

test_files <- c(
  "test_html.R",
  "test_router.R",
  "test_reactive.R",
  "test_integration.R"
)

all_pass <- TRUE

for (tf in test_files) {
  test_path <- file.path(test_dir, tf)
  if (!file.exists(test_path)) {
    cat("SKIP: not found:", test_path, "\n")
    next
  }

  result <- tryCatch({
    source(test_path, local = TRUE)
    TRUE
  }, error = function(e) {
    cat("ERROR in", tf, ":", e$message, "\n")
    FALSE
  })

  if (!isTRUE(result)) {
    all_pass <- FALSE
  }
}

cat("======================================\n")
if (all_pass) {
  cat("       ALL TESTS PASSED               \n")
} else {
  cat("       SOME TESTS FAILED              \n")
}
cat("======================================\n")

if (!all_pass) quit(status = 1)
