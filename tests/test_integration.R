# =============================================================================
# Tests: Integration (Smoke Test)
# =============================================================================

cat("=== Integration Tests ===\n")

source(file.path(Sys.getenv("RUNE_ROOT", ".."), "tests", "test_helpers.R"), local = FALSE)

# Check dependencies
if (!requireNamespace("httpuv", quietly = TRUE)) {
  cat("SKIP: httpuv not installed\n")
  quit(status = 0)
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  cat("SKIP: jsonlite not installed\n")
  quit(status = 0)
}

# Load rune-core
source(file.path(core_dir, "R", "rune.R"), local = FALSE)
rune_load(core_dir)

# Source demo app components
demo_dir <- file.path(rune_root, "examples", "hello-rune")
comp_dir <- file.path(demo_dir, "components")
if (dir.exists(comp_dir)) {
  for (f in list.files(comp_dir, pattern = "\\.R$", full.names = TRUE)) {
    source(f, local = FALSE)
  }
}

# Build route table
routes_dir <- file.path(demo_dir, "app")
routes <- build_route_table(routes_dir)

cat("\nRoute table built:\n")
for (r in routes) {
  cat(sprintf("  %s %s\n", r$type, r$pattern))
}

# --- Test route matching ---
cat("\nRoute matching:\n")

m1 <- match_route("/", routes)
assert("/ matches a route", !is.null(m1))
assert("/ is a page", m1$route$type == "page")

m2 <- match_route("/about", routes)
assert("/about matches", !is.null(m2))

m3 <- match_route("/api/hello", routes)
assert("/api/hello matches", !is.null(m3))
assert("/api/hello is api type", m3$route$type == "api")

m4 <- match_route("/users/42", routes)
assert("/users/42 matches", !is.null(m4))
assert("/users/42 param id = 42", identical(m4$params$id, "42"))

# --- Test page rendering ---
cat("\nPage rendering:\n")

page_env <- new.env(parent = globalenv())
source(m1$route$file, local = page_env)
tmp_session <- create_session("test_ssr_1")
set_current_session(tmp_session)
page_result <- page_env$page(list(), tmp_session)
assert("home page returns rune_node", inherits(page_result, "rune_node"))

html_out <- render_html(page_result)
assert("home page HTML contains Hello", grepl("Hello", html_out))
assert("home page HTML contains counter island",
       grepl("data-rune-island", html_out))

remove_session(tmp_session$id)

# --- Test API route ---
cat("\nAPI route:\n")

api_env <- new.env(parent = globalenv())
source(m3$route$file, local = api_env)
api_result <- api_env$handle(list(
  params = list(),
  method = "GET",
  headers = list(),
  query = list(),
  body = NULL
))

assert("API returns list", is.list(api_result))
assert("API has message field", !is.null(api_result$message))
assert("API message is correct", api_result$message == "Hello from Rune!")

# --- Test user param page ---
cat("\nDynamic route rendering:\n")

user_env <- new.env(parent = globalenv())
source(m4$route$file, local = user_env)
tmp_session2 <- create_session("test_ssr_2")
set_current_session(tmp_session2)
user_page <- user_env$page(list(id = "42"), tmp_session2)
user_html <- render_html(user_page)
assert("user page contains id 42", grepl("42", user_html))
remove_session(tmp_session2$id)

# --- Test HTTP handler directly ---
# Instead of starting a real server (which requires async request handling),
# we directly call the request handler with mock request environments.
# This tests the full stack: routing -> page rendering -> response.
cat("\nHTTP handler smoke test:\n")

css_files <- if (dir.exists(comp_dir)) {
  list.files(comp_dir, pattern = "\\.css$", full.names = TRUE)
} else {
  character(0)
}
theme_file <- file.path(demo_dir, "public", "theme.css")
client_js <- get_client_js()

# Helper: create a mock request environment
mock_request <- function(path, method = "GET") {
  list(
    PATH_INFO = path,
    REQUEST_METHOD = method,
    QUERY_STRING = "",
    HEADERS = list(),
    rook.input = NULL
  )
}

# GET /
tryCatch({
  resp <- handle_request(mock_request("/"), routes, routes_dir, demo_dir,
                          css_files, theme_file, client_js, FALSE)
  assert("GET / status 200", resp$status == 200L)
  assert("GET / returns HTML", grepl("<!DOCTYPE html>", resp$body))
  assert("GET / contains page content", grepl("Hello", resp$body))
  assert("GET / contains client.js script tag", grepl("client.js", resp$body))
}, error = function(e) {
  cat("  FAIL: GET / error:", e$message, "\n")
  fail <<- fail + 1L
})

# GET /api/hello
tryCatch({
  resp <- handle_request(mock_request("/api/hello"), routes, routes_dir, demo_dir,
                          css_files, theme_file, client_js, FALSE)
  assert("GET /api/hello status 200", resp$status == 200L)
  api_data <- jsonlite::fromJSON(resp$body)
  assert("API JSON has message", api_data$message == "Hello from Rune!")
}, error = function(e) {
  cat("  FAIL: GET /api/hello error:", e$message, "\n")
  fail <<- fail + 1L
})

# GET /users/99
tryCatch({
  resp <- handle_request(mock_request("/users/99"), routes, routes_dir, demo_dir,
                          css_files, theme_file, client_js, FALSE)
  assert("GET /users/99 status 200", resp$status == 200L)
  assert("GET /users/99 contains user id", grepl("99", resp$body))
}, error = function(e) {
  cat("  FAIL: GET /users/99 error:", e$message, "\n")
  fail <<- fail + 1L
})

# GET /nonexistent -> 404
tryCatch({
  resp <- handle_request(mock_request("/nonexistent"), routes, routes_dir, demo_dir,
                          css_files, theme_file, client_js, FALSE)
  assert("GET /nonexistent returns 404", resp$status == 404L)
  assert("404 body contains 404 text", grepl("404", resp$body))
}, error = function(e) {
  cat("  FAIL: GET /nonexistent error:", e$message, "\n")
  fail <<- fail + 1L
})

# GET /_rune/components.css
tryCatch({
  resp <- handle_request(mock_request("/_rune/components.css"), routes, routes_dir,
                          demo_dir, css_files, theme_file, client_js, FALSE)
  assert("GET /_rune/components.css status 200", resp$status == 200L)
  assert("CSS response has content-type", grepl("text/css", resp$headers[["Content-Type"]]))
}, error = function(e) {
  cat("  FAIL: CSS error:", e$message, "\n")
  fail <<- fail + 1L
})

# GET /_rune/client.js
tryCatch({
  resp <- handle_request(mock_request("/_rune/client.js"), routes, routes_dir,
                          demo_dir, css_files, theme_file, client_js, FALSE)
  assert("GET /_rune/client.js status 200", resp$status == 200L)
  assert("JS response has content-type",
         grepl("javascript", resp$headers[["Content-Type"]]))
}, error = function(e) {
  cat("  FAIL: JS error:", e$message, "\n")
  fail <<- fail + 1L
})

report("Integration tests")
