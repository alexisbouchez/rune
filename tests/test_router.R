# =============================================================================
# Tests: Router
# =============================================================================

cat("=== Router Tests ===\n")

source(file.path(Sys.getenv("RUNE_ROOT", ".."), "tests", "test_helpers.R"), local = FALSE)
source(file.path(core_dir, "R", "html.R"), local = FALSE)
source(file.path(core_dir, "R", "router.R"), local = FALSE)

# --- Test parse_route_pattern ---

cat("\nparse_route_pattern:\n")

r1 <- parse_route_pattern("/")
assert("root pattern is /", r1$pattern == "/")
assert("root regex matches /", grepl(r1$regex, "/"))
assert("root has no params", length(r1$param_names) == 0)

r2 <- parse_route_pattern("/about")
assert("about pattern is /about", r2$pattern == "/about")
assert("about regex matches /about", grepl(r2$regex, "/about"))
assert("about regex does NOT match /", !grepl(r2$regex, "/"))
assert("about has no params", length(r2$param_names) == 0)

r3 <- parse_route_pattern("/users/[id]")
assert("users/[id] pattern is /users/[id]", r3$pattern == "/users/[id]")
assert("users/[id] regex matches /users/42", grepl(r3$regex, "/users/42"))
assert("users/[id] regex matches /users/hello", grepl(r3$regex, "/users/hello"))
assert("users/[id] regex does NOT match /users/", !grepl(r3$regex, "/users/"))
assert("users/[id] regex does NOT match /users/42/extra",
       !grepl(r3$regex, "/users/42/extra"))
assert("users/[id] has param 'id'", identical(r3$param_names, "id"))

r4 <- parse_route_pattern("/posts/[slug]/comments/[commentId]")
assert("nested params parse correctly",
       identical(r4$param_names, c("slug", "commentId")))
assert("nested params regex matches",
       grepl(r4$regex, "/posts/my-post/comments/123"))

# --- Test match_route ---

cat("\nmatch_route:\n")

routes <- list(
  list(pattern = "/", regex = "^/$", param_names = character(0),
       file = "page.R", type = "page", layouts = character(0)),
  list(pattern = "/about", regex = "^/about$", param_names = character(0),
       file = "about/page.R", type = "page", layouts = character(0)),
  list(pattern = "/api/hello", regex = "^/api/hello$", param_names = character(0),
       file = "api/hello.R", type = "api", layouts = character(0)),
  list(pattern = "/users/([^/]+)", regex = "^/users/([^/]+)$",
       param_names = "id", file = "users/[id]/page.R", type = "page",
       layouts = character(0))
)

m1 <- match_route("/", routes)
assert("matches root path", !is.null(m1))
assert("root match has correct file", m1$route$file == "page.R")
assert("root match has no params", length(m1$params) == 0)

m2 <- match_route("/about", routes)
assert("matches /about", !is.null(m2))
assert("/about has correct file", m2$route$file == "about/page.R")

m3 <- match_route("/api/hello", routes)
assert("matches /api/hello", !is.null(m3))
assert("/api/hello is api type", m3$route$type == "api")

m4 <- match_route("/users/42", routes)
assert("matches /users/42", !is.null(m4))
assert("/users/42 has param id=42", identical(m4$params$id, "42"))

m5 <- match_route("/users/alice", routes)
assert("matches /users/alice", !is.null(m5))
assert("/users/alice has param id=alice", identical(m5$params$id, "alice"))

m6 <- match_route("/nonexistent", routes)
assert("returns NULL for unknown path", is.null(m6))

m7 <- match_route("/about/", routes)
assert("trailing slash on /about/ still matches", !is.null(m7))

# --- Test build_route_table ---

cat("\nbuild_route_table:\n")

demo_app_dir <- file.path(rune_root, "examples", "hello-rune", "app")
if (dir.exists(demo_app_dir)) {
  rt <- build_route_table(demo_app_dir)
  assert("route table is a list", is.list(rt))
  assert("route table has entries", length(rt) > 0)

  patterns <- vapply(rt, function(r) r$pattern, character(1))
  assert("has root route /", "/" %in% patterns)
  assert("has /about route", "/about" %in% patterns)
  assert("has /api/hello route", "/api/hello" %in% patterns)
  assert("has /users dynamic route", any(grepl("users", patterns)))

  rm1 <- match_route("/", rt)
  assert("route table: matches /", !is.null(rm1))
  assert("route table: / is a page", rm1$route$type == "page")

  rm2 <- match_route("/api/hello", rt)
  assert("route table: matches /api/hello", !is.null(rm2))
  assert("route table: /api/hello is api", rm2$route$type == "api")

  rm3 <- match_route("/users/99", rt)
  assert("route table: matches /users/99", !is.null(rm3))
  assert("route table: param id = 99", identical(rm3$params$id, "99"))
} else {
  cat("  SKIP: demo app not found at", demo_app_dir, "\n")
}

report("Router tests")
