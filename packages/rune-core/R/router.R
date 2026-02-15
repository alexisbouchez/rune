# =============================================================================
# Rune Router
# =============================================================================
# File-based routing inspired by Next.js App Router.
#
# Convention:
#   app/page.R          -> "/"
#   app/about/page.R    -> "/about"
#   app/users/[id]/page.R -> "/users/:id"  (param: id)
#   app/api/hello.R     -> "/api/hello"    (API route)
#   app/layout.R        -> layout wrapper for "/" and all sub-routes
#   app/about/layout.R  -> nested layout for "/about" and sub-routes
#
# Route matching walks from most-specific to least-specific.
# Dynamic segments like [id] match any single path segment.
# =============================================================================

#' Scan an app directory and build a route table.
#' @param app_dir Path to the app/ directory.
#' @return A list of route entries, each with: pattern, regex, param_names,
#'         file, type ("page" or "api"), layouts (character vector of layout files).
build_route_table <- function(app_dir) {
  app_dir <- normalizePath(app_dir, mustWork = TRUE)
  routes <- list()

  # Find all page.R and api .R files
  all_files <- list.files(app_dir, recursive = TRUE, full.names = TRUE)
  page_files <- all_files[basename(all_files) == "page.R"]
  api_files <- all_files[grepl("/api/", all_files) & basename(all_files) != "page.R" &
                          grepl("\\.R$", all_files)]

  # Process page routes
  for (f in page_files) {
    rel <- substring(f, nchar(app_dir) + 1)  # e.g. "/about/page.R" or "/page.R"
    # Remove /page.R suffix to get the route directory
    route_dir <- sub("/page\\.R$", "", rel)
    if (route_dir == "") route_dir <- "/"

    route <- parse_route_pattern(route_dir)
    route$file <- f
    route$type <- "page"
    route$layouts <- find_layouts(app_dir, route_dir)
    routes <- c(routes, list(route))
  }

  # Process API routes
  for (f in api_files) {
    rel <- substring(f, nchar(app_dir) + 1)  # e.g. "/api/hello.R"
    route_path <- sub("\\.R$", "", rel)  # "/api/hello"

    route <- parse_route_pattern(route_path)
    route$file <- f
    route$type <- "api"
    route$layouts <- character(0)
    routes <- c(routes, list(route))
  }

  # Sort: static routes first, then by specificity (more segments first)
  routes <- routes[order(
    vapply(routes, function(r) length(r$param_names) > 0, logical(1)),
    -vapply(routes, function(r) nchar(r$pattern), integer(1))
  )]

  routes
}

#' Parse a route directory path into a pattern with regex and param names.
#' e.g. "/users/[id]" -> pattern="/users/[id]", regex="^/users/([^/]+)$", params=c("id")
parse_route_pattern <- function(route_dir) {
  # Normalize
  pattern <- route_dir
  if (!startsWith(pattern, "/")) pattern <- paste0("/", pattern)

  # Extract param names from [name] segments
  param_names <- character(0)
  matches <- gregexpr("\\[([^]]+)\\]", pattern)
  if (matches[[1]][1] != -1) {
    param_names <- regmatches(pattern, matches)[[1]]
    param_names <- gsub("\\[|\\]", "", param_names)
  }

  # Build regex: replace [name] with ([^/]+), escape the rest
  regex <- pattern
  # Escape regex special chars (except [ and ])
  regex <- gsub("\\.", "\\\\.", regex)
  # Replace [param] with capture group
  regex <- gsub("\\[([^]]+)\\]", "([^/]+)", regex)
  # Anchor
  regex <- paste0("^", regex, "$")

  list(
    pattern = pattern,
    regex = regex,
    param_names = param_names
  )
}

#' Find all layout.R files that apply to a given route directory.
#' Walks from root to the route's directory, collecting layout.R files.
find_layouts <- function(app_dir, route_dir) {
  layouts <- character(0)

  # Split route into segments
  if (route_dir == "/") {
    segments <- character(0)
  } else {
    segments <- strsplit(sub("^/", "", route_dir), "/")[[1]]
  }

  # Check root layout
  root_layout <- file.path(app_dir, "layout.R")
  if (file.exists(root_layout)) {
    layouts <- c(layouts, root_layout)
  }

  # Check each nested directory
  current <- app_dir
  for (seg in segments) {
    current <- file.path(current, seg)
    layout_file <- file.path(current, "layout.R")
    if (file.exists(layout_file)) {
      layouts <- c(layouts, layout_file)
    }
  }

  layouts
}

#' Match a request path against the route table.
#' @param path The request URL path (e.g. "/users/42").
#' @param routes The route table from build_route_table().
#' @return A list with: route (the matched route entry), params (named list).
#'         NULL if no match.
match_route <- function(path, routes) {
  # Normalize path
  if (!startsWith(path, "/")) path <- paste0("/", path)
  # Remove trailing slash (except for root)
  if (nchar(path) > 1 && endsWith(path, "/")) {
    path <- sub("/$", "", path)
  }

  for (route in routes) {
    m <- regexec(route$regex, path)
    if (m[[1]][1] != -1) {
      captures <- regmatches(path, m)[[1]]
      params <- list()
      if (length(route$param_names) > 0) {
        # First capture is full match, rest are groups
        for (i in seq_along(route$param_names)) {
          params[[route$param_names[i]]] <- captures[i + 1]
        }
      }
      return(list(route = route, params = params))
    }
  }

  NULL  # no match -> 404
}
