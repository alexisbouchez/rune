# =============================================================================
# Log Viewer - Main Page
# =============================================================================
# Nginx access log analyzer with 3 tabs:
#   1. Most Visited Pages (KPIs + pie chart)
#   2. Traffic (regex-filtered traffic line chart)
#   3. Data (raw data table with pagination)
# =============================================================================

# --- Log parsing (done once at source time) ---
parse_access_log <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)
  # Nginx combined log format regex
  pattern <- '^(\\S+) \\S+ \\S+ \\[([^]]+)\\] "(\\S+) (\\S+) [^"]*" (\\d+) (\\d+) "[^"]*" "([^"]*)"'
  matches <- regmatches(lines, regexec(pattern, lines))
  valid <- vapply(matches, length, integer(1)) == 8L
  matches <- matches[valid]
  if (length(matches) == 0) {
    return(data.frame(
      ip = character(0), datetime = character(0), method = character(0),
      path = character(0), status = integer(0), size = integer(0),
      ua = character(0), timestamp = as.POSIXct(character(0)),
      stringsAsFactors = FALSE
    ))
  }
  df <- data.frame(
    ip       = vapply(matches, `[`, character(1), 2),
    datetime = vapply(matches, `[`, character(1), 3),
    method   = vapply(matches, `[`, character(1), 4),
    path     = vapply(matches, `[`, character(1), 5),
    status   = as.integer(vapply(matches, `[`, character(1), 6)),
    size     = as.integer(vapply(matches, `[`, character(1), 7)),
    ua       = vapply(matches, `[`, character(1), 8),
    stringsAsFactors = FALSE
  )
  df$timestamp <- as.POSIXct(df$datetime, format = "%d/%b/%Y:%H:%M:%S %z")
  df
}

BOT_PATTERN <- "bot|crawl|spider|slurp|Googlebot|Bingbot|Yandex|Baidu|AhrefsBot|SemrushBot|DotBot|MJ12bot|PetalBot"
STATIC_PATTERN <- "\\.(css|js|png|jpg|jpeg|gif|svg|ico|woff|woff2|ttf|eot|map|webp|avif)$"
CHART_COLORS <- c("#3b82f6", "#ef4444", "#10b981", "#f59e0b", "#8b5cf6",
                  "#ec4899", "#06b6d4", "#84cc16", "#f97316", "#6366f1")

# Parse logs once (cached in globalenv across sessions/requests)
LOG_FILE <- file.path(getwd(), "access.log")
if (!exists(".log_viewer_data", envir = .GlobalEnv)) {
  assign(".log_viewer_data", parse_access_log(LOG_FILE), envir = .GlobalEnv)
}
ALL_LOGS <- get(".log_viewer_data", envir = .GlobalEnv)

# --- IP geolocation cache (shared across sessions) ---
if (!exists(".geo_cache", envir = .GlobalEnv)) {
  assign(".geo_cache", new.env(parent = emptyenv()), envir = .GlobalEnv)
}
.geo_cache <- get(".geo_cache", envir = .GlobalEnv)

# Batch geolocate IPs using ip-api.com (free, 100 IPs/batch, 45 req/min)
geolocate_ips <- function(ips) {
  ips <- unique(ips)
  # Check which IPs we already have cached
  uncached <- ips[!vapply(ips, function(ip) exists(ip, envir = .geo_cache), logical(1))]
  if (length(uncached) > 0) {
    # Process in batches of 100
    for (i in seq(1, length(uncached), by = 100)) {
      batch <- uncached[i:min(i + 99, length(uncached))]
      body <- jsonlite::toJSON(
        lapply(batch, function(ip) list(query = ip, fields = "query,country,countryCode")),
        auto_unbox = TRUE
      )
      result <- tryCatch({
        tmp <- tempfile(fileext = ".json")
        writeLines(body, tmp)
        resp <- system2("curl", args = c("-s", "-X", "POST",
                                          "-H", "Content-Type: application/json",
                                          "-d", paste0("@", tmp),
                                          "http://ip-api.com/batch"),
                         stdout = TRUE, stderr = FALSE)
        unlink(tmp)
        if (length(resp) > 0 && nzchar(resp[1])) {
          jsonlite::fromJSON(paste(resp, collapse = ""))
        } else {
          NULL
        }
      }, error = function(e) NULL)

      if (!is.null(result) && is.data.frame(result)) {
        for (j in seq_len(nrow(result))) {
          ip <- result$query[j]
          cc <- if (!is.null(result$countryCode)) result$countryCode[j] else ""
          country <- if (!is.null(result$country)) result$country[j] else ""
          assign(ip, list(country = country, code = cc), envir = .geo_cache)
        }
      }
      # Rate limit: wait between batches
      if (i + 100 <= length(uncached)) Sys.sleep(1.5)
    }
  }
  # Return country for each IP
  vapply(ips, function(ip) {
    if (exists(ip, envir = .geo_cache)) {
      geo <- get(ip, envir = .geo_cache)
      geo$country
    } else {
      ""
    }
  }, character(1))
}

# Pre-fetch geolocation for all unique IPs at startup (only once)
if (nrow(ALL_LOGS) > 0 && !exists(".geo_done", envir = .GlobalEnv)) {
  cat("Fetching IP geolocation data...\n")
  unique_ips <- unique(ALL_LOGS$ip)
  geolocate_ips(unique_ips)
  cat(sprintf("Geolocated %d unique IPs\n", length(unique_ips)))
  assign(".geo_done", TRUE, envir = .GlobalEnv)
}

# Helper to get country for a vector of IPs (uses cache only, no network)
get_countries <- function(ips) {
  vapply(ips, function(ip) {
    if (exists(ip, envir = .geo_cache)) {
      get(ip, envir = .geo_cache)$country
    } else {
      ""
    }
  }, character(1))
}

get_country_codes <- function(ips) {
  vapply(ips, function(ip) {
    if (exists(ip, envir = .geo_cache)) {
      get(ip, envir = .geo_cache)$code
    } else {
      ""
    }
  }, character(1))
}

# Country code to flag emoji
country_flag <- function(codes) {
  vapply(codes, function(cc) {
    if (is.null(cc) || is.na(cc) || !nzchar(cc) || nchar(cc) != 2) return("")
    chars <- strsplit(toupper(cc), "")[[1]]
    # Regional indicator symbols: A=U+1F1E6, etc.
    paste0(vapply(chars, function(ch) {
      intToUtf8(0x1F1E6 + match(ch, LETTERS) - 1L)
    }, character(1)), collapse = "")
  }, character(1))
}

page <- function(params, session) {
  # =========================================================================
  # Reactive signals
  # =========================================================================
  active_tab     <- signal("overview")
  time_unit      <- signal("d")
  time_n         <- signal("30")
  exclude_bots   <- signal(TRUE)
  exclude_static <- signal(TRUE)
  regex_filter   <- signal("")
  dark_mode      <- signal(FALSE)
  table_page     <- signal(1L)
  TABLE_PAGE_SIZE <- 50L

  # =========================================================================
  # Computed: filtered data
  # =========================================================================
  filtered_data <- computed(function() {
    df <- ALL_LOGS
    if (nrow(df) == 0) return(df)

    n_val <- suppressWarnings(as.integer(time_n$get()))
    if (is.na(n_val) || n_val < 1) n_val <- 30L
    unit <- time_unit$get()
    secs <- switch(unit,
      "h" = 3600, "d" = 86400, "w" = 604800, "m" = 2592000, "y" = 31536000, 86400
    )
    cutoff <- max(df$timestamp, na.rm = TRUE) - (n_val * secs)
    df <- df[!is.na(df$timestamp) & df$timestamp >= cutoff, ]

    if (exclude_bots$get()) {
      df <- df[!grepl(BOT_PATTERN, df$ua, ignore.case = TRUE), ]
    }
    if (exclude_static$get()) {
      df <- df[!grepl(STATIC_PATTERN, df$path, ignore.case = TRUE), ]
    }

    rx <- regex_filter$get()
    if (nzchar(rx)) {
      keep <- tryCatch(grepl(rx, df$path, ignore.case = TRUE),
                       error = function(e) rep(TRUE, nrow(df)))
      df <- df[keep, ]
    }
    df
  })

  # =========================================================================
  # Event handlers
  # =========================================================================
  on_click("tab-overview", function(e, s) { active_tab$set("overview") })
  on_click("tab-traffic",  function(e, s) { active_tab$set("traffic") })
  on_click("tab-data",     function(e, s) { active_tab$set("data") })

  on_input("filter-unit", function(e, s) {
    time_unit$set(e$value); table_page$set(1L)
  })
  on_input("filter-n", function(e, s) {
    time_n$set(e$value); table_page$set(1L)
  })
  on_input("filter-bots", function(e, s) {
    exclude_bots$set(e$value == "true"); table_page$set(1L)
  })
  on_input("filter-static", function(e, s) {
    exclude_static$set(e$value == "true"); table_page$set(1L)
  })
  on_input("filter-regex", function(e, s) {
    regex_filter$set(e$value); table_page$set(1L)
  })

  on_click("dark-toggle", function(e, s) {
    dark_mode$set(!dark_mode$get())
  })

  on_click("page-prev", function(e, s) {
    pg <- table_page$get()
    if (pg > 1L) table_page$set(pg - 1L)
  })
  on_click("page-next", function(e, s) {
    df <- filtered_data$get()
    max_pg <- max(1L, ceiling(nrow(df) / TABLE_PAGE_SIZE))
    pg <- table_page$get()
    if (pg < max_pg) table_page$set(pg + 1L)
  })

  # =========================================================================
  # Chart helpers
  # =========================================================================
  send_charts <- function(sess) {
    df <- filtered_data$get()
    tab <- active_tab$get()
    is_dark <- dark_mode$get()
    text_col <- if (is_dark) "#e2e8f0" else "#1a1a2e"
    grid_col <- if (is_dark) "rgba(226,232,240,0.1)" else "rgba(0,0,0,0.06)"

    if (tab == "overview" && nrow(df) > 0) {
      counts <- sort(table(df$path), decreasing = TRUE)
      top_n <- min(5L, length(counts))
      labs <- names(counts)[seq_len(top_n)]
      vals <- as.integer(counts[seq_len(top_n)])
      if (length(counts) > top_n) {
        labs <- c(labs, "Other")
        vals <- c(vals, sum(as.integer(counts[-seq_len(top_n)])))
      }
      send_chart(sess, "pie-chart", list(
        type = "doughnut",
        data = list(
          labels = labs,
          datasets = list(list(
            data = vals,
            backgroundColor = CHART_COLORS[seq_along(labs)],
            borderWidth = 0
          ))
        ),
        options = list(
          responsive = TRUE, maintainAspectRatio = FALSE,
          plugins = list(
            legend = list(position = "right",
                          labels = list(color = text_col, font = list(size = 12))),
            title = list(display = TRUE, text = "Top Visited Pages",
                         color = text_col, font = list(size = 16, weight = "bold"))
          )
        )
      ))
    }

    if (tab == "traffic" && nrow(df) > 0) {
      unit <- time_unit$get()
      fmt <- switch(unit,
        "h" = "%Y-%m-%d %H:00", "d" = "%Y-%m-%d", "w" = "%Y-W%W",
        "m" = "%Y-%m", "y" = "%Y", "%Y-%m-%d"
      )
      buckets <- table(format(df$timestamp, fmt))
      labs <- names(buckets)
      vals <- as.integer(buckets)
      ord <- order(labs)
      labs <- labs[ord]; vals <- vals[ord]

      send_chart(sess, "line-chart", list(
        type = "line",
        data = list(
          labels = labs,
          datasets = list(list(
            label = "Requests", data = vals,
            borderColor = "#3b82f6", backgroundColor = "rgba(59,130,246,0.1)",
            fill = TRUE, tension = 0.3, pointRadius = 2
          ))
        ),
        options = list(
          responsive = TRUE, maintainAspectRatio = FALSE,
          plugins = list(
            legend = list(labels = list(color = text_col)),
            title = list(display = TRUE, text = "Traffic Over Time",
                         color = text_col, font = list(size = 16, weight = "bold"))
          ),
          scales = list(
            x = list(ticks = list(color = text_col, maxTicksLimit = 15),
                      grid = list(color = grid_col)),
            y = list(ticks = list(color = text_col),
                      grid = list(color = grid_col), beginAtZero = TRUE)
          )
        )
      ))
    }
  }

  # =========================================================================
  # Render helpers
  # =========================================================================
  render_filters <- function() {
    uv <- time_unit$get()
    div(class = "log-filters",
      div(class = "log-filter-group",
        label_("Last", `for` = "filter-n"),
        input(id = "filter-n", type = "number", value = time_n$get(),
              min = "1", max = "999")
      ),
      div(class = "log-filter-group",
        label_("Unit", `for` = "filter-unit"),
        select_(id = "filter-unit",
          option(value = "h", selected = (uv == "h"), "Hours"),
          option(value = "d", selected = (uv == "d"), "Days"),
          option(value = "w", selected = (uv == "w"), "Weeks"),
          option(value = "m", selected = (uv == "m"), "Months"),
          option(value = "y", selected = (uv == "y"), "Years")
        )
      ),
      div(class = "log-checkbox-group",
        if (exclude_bots$get()) input(id = "filter-bots", type = "checkbox", checked = TRUE)
        else input(id = "filter-bots", type = "checkbox"),
        label_("Exclude bots", `for` = "filter-bots")
      ),
      div(class = "log-checkbox-group",
        if (exclude_static$get()) input(id = "filter-static", type = "checkbox", checked = TRUE)
        else input(id = "filter-static", type = "checkbox"),
        label_("Exclude static", `for` = "filter-static")
      ),
      if (active_tab$get() == "traffic") {
        div(class = "log-filter-group",
          label_("URL regex", `for` = "filter-regex"),
          input(id = "filter-regex", type = "text", value = regex_filter$get(),
                placeholder = "/articles/.*")
        )
      }
    )
  }

  render_kpis <- function() {
    df <- filtered_data$get()
    n_hits <- nrow(df)
    n_ips <- length(unique(df$ip))
    n_pages <- length(unique(df$path))
    avg_sz <- if (n_hits > 0) round(mean(df$size, na.rm = TRUE)) else 0L
    div(class = "log-kpis",
      div(class = "log-kpi",
        div(class = "log-kpi-label", "Total Hits"),
        div(class = "log-kpi-value", format(n_hits, big.mark = ","))
      ),
      div(class = "log-kpi",
        div(class = "log-kpi-label", "Unique IPs"),
        div(class = "log-kpi-value", format(n_ips, big.mark = ","))
      ),
      div(class = "log-kpi",
        div(class = "log-kpi-label", "Unique Pages"),
        div(class = "log-kpi-value", format(n_pages, big.mark = ","))
      ),
      div(class = "log-kpi",
        div(class = "log-kpi-label", "Avg Response"),
        div(class = "log-kpi-value", paste0(format(avg_sz, big.mark = ","), " B"))
      )
    )
  }

  render_data_tab <- function() {
    df <- filtered_data$get()
    if (nrow(df) == 0) {
      return(div(class = "log-empty", "No data matches the current filters."))
    }
    pg <- table_page$get()
    max_pg <- max(1L, ceiling(nrow(df) / TABLE_PAGE_SIZE))
    if (pg > max_pg) pg <- max_pg
    s <- (pg - 1L) * TABLE_PAGE_SIZE + 1L
    e <- min(pg * TABLE_PAGE_SIZE, nrow(df))
    pdf <- df[s:e, , drop = FALSE]
    codes <- get_country_codes(pdf$ip)
    flags <- country_flag(codes)
    countries <- get_countries(pdf$ip)
    location <- ifelse(nzchar(flags),
                        paste(flags, countries),
                        countries)
    ddf <- data.frame(
      IP = pdf$ip,
      Country = location,
      Time = format(pdf$timestamp, "%Y-%m-%d %H:%M:%S"),
      Method = pdf$method,
      Path = pdf$path,
      Status = as.character(pdf$status),
      Size = format(pdf$size, big.mark = ","),
      stringsAsFactors = FALSE
    )
    list(
      Table(ddf),
      div(class = "log-pagination",
        span(paste0("Showing ", s, "-", e, " of ", nrow(df), " rows")),
        div(class = "log-pagination-btns",
          Button(id = "page-prev", label = "Prev", variant = "outline", size = "sm",
                 disabled = (pg <= 1L)),
          span(paste0("Page ", pg, " / ", max_pg)),
          Button(id = "page-next", label = "Next", variant = "outline", size = "sm",
                 disabled = (pg >= max_pg))
        )
      )
    )
  }

  # =========================================================================
  # Island: main content
  # =========================================================================
  register_island("main-content", function() {
    tab <- active_tab$get()
    is_dark <- dark_mode$get()
    sess <- get_current_session()

    # Dark mode body class
    if (!is.null(sess$ws)) {
      send_body_class(sess, "dark", add = is_dark)
    }

    tab_content <- if (tab == "overview") {
      list(
        render_kpis(),
        div(class = "log-chart-container", style = "height:400px",
          rune_node("canvas", id = "pie-chart")
        )
      )
    } else if (tab == "traffic") {
      list(
        div(class = "log-chart-container", style = "height:400px",
          rune_node("canvas", id = "line-chart")
        )
      )
    } else {
      render_data_tab()
    }

    result <- list(
      # Top bar
      div(class = "log-topbar",
        h1("Log Viewer"),
        div(class = "log-topbar-actions",
          Button(id = "dark-toggle",
                 label = if (is_dark) "Light Mode" else "Dark Mode",
                 variant = "outline", size = "sm")
        )
      ),
      # Tabs
      div(class = "log-tabs",
        button(id = "tab-overview",
               class = paste("log-tab", if (tab == "overview") "active"),
               "Most Visited"),
        button(id = "tab-traffic",
               class = paste("log-tab", if (tab == "traffic") "active"),
               "Traffic"),
        button(id = "tab-data",
               class = paste("log-tab", if (tab == "data") "active"),
               "Data")
      ),
      # Filters
      render_filters(),
      # Tab content
      tab_content
    )

    # Send chart data after DOM is built
    if (!is.null(sess$ws)) {
      send_charts(sess)
    }

    result
  })

  # =========================================================================
  # Initial SSR render
  # =========================================================================
  island("main-content",
    div(class = "log-topbar",
      h1("Log Viewer"),
      div(class = "log-topbar-actions",
        Button(id = "dark-toggle", label = "Dark Mode", variant = "outline", size = "sm")
      )
    ),
    div(class = "log-tabs",
      button(id = "tab-overview", class = "log-tab active", "Most Visited"),
      button(id = "tab-traffic", class = "log-tab", "Traffic"),
      button(id = "tab-data", class = "log-tab", "Data")
    ),
    render_filters(),
    render_kpis(),
    div(class = "log-chart-container", style = "height:400px",
      rune_node("canvas", id = "pie-chart")
    )
  )
}
