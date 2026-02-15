# =============================================================================
# Rune UI: Table
# =============================================================================
# A styled data table that renders from a data.frame or list of lists.
# =============================================================================

#' Create a Table component.
#' @param data A data.frame to render.
#' @param columns Optional character vector of column names to display.
#'        Defaults to all columns.
#' @param class Additional CSS classes.
Table <- function(data, columns = NULL, class = "") {
  if (is.null(columns)) {
    columns <- names(data)
  }

  # Build header row
  header_cells <- lapply(columns, function(col) {
    th(col)
  })
  header_row <- tr(header_cells)

  # Build body rows
  body_rows <- lapply(seq_len(nrow(data)), function(i) {
    cells <- lapply(columns, function(col) {
      td(as.character(data[[col]][i]))
    })
    tr(cells)
  })

  div(class = paste("rune-table-wrapper", class),
    table_(class = "rune-table",
      thead(header_row),
      tbody(body_rows)
    )
  )
}
