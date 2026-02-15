# Rune UI: Table
Table <- function(data, columns = NULL, class = "") {
  if (is.null(columns)) columns <- names(data)
  header_cells <- lapply(columns, function(col) th(col))
  header_row <- tr(header_cells)
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
