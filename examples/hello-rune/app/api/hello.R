# API route - returns JSON
handle <- function(req) {
  list(
    message = "Hello from Rune!",
    method = req$method,
    timestamp = as.character(Sys.time()),
    version = "0.1.0"
  )
}
