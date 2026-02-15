# User profile page - demonstrates dynamic route params
page <- function(params, session) {
  user_id <- params$id

  div(
    h1(paste("User Profile:", user_id)),
    Card(
      CardHeader(
        CardTitle(paste("User #", user_id)),
        CardDescription(paste("Viewing profile for user ID:", user_id))
      ),
      CardBody(
        p("This page demonstrates dynamic route parameters."),
        p(paste("The [id] segment in app/users/[id]/page.R matched:", user_id)),
        hr(),
        p("Try visiting:"),
        ul(
          li(a(href = "/users/1", "/users/1")),
          li(a(href = "/users/42", "/users/42")),
          li(a(href = "/users/hello", "/users/hello"))
        )
      )
    )
  )
}
