
#' Connect to the PI web API
#'
#' Connects to a web API endpoint, optionally with authentication. *Never store
#' passwords in script files or type them into the console!!!!* Instead, this
#' function will propmpt you for a password in RStudio, or you can set the
#' R_PI_PASSWORD environment variable (you could do so using
#' [usethis::edit_r_environ()]).
#'
#' @param endpoint The base URL for the endpoint.
#' @param user Username for authentication.
#'
#' @return A connection object
#' @export
#'
pi_connect <- function(endpoint, user = Sys.getenv("R_PI_USER")) {
  structure(
    list(
      endpoint = endpoint,
      user = user,
      auth = pi_autheticate(user)
    ),
    class = "pi_connection"
  )
}

#' @rdname pi_connect
#' @export
pi_autheticate <- function(user = Sys.getenv("R_PI_USER")) {
  if (!is.null(user) && user != "") {
    httr::authenticate(
      user,
      if (Sys.getenv("R_PI_PASSWORD") != "") {
        Sys.getenv("R_PI_PASSWORD")
      } else if (rstudioapi::isAvailable()) {
        rstudioapi::askForPassword(
          glue::glue("Enter PI password for '{user}':")
        )
      } else {
        rlang::abort(
          "No RStudio API found and no password found at environment variable 'R_PI_PASSWORD'"
        )
      }
    )
  } else {
    NULL
  }
}

pi_get <- function(con, fun = "", ...) {
  response <- httr::GET(
    glue::glue("{con$endpoint}/{fun}"),
    con$auth
  )
  httr::stop_for_status(response)
  httr::content(response, "parsed")
}
