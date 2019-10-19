
pi_connect <- function(endpoint, user = NULL) {
  structure(
    list(
      endpoint = endpoint,
      user = user,
      auth = pi_autheticate(user),
      handle = curl::new_handle()
    ),
    class = "pi_connection"
  )
}

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

pi_test <- function(con) {
  httr::GET()
}
