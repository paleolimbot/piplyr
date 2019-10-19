
#' Connect to the PI web API
#'
#' Connects to a web API endpoint, optionally with authentication. *Never store
#' passwords in script files or type them into the console!!!!* Instead, this
#' function will propmpt you for a password in RStudio, or you can set the
#' R_PI_PASSWORD environment variable (you could do so using
#' [usethis::edit_r_environ()]). Use [pi_connect_public()] to connect to the
#' [OSIsoft public web API endpoint](https://pisquare.osisoft.com/docs/DOC-2653-public-pi-web-api-endpoint)
#' (to do so, you'll need to set the R_PI_PUBLIC_USER and R_PI_PUBLIC_PASSWORD
#' environment variables).
#'
#' @param endpoint The base URL for the endpoint.
#' @param user Username for authentication.
#' @param test Use `test = FALSE` to skip testing the endpoint/authentication.
#'   This is used to reduce the number of HTTP calls in the package tests.
#'
#' @return A connection object
#' @export
#'
#' @examples
#' pi_connect_public()
#'
pi_connect <- function(endpoint = Sys.getenv("R_PI_ENDPOINT"),
                       user = Sys.getenv("R_PI_USER"), test = TRUE) {
  if (identical(endpoint, "")) {
    rlang::abort(
      "`endpoint` must be specified if the 'R_PI_ENDPOINT' environment variable is not set"
    )
  }

  con <- structure(
    list(
      # trip trailing slashes off of the endpoint
      endpoint = gsub("/+$", "", endpoint),
      user = user,
      auth = pi_autheticate(user)
    ),
    class = "pi_connection"
  )

  if (test) {
    # test connection and add base information
    con$Links <- pi_get(con)$Links
  }

  con
}

#' @rdname pi_connect
#' @export
pi_connect_public <- function(test = TRUE) {
  withr::with_envvar(
    list(
      R_PI_USER = Sys.getenv("R_PI_PUBLIC_USER"),
      R_PI_PASSWORD = Sys.getenv("R_PI_PUBLIC_PASSWORD")
    ),
    pi_connect("https://devdata.osisoft.com/piwebapi", test = test)
  )
}

pi_connect_test <- function() {
  pi_connect_public(test = FALSE)
}

pi_connect_httpbin <- function(user = Sys.getenv("R_PI_USER")) {
  pi_connect("https://httpbin.org", user = user, test = FALSE)
}

pi_autheticate <- function(user = Sys.getenv("R_PI_USER")) {
  if (!is.null(user) && user != "") {
    httr::authenticate(
      user,
      if (Sys.getenv("R_PI_PASSWORD") != "") {
        Sys.getenv("R_PI_PASSWORD")
      } else if (rstudioapi::isAvailable()) {
        pass <- rstudioapi::askForPassword(
          glue::glue("Enter PI password for '{user}':")
        )

        if (is.null(pass)) {
          rlang::abort("RStudio password dialog cancelled")
        }

        pass
      } else {
        rlang::abort(
          "No RStudio API found and no password found at environment variable 'R_PI_PASSWORD'"
        )
      }
    )
  } else {
    list()
  }
}

#' @export
print.pi_connection <- function(x, ...) {
  if (length(x$auth) > 0) {
    auth <- "basic"
  } else {
    auth <- "anonymous"
  }

  cat(
    glue::glue(
      "
      <pi_connection>
        base URL: {x$endpoint}
        authentication: {auth}
      "
    )
  )

  invisible(x)
}


#' Low-level API access
#'
#' @param .con A [pi_connect()] object
#' @param .fun A function ame
#' @param .quiet Use `.quiet = FALSE` to see the URL calls
#' @param .verb The HTTP verb to use (see [httr::VERB()]).
#' @param ... Key-value query string parameters. This uses [rlang::list2()],
#'   so splicing (i.e., `!!!`) is supported.
#'
#' @return [pi_get()] returns a [list()], [pi_response()] returns an
#'   [httr::response()], and [pi_url()] returns a bare URL.
#' @export
#'
#' @examples
#' con <- pi_connect_public()
#' pi_url(con, "assetservers")
#' pi_response(con, "assetservers")
#' result <- pi_get(con, "assetservers")
#' result$Items[[1]]$Name
#'
pi_get <- function(.con, .fun = NULL, ..., .quiet = TRUE) {
  response <- pi_response(.con, .fun, ..., .verb = "GET", .quiet = .quiet)
  httr::stop_for_status(response)
  httr::content(response, "parsed")
}

#' @rdname pi_get
#' @export
pi_response <- function(.con, .fun = NULL, ..., .verb = "GET", .quiet = TRUE) {
  url <- pi_url(.con, .fun, ...)
  if (!.quiet) {
    message(glue::glue("{.verb} {url}"))
  }

  httr::VERB(.verb, url, config = .con$auth)
}

#' @rdname pi_get
#' @export
pi_url <- function(.con, .fun = NULL, ...) {
  url_base <- paste0(
    c(
      .con$endpoint,
      gsub("(^/)|(/$)", "", .fun)
    ),
    collapse = "/"
  )

  httr::modify_url(url_base, query = rlang::list2(...))
}
