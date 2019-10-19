
#' Retrieve a list of all Asset Servers known to this service
#'
#' @inheritParams pi_get
#' @param webId The ID of the server.
#' @param ... Passed to [pi_get()]
#'
#' @export
#'
#' @references
#' https://devdata.osisoft.com/piwebapi/help/controllers/assetserver/actions/list
#' https://devdata.osisoft.com/piwebapi/help/controllers/assetserver/actions/get
#'
#' @examples
#' con <- pi_connect_public()
#' pi_assetserver_list(con)
#' pi_assetserver(con, "F1RSIRAQC7zjPUOfBqai218IAwUElTUlYx")
#'
pi_assetserver_list <- function(.con, ...) {
  pi_get(.con, "assetservers", ...)
}

#' @rdname pi_assetserver_list
#' @export
pi_assetserver <- function(.con, webId, ...) {
  webId <- pi_web_id(webId)
  pi_get(
    .con,
    glue::glue("assetservers/{webId}"),
    webId = webId,
    ...
  )
}
