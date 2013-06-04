#' GET a url (deferred version)
#'
#' see GET
#' @return a list of curl, response, where curl is a curl handle and
#' response is a continuation function processing the response from
#' a completed request
#'
#' @examples
#' google_later <- GET_deferred("http://www.google.com")
#' curlPerform(curl=google_later$curl)
#' google_later$response()
GET_deferred <- function(url = NULL, config = list(), ..., handle = NULL) {
  hu <- handle_url(handle, url, ...)
  make_request_deferred("get", hu$handle, hu$url, config = config)
}
