#' Maintain a pool of handles.
#'
#' The handle pool is used to automatically reuse Curl handles for the same
#' scheme/host/port combination.  This ensures that the http session is
#' automatically reused, and cookies are maintained across requests to a site
#' without user intervention.
#'
#' @format An environment.
#' @keywords internal
handle_pool <- NULL
empty_pool <- function() {
  handle_pool <<- new.env(hash = TRUE, parent = emptyenv())
}
empty_pool()

find_handle <- function(url) {
  handle_name <- build_url(parse_url(url)[c("scheme", "hostname", "port")])

  # Note: if multi is in use, the handle pool may contain handles that are already 
  # in use, and should not be re-used.  On the other hand, when handles have 
  # concluded their operation and are popped from the multi, they can be re-used
  # which can help take advantage of HTTP/1.1.  The pool might thus contain 
  # multiple handles for a given scheme/hostname/port.  None of this is supported
  # right now, so find_handle should not be used with multi.
  if (exists(handle_name, handle_pool)) {
    handle <- handle_pool[[handle_name]]
  } else {
    handle <- handle(handle_name)
    handle_pool[[handle_name]] <- handle
  }

  handle
}

