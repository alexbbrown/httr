#' Deferred fetches allow control to be passed to \code{\ref{curlMultiPerform}}
#'
#' make_request_deferred is a version of make_request that does not return the 
#' content, but returns a list of an r handle (curl) and a continuation function
#' (response) which can be invoked later to perform the fetch.
#' 
#' @param action function with (at least) arguments \code{handle}, \code{url},
#'   \code{opts}, which should return a list of \code{curl} and \code{response}
#'   first the curl handle should be \code{perform}ed and then the reponse
#'   function should be executed, to return binary content from the specified
#'  request. \code{make_request} will take care of resetting the handle's
#'   config after the request is made.
#' 
#' @examples
#' deferred_request <- httr:::make_request_deferred(method="GET",handle=handle$handle,url=handle$url)
#' curlPerform(curl=deferred_request$curl)
#' deferred_request$response()
make_request_deferred <- function(method, handle, url, ..., config = list()) {
  stopifnot(is.handle(handle))
  stopifnot(is.character(url), length(url) == 1)
  
  # to avoid handle re-use, always duplicate the handle
	handle$handle <- dupCurlHandle(handle$handle)

  # Sign request, if needed
  if (!is.null(config$signature)) {
    signed <- config$signature(method, url)
    url <- signed$url
    config <- c(config, signed$config)

    config$signature <- NULL
  }

  # Figure out curl options --------------------------------------------------
  opts <- default_config()
  opts$customrequest <- toupper(method)
  opts$url <- url

  # Action config override defaults
  config_f <- match.fun(str_c(tolower(method), "_config"))
  action_config <- config_f(...)
  opts <- modifyList(opts, action_config)

  # Config argument overrides everything
  opts <- modifyList(opts, config)

  # But we always override headerfunction and writefunction
  hg <- basicHeaderGatherer()
  opts$headerfunction <- hg$update
  buffer <- binaryBuffer()
  opts$writefunction <-
    getNativeSymbolInfo("R_curl_write_binary_data")$address
  opts$writedata <- buffer@ref

  # Must always reset the handle config, even if something goes wrong
  # ABB: not sure how to reconcile this with the deferred model
  #on.exit(reset_handle_config(handle, opts))

  # Perform request and capture output ---------------------------------------
  curl_opts <- curlSetOpt(curl = NULL, .opts = opts)#, .isProtected=TRUE)

  is_post <- isTRUE(attr(action_config, "post"))
  if (is_post) {
		warning("POST not fully supported yet")
    body <- attr(action_config, "body")
    style <- attr(action_config, "style") # ignored for now - see httppost
		curlSetOpt(curl=handle$handle, .opts = curl_opts$values, postfields=body, .isProtected=TRUE)
    #.postForm(handle$handle, curl_opts, body, style)
    # reset configuration after post - again not sure how
    # this fits, but it certainly must.
		warning("POST not fully supported yet - do not re-use handle")
    #curlSetOpt(httppost = NULL, post = NULL, postfields = NULL,
    #  curl = handle$handle)
  } else {
    curlSetOpt(curl = handle$handle, .opts = curl_opts$values, .isProtected=TRUE)
  }

	return( 
	  list(
			curl=handle$handle,
			config=action_config,
			response=function() {
				content <- as(buffer, "raw")
				info <- last_request(handle)
				times <- request_times(handle)
				headers <- insensitive(as.list(hg$value()))
				status <- as.numeric(str_extract(headers$status, "[0-9]+"))

				response(
					url = info$effective.url,
					handle = handle,
					status_code = status,
					headers = headers,
					cookies = parse_cookies(info$cookielist),
					content = content,
								times = times,
								config = config
				)
			}
		)
	)
}
