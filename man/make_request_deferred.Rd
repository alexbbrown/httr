\name{make_request_deferred}
\alias{make_request_deferred}
\title{Deferred fetches allow control to be passed to \code{\ref{curlMultiPerform}}}
\usage{
  make_request_deferred(method, handle, url, ...,
    config = list())
}
\arguments{
  \item{action}{function with (at least) arguments
  \code{handle}, \code{url}, \code{opts}, which should
  return a list of \code{curl} and \code{response} first
  the curl handle should be \code{perform}ed and then the
  reponse function should be executed, to return binary
  content from the specified request. \code{make_request}
  will take care of resetting the handle's config after the
  request is made.}
}
\description{
  make_request_deferred is a version of make_request that
  does not return the content, but returns a list of an r
  handle (curl) and a continuation function (response)
  which can be invoked later to perform the fetch.
}
\examples{
deferred_request <- httr:::make_request_deferred(method="GET",handle=handle$handle,url=handle$url) -> X
curlPerform(curl=deferred_request$curl)
deferred_request$response()
}

