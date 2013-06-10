\name{POST_deferred}
\alias{POST_deferred}
\title{construct a deferred POST of a file (or body or form) to a server.}
\usage{
  POST_deferred(url = NULL, config = list(), body = NULL,
    multipart = FALSE, ..., handle = NULL)
}
\arguments{
  \item{body}{Use \code{NULL} for an empty body, a
  length-one character or file. Each component should
  either be a character value or the object returned by
  \code{\link[RCurl]{fileUpload}} (if you want to upload a
  file).  If \code{multipart} is \code{FALSE} elements will
  be escaped automatically - if the values have already
  been escaped, then use `I` to prevent double-escaping.}

  \item{multipart}{Should the form be send as
  multipart/form-data (\code{TRUE}), or
  application/x-www-form-urlencoded (\code{FALSE}).  Files
  can not be uploaded when \code{FALSE}.}
}
\description{
  Haven't tested files yet.  Body as list doesn't work
  right.
}
\details{
  Body as list is beta - ish.  leave multipart to false -
  RCurl can't do the right setOptions for httppost right
  now.
}
\examples{
deferred_post<-POST_deferred("http://httpbin.org/",path="post",body="foo")
curlPerform(curl=deferred_post$curl)
content(deferred_request$response())

bodies <- c("harpo","zeppo","groucho")
deferred_posts<-lapply(bodies,function(body)POST_deferred("http://httpbin.org/",path="post",body=body))
multi <- getCurlMultiHandle()
lapply(deferred_posts,function(a_post)push(multi,a_post$curl))
curlMultiPerform(multi)
lapply(deferred_posts,function(a_post)content(a_post$response())$data)
}

