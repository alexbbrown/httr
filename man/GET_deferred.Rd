\name{GET_deferred}
\alias{GET_deferred}
\title{GET a url (deferred version)}
\usage{
  GET_deferred(url = NULL, config = list(), ...,
    handle = NULL)
}
\value{
  a list of curl, response, where curl is a curl handle and
  response is a continuation function processing the
  response from a completed request
}
\description{
  see GET
}
\examples{
google_later <- GET_deferred("http://www.google.com")
curlPerform(curl=google_later$curl)
google_later$response()
}

