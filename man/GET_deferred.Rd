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
google_later <- GET_deferred("http://httpbin.org/get")
RCurl::curlPerform(curl=google_later$curl)
google_later$response()

delays<-as.list(paste0("delay/",rep(5,10)))
deferred_delays<-lapply(delays,function(x)GET_deferred("http://httpbin.org/",path=x))
multi <- RCurl::getCurlMultiHandle()
dev_null <- lapply(deferred_delays,function(a_post)RCurl::push(multi,a_post$curl))
system.time(RCurl::curlMultiPerform(multi))
unlist(lapply(deferred_delays,function(a_post)http_status(a_post$response())))

system.time(lapply(delays,function(x)GET("http://httpbin.org/",path=x)))
}

