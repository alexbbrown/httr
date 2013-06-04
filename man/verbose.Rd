\name{verbose}
\alias{verbose}
\title{Give verbose output.}
\usage{
  verbose()
}
\description{
  A verbose connection provides much more information about
  the flow of information between the client and server.
}
\details{
  \code{verbose} cannot always be combined with other
  request parameters in GET and other calls.  Use
  \code{set_config(verbose()} to set verbose on for all
  subsequent httr requests, and use \code{reset_config()}
  to stop verbose debugging.

  In the R GUI, verbose interacts strangely with readline,
  use with caution.
}
\examples{
b <- new_bin()
GET(b, verbose())
set_config(verbose())
GET(b)
reset_config()
}
\seealso{
  Other config: \code{\link{accept_json}},
  \code{\link{accept_xml}}, \code{\link{add_headers}},
  \code{\link{authenticate}}, \code{\link{config}},
  \code{\link{set_cookies}}, \code{\link{timeout}},
  \code{\link{use_proxy}}, \code{\link{user_agent}}
}

