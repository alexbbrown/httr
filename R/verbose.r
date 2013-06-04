#' Give verbose output.
#'
#' A verbose connection provides much more information about the flow of
#' information between the client and server.
#'
#' \code{verbose} cannot always be combined with other request parameters in GET and other
#' calls.  Use \code{set_config(verbose()} to set verbose on for all subsequent
#' httr requests, and use \code{reset_config()} to stop verbose debugging.
#'
#' In the R GUI, verbose interacts strangely with readline, use with caution.
#'
#' @family config
#' @export
#' @examples
#' b <- new_bin()
#' GET(b, verbose())
#' set_config(verbose())
#' GET(b)
#' reset_config()
verbose <- function() config(verbose = TRUE)
