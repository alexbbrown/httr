#' Guess the media type of a path from it's extension.
#'
#' @param x path to file
#' @importFrom tools file_ext
#' @export
#' @examples
#' guess_media("report.doc")
#' guess_media("owl.png")
guess_media <- function(x) {
  ext <- file_ext(x)

  if (ext %in% names(ext_media)) {
    ext_media[[ext]]
  } else {
    NULL
  }
}

guess_media_url <- function(x) {
  guess_media(parse_url(x)$path)
}

cache_media <- function() {
  url <- "http://www.stdicon.com/mimetypes"
	ext_media <- unlist(RJSONIO::fromJSON(url))
	names(ext_media) <- gsub("^.","",names(ext_media))
  save(ext_media, file = "R/sysdata.rda")
}
