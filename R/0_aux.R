# auxiliary functions

#' Parse standard ISO 8601 into POSIXTlt
#' @noRd
#' @param x Character string.
#' @return POSIXlt object
parse_utc <- function(x) {
  lubridate::fast_strptime(x, '%Y-%m-%dT%H:%M:%OS%z')
}

#' Merges components of a baseurl and enpoints together.
#' @noRd
#' @param baseurl Single string, http://you.domain/'
#' @param ... Additional components that forms the endpoint.
url.endpoint <- function(baseurl, ...) {
  endpoint <- unlist(list(...)) %>% as.character
  url <- c(baseurl, endpoint) %>%
    trimws('both', '/')
  paste(url, collapse='/')
}
