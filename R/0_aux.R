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
  if (endpoint[1] == baseurl)
    endpoint <- endpoint[-1]

  url <- c(baseurl, endpoint) %>%
    trimws('both', '/')
  paste(url, collapse='/', sep='')
}


#' Packs the access token into the required header.
#' @noRd
#' @param token The access token.
#' @return A named character vector with the token.
#' @example
#' token <- '1234abc'
#' httr::add_headers(token_as_h(token))
token_as_h <- function(token) {
  assertthat::assert_that(rlang::is_character(token, n=1))
  c('Authorization'=paste('Bearer', token))
}

#' Wrapper call to add http headers and the token.
#'
#' Calls \code{httr}'s \code{\link[httr]{add_headers}}.
#'
#' @noRd
#' @param token The access token.
#' @param ... Additional headers, as named characters.
#' @return A \code{request} object for a http method, such as \code{\link[httr]{GET}}.
#' @example
#' add_headers('1234abc', 'X-type'='text')
#' # <request>
#' # Headers:
#' # * Authorization: Bearer 1234abc
#' # * X-type: text
add_headers <- function(token, ...) {
  httr::add_headers(token_as_h(token), ...)
}
