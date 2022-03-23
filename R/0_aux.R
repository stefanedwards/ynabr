# auxiliary functions

## From some tidyverse
`%||%` <- function(x,y) {
  if (!is.null(x)) return(x)
  return(y)
}

# --- Functions for querying the YNAB API ---------------

#' Parse standard ISO 8601 into POSIXTlt
#' @noRd
#' @param x Character string.
#' @return POSIXlt object
parse_utc <- function(x) {
  if (lubridate::is.POSIXct(x)) return(x)
  lubridate::fast_strptime(x, '%Y-%m-%dT%H:%M:%OS%z')
}

#' Merges components of a baseurl and enpoints together.
#' @noRd
#' @param baseurl Single string, http://you.domain/'
#' @param ... Additional components that forms the endpoint.
url.endpoint <- function(baseurl, ...) {
  endpoint <- as.character(unlist(list(...)))
  if (endpoint[1] == baseurl)
    endpoint <- endpoint[-1]

  url <- trimws(c(baseurl, endpoint), 'both', '/')
  paste(url, collapse='/', sep='')
}


#' Packs the access token into the required header.
#' @noRd
#' @param token The access token.
#' @return A named character vector with the token.
#' @examples
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
#' @examples
#' add_headers('1234abc', 'X-type'='text')
#' # <request>
#' # Headers:
#' # * Authorization: Bearer 1234abc
#' # * X-type: text
add_headers <- function(token, ...) {
  httr::add_headers(token_as_h(token), ...)
}


# Wrappers around common rlang type predicates -----s

#' Wrappers around common rlang type predicates
#'
#' @rdname scalar-type-predicates
#' @param na.ok Logical, when TRUE, the scalar may be a \code{NA}.
#' @docType internal
#' @importFrom rlang is_scalar_logical
is_single_logical <- function(x, na.ok=FALSE) {
  #browser()
  if (na.ok && length(x) == 1 && is.na(x)) return(TRUE)
  return(is_scalar_logical(x) && !is.na(x))
}

#' @rdname scalar-type-predicates
#' @importFrom rlang is_scalar_double
is_single_numeric <- function(x, na.ok=FALSE) {
  if (na.ok && length(x) == 1 && is.na(x)) return(TRUE)
  return((is_scalar_double(x) || is_scalar_integer(x)) && is.finite(x))
}

#' @rdname scalar-type-predicates
#' @importFrom rlang is_scalar_character
is_single_string <- function(x, na.ok=FALSE) {
  if (na.ok && length(x) == 1 && is.na(x)) return(TRUE)
  return(is_scalar_character(x) && !is.na(x))
}



