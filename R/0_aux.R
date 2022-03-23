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

# Idfy -- names list-elements by the $id -----
#' Names list-elements by an element
#'
#' @export
#' @examples
#' l1 <- list(list(id = '123', name = 'peter'), list(id = '456', name = 'bingo'))
#' idfy(l1)
#' @param x List-object to traverse
#' @param el Name of the list-elements' element to use as name for the list-element.
#'   Can also be index.
#' @param max.depth Max recursion depth.
#' @param max.none.depth If a list-element doesn't contain the named element,
#'   how many levels can we recurse into without that named element.
#'   Not used for now.
#' @param names_repair Ensures that the names for the list-elements are valid.
#'   Accepts \code{"minimal"}, \code{"unique"}, \code{"universal"}, \code{"check_unique"},
#'   or \code{FALSE} to disable. See \code{\link[vctrs]{vec_as_names}}.
#' @param ... Arguments passed on to \code{\link[vctrs]{vec_as_names}}.
#' @return
#' Input \code{x} with all list-elements named after their element \code{el},
#' all the way down, up to \code{max.depth} levels.
idfy <- function(x, el='id', max.depth=Inf, max.none.depth=2, names_repair = 'minimal', ...) {
  assert_that(is.list(x))
  sublists <- sapply(x, is.list, simplify = TRUE)
  if (!length(sublists) || sum(sublists) == 0) return(x)

  if (rlang::is_integerish(el, n=1, finite=TRUE) && el > 0) {
    f <- ~if (length(.) < el) '' else .[[el]] %||% ''
  } else {
    f <- ~.[[el]] %||% ''
  }
  ids <- purrr::map_chr(x[sublists], f)

  nok <- ids == ''
  if (!all(nok)) {
    if (length(nok) > 0 & !is.null(names(x)))
      ids[nok] <- names(x)[sublists][nok]
    names(x)[sublists] <- ids
    if (length(names_repair) & names_repair != FALSE) {
      names(x) <- vctrs::vec_as_names(names(x), repair=names_repair, ...)
    }
  }

  if (max.depth > 0)
    x[sublists] <- lapply(x[sublists], idfy, el=el, max.depth=max.depth-1, max.none.depth=max.none.depth-1, names_repair=names_repair, ...)



  return(x)
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



