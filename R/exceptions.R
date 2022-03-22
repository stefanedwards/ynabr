# exceptions

#' @export
#' @concept Exceptions
rate.limited.exception <- function(endpoint, response, call = sys.call(-1)) {
  if (inherits(response, 'response')) {
    response <- httr::content(response)
  }
  now <- lubridate::now()
  wait <- difftime(lubridate::ceiling_date(now, unit='h'), now, units='s')
  structure(
    class = c('rate.limited.exception', 'condition'),
    list(
      message = 'Call was rate limited from server side',
      call=call,
      wait = wait)
  )
}

#' @export
#' @concept Exceptions
key.already.exists.exception <- function(key, list, call = sys.call(-1)) {
  structure(
    class = c('key.already.exists.exception', 'condition'),
    list(
      message = glue::glue("Key '{key}' already exists in `{list}`"),
      call=call,
      key = key,
      list = list
    )
  )
}

