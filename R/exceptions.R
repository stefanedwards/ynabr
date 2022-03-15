# exceptions

rate.limited <- function(endpoint, response, call = sys.call(-1)) {
  if (inherits(response, 'response')) {
    response <- httr::content(response)
  }
  now <- lubridate::now()
  wait <- difftime(lubridate::ceiling_date(now, unit='h'), now, units='s')
  structure(
    class = c('rate.limited', 'condition'),
    list(
      message = 'Call was rate limited from server side',
      call=call,
      wait = wait)
  )
}
