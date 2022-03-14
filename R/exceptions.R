# exceptions

rate.limited <- function(endpoint, response, call = sys.call(-1)) {
  if (inherits(response, 'response')) {
    response <- content(response)
  }
  now <- lubridate::now()
  wait <- difftime(lubridate::ceiling_date(now, unit='h'), now, unit='s')
  structure(
    class = c('rate.limited', 'condition'),
    list(
      message = 'Call was rate limited from server side',
      call=call,
      wait = wait)
  )
}
