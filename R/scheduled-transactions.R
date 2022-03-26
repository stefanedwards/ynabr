
# frequency:
# never, daily, weekly, everyOtherWeek, twiceAMonth, every4Weeks, monthly, everyOtherMonth, every3Months, every4Months, twiceAYear, yearly, everyOtherYear

repeat.scheduled.transactions <- function(scheduled_transactions, column = c('date_first','date_next')) {
  column <- match.arg(column)
  col <- rlang::sym(column)



}


.repeat.dates <- function(start, end, frequency) {
  freq <- switch(frequency,
    #'never' = stop('?'),
    daily = 'day',
    weekly = 'week',
    everyOtherWeek = '2 weeks',
    twiceAMonth = months(1), ## and then add half a month
    every4Weeks = '4 weeks',
    monthly = months(1),
    everyOtherMonth = months(2),
    every3Months = months(3),
    every4Months = months(4),
    twiceAYear = months(6),
    yearly = lubridate::years(1),
    everyOtherYear = lubridate::years(2),
    stop('frequency not recognized')
  )

  start = as.Date(start)
  end = as.Date(end)
  if (is.character(freq)) {
    x <- seq(from=start, to=end, by=freq)
  } else if (lubridate::is.period(freq)) {
    ## estimate how many
    l <- ceiling(lubridate::interval(start, end) / freq)
    x <- c(start, lubridate::add_with_rollback(start, freq*1:l))
    if (frequency == 'twiceAMonth') {
      x <- c(x, .repeat.dates(start + lubridate::days(15), end, 'monthly')) %>%
        as.Date() %>%
        sort
    }
    x <- x[x<=end]
  }



  return(as.Date(x))
}

#' @export
repeat.dates <- Vectorize(.repeat.dates, vectorize.args=c('start', 'frequency'), SIMPLIFY = FALSE)
