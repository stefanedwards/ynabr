

#' @export
repeat.scheduled.transactions <- function(df, column = c('date_first','date_next'), end_date) {
  column <- match.arg(column)
  col <- rlang::sym(column)

  df %>%
    mutate(date_next = purrr::map2(date_next, frequency, .repeat.dates, end='2022-06-01')) %>%
    tidyr::unnest(date_next)

}

#' @import lubridate
#' @noRd
.repeat.dates <- function(start, end, frequency) {
  if (frequency == 'never') return(as.Date(start))
  freq <- switch(frequency,
    daily = days(1),
    weekly = weeks(1),
    everyOtherWeek = weeks(2),
    twiceAMonth = months(1), ## and then add half a month
    every4Weeks = weeks(4),
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
  if (start == end) return(start)
  ## estimate how many
  l <- ceiling(lubridate::interval(start, end) / freq)
  x <- c(start, lubridate::add_with_rollback(start, freq*1:l))
  if (frequency == 'twiceAMonth') {
    x <- c(x, .repeat.dates(start + lubridate::days(15), end, 'monthly')) %>%
      as.Date() %>%
      sort
  }
  x <- x[x<=end]

  return(as.Date(x))
}

#' @export
repeat.dates <- Vectorize(.repeat.dates, vectorize.args=c('start', 'frequency'), SIMPLIFY = FALSE, USE.NAMES=FALSE)


#' @returns Data.frame where a split transaction has been replaced by its subtransactions.
#' @export
build.scheduled.transactions <- function(budget) {
  assert_that(is.full_budget(budget), length(budget$scheduled_transactions) > 0)
  scheduled <- list.coalesce_1(budget$scheduled_transactions) %>%
    bind_rows()
  if (length(budget$scheduled_subtransactions) == 0) return(scheduled %>% mutate(scheduled_transaction_id = id))
  subs <- list.coalesce_1(budget$scheduled_subtransactions) %>%
    bind_rows() %>%
    inner_join(scheduled, by=c('scheduled_transaction_id'='id')) %>%
    mutate(
      category_id = category_id.x,
      amount = amount.x,
      memo = paste0(memo.y, ifelse(nchar(memo.y) & nchar(memo.x), ' // ', ''), memo.x),
      payee_id = coalesce(payee_id.x,payee_id.y),
      deleted = deleted.x | deleted.y,
      transfer_account_id = transfer_account_id.x
    ) %>%
    select(-ends_with('.x'), -ends_with('.y'))

  ## merge back into scheduled, replacing the split transactions
  anti_join(scheduled, subs, by=c('id'='scheduled_transaction_id')) %>%
    bind_rows(subs)
}
