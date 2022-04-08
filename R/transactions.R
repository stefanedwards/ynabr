

#' @export
#' @importFrom assertthat assert_that
#' @import lubridate
repeat.scheduled.transactions <- function(df, end_date) {
  end_date <- as.Date(end_date)
  assert_that(is.Date(end_date))

  df %>%
    mutate(date_next = purrr::map2(date_next, frequency, .repeat.dates, end=end_date)) %>%
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


#' Merge transactions and subtransactions
#'
#'
#' @rdname transactions
#' @returns Data.frame where a split transaction has been replaced by its subtransactions.
#' @export
build.transactions <- function(budget) {
  assert_that(is.full_budget(budget), length(budget$transactions) > 0)
  transactions <- list.coalesce_1(budget$transactions) %>%
    bind_rows()
  if (length(budget$subtransactions) == 0) return(transactions %>% mutate(transaction_id = id))
  subs <- list.coalesce_1(budget$subtransactions) %>%
    bind_rows() %>%
    inner_join(transactions, by=c('transaction_id'='id')) %>%
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
  anti_join(transactions, subs, by=c('id'='transaction_id')) %>%
    bind_rows(subs) %>%
    mutate(
      reconciled = cleared == 'reconciled',
      cleared = cleared == 'cleared'
    )
}

#' @rdname transactions
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


#' @export
add.account.info <- function(transactions, budget) {
  assert_that(is.full_budget(budget), length(budget$accounts) > 0, is.data.frame(transactions))

  budget$accounts %>% bind_rows() %>%
    select(account_id = id, account_name = name, account_on_budget=on_budget) %>%
    right_join(transactions, by='account_id')
}


monthly.expenses.by.category <- function(transactions) {
  assert_that(is.data.frame(transactions), is.transaction(transactions))
  transactions %>%
    mutate(date = as_date(date)) %>%
    group_by(month = month(date), year=year(date), category_id) %>%
    summarise(total_amount = sum(amount))

}
