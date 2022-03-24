
#' @importFrom assertthat assert_that
#' @import dplyr
#' @include guess-connections.R
category.estimate.remaining.payments.DEBT <- function(df, accounts, transactions, ...) {
  assert_that(is.list(accounts), all(sapply(accounts, is.account)), is.data.frame(transactions), is.transaction(transactions))
  df %>% filter(goal_type == 'DEBT') %>% pull(id) %>%
    guess.debt.account.from.transfer(., transactions) %>%
    rename(id=category_id) %>%
    inner_join(df, by=c('id')) %>%
    mutate(
      remaining_balance = purrr::map_dbl(transfer_account_id, ~accounts[[.]]$balance),
      payments_left = (remaining_balance + balance) * -1 / goal_target,
      payments_left = pmax(payments_left, 0)
    ) %>%
    select(id, transfer_account_id, name, budgeted, activity, category_balance=balance, goal_target, remaining_balance, payments_left)
}

#' TDB=’Target Category Balance by Date’,
#' category has a goal date -- use that!
#' @importFrom assertthat assert_that
#' @import dplyr
category.estimate.remaining.payments.TDB <- function(df, current_month=Sys.Date(), ...) {
  current_month <- as.Date(current_month)
  assert_that(!is.na(current_month))
  df %>% filter(goal_type == 'TBD') %>%
    mutate(
      first_month = ifelse(budgeted == 0,
          lubridate::floor_date(current_month, unit='month'),
          lubridate::ceiling_date(current_month, unit='month')
      ) %>% as.Date(origin=lubridate::origin),
      gtm = as.Date(goal_target_month),
      payments = lubridate::interval(x$start_month, x$gtm) %>% as.numeric('months') %>% round,
      installment = (goal_target - balance) / payments
  )

}
