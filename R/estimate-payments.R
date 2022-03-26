
#' For DEBT,  `target` is the monthly installment.
#'
#' Estimates how many payments there are left of a DEBT, by using the month's
#' goal_target as the installment.
#'
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
#' For TBD, `target` is the end goal.
#'
#' Estimates how many months there are between coming month and the categories' goal_target_month;
#' divides remaining needed between those.
#' @importFrom assertthat assert_that
#' @import dplyr
#' @param df Data frame of categories
#' @param current_month As Date object or string ('YYYY-MM-DD') as the basis date for the estimation.
#' @param include.target_month Logical, whether the goal_target_month is included as the last payment.
category.estimate.remaining.payments.TBD <- function(df, current_month=Sys.Date(), include.target_month=FALSE, ...) {
  current_month <- as.Date(current_month) %>% lubridate::ceiling_date(unit='month')
  assert_that(!is.na(current_month))
  df %>% filter(goal_type == 'TBD') %>%
    mutate(
      first_month = current_month,
      gtm = as.Date(goal_target_month),
      payments = lubridate::interval(first_month, gtm) %>% as.numeric('months') %>% round + as.integer(include.target_month),
      installment = (goal_target - balance) / payments
  )

}


#' NEED = Plan Your Spending
#'
#'
category.estimate.remaining.payments.NEED <- function(df, current_month=Sys.Date(), ...) {
  #current_month <- as.Date(current_month) %>% lubridate::ceiling_date(unit='month')
  #assert_that(!is.na(current_month))
  df %>% filter(goal_type == 'NEED', !is.na(goal_target_month) & goal_percentage_complete < 100) %>%
    mutate(
      .goal_type = goal_type,
      goal_type = 'TBD'
    ) %>% category.estimate.remaining.payments.TBD(current_month = current_month, ...) %>%
    select(-goal_type) %>% rename(goal_type = .goal_type)
}

#' MF - monthly savings builder
#' NEED, with no target month,
#'   no end date, or anything. simply this amount of money each month.
#' TB - Savings balance, without date.
#'   No idea. Include in an "after period" summary?
