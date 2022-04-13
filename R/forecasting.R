# forecasting


#' Creates data.frame with all months etc. between \code{start_date} and \code{end_date}.
#' Basically the same as \code{base::seq}.
#' @include transactions.R
expand.months <- function(start_date, end_date, category_id=NA_character_, frequency='monthly') {
  tidyr::crossing(
    month=.repeat.dates(start_date, end_date, frequency = frequency),
    category_id = category_id
  )
}

#' @importFrom rlang sym
year.month <- function(df, column, month_name='month', year_name='year', check.names=TRUE) {
  ## check existing names
  if (check.names && c(month_name, year_name) %in% names(df)) {
    stop('Names for new columns month and year already exists in input data.frame.')
  }

  m_ <- sym(month_name)
  y_ <- sym(year_name)

  df %>%
    mutate(local({
      d <- cur_data()[,column,drop=TRUE]
      d <- as.Date(d)
      tibble(!!m_:=month(d), !!y_:=year(d))
    }))

}


forecast.category.expenditure.NEED <- function(categories, start_date, end_date, frequency='monthly') {
  assert_that(is.category(categories), is.data.frame(categories))
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  tidyr::crossing(
    month = .repeat.dates(start_date, end_date, frequency),
    categories %>% filter(goal_type == 'NEED')
  ) %>%
    filter(!(!is.na(goal_target_month) & month > goal_target_month)) %>%
    group_by(id) %>%
    mutate(
      balance = ifelse(row_number() == 1, balance, 0),
      to_budget = goal_target - balance
    ) %>%
    ungroup %>%
    select(month, id, to_budget)
}

forecast.category.expenditure.DEBT <- function(categories, transactions, accounts, start_date, end_date, frequency='monthly') {
  assert_that(is.category(categories), is.data.frame(categories))
  #assert_that(is.transaction(transactions), is.data.frame(transactions))
  assert_that(rlang::is_bare_list(accounts), all(sapply(accounts, is.account)))

  accounts <- idfy(accounts, max.depth=1)

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  guess.debt.account.from.transfer(transactions, warnings=FALSE) %>%
    select(-transfers) %>%
    right_join(categories %>% filter(goal_type == 'DEBT'), by=c('category_id'='id')) %>%
    mutate(
      remaining_balance = purrr::map_dbl(transfer_account_id, ~accounts[[.]]$balance %||% NA) + balance
    ) %>%
    tidyr::crossing(
      month = .repeat.dates(start_date, end_date, frequency)
    ) %>%
    group_by(category_id) %>%
    arrange(month) %>%
    mutate(
      goal_target = ifelse(row_number() == 1, 0, goal_target),
      account_balance = remaining_balance + cumsum(goal_target),
      goal_target = ifelse(account_balance > 0, goal_target - account_balance, goal_target) %>% pmax(0),
      account_balance = pmin(account_balance, 0)
    ) %>% ungroup %>%
    select(month, id=category_id, to_budget=goal_target, account_balance) %>%
    arrange(id, month)

}
