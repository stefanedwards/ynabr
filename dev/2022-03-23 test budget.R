## Test budget

library(dplyr)
library(purrr)
library(tidyr)
library(glue)

json.file <- here::here('inst/extdata/test-budget.json')
if (FALSE) {
  ynab <- YNAB$new(readLines(here::here('secret')))

  budget_id <- "7bce0db0-b7b0-4a1a-9986-2828dbf2cea6" # test budget
  budget <- ynab$Query(paste0('/budgets/', budget_id))$budget
  jsonlite::toJSON(budget, auto_unbox = TRUE, pretty = TRUE, null='null') %>%
    writeLines(json.file)
} else {
  budget <- jsonlite::read_json(json.file, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
}

category_groups <- bind_rows(budget$category_groups) ## only that one
categories <- bind_rows(budget$categories) %>%
  #mutate_at(vars(budgeted, activity, balance, goal_target), ~./1000) %>%
  filter(deleted == FALSE)

categories %>% filter(!is.na(goal_type)) %>% View('categories with targets')

categories.all.months <- bind_rows(budget$month) %>%
  select(-budgeted, -activity, -deleted) %>%
  mutate(categories = purrr::map(categories, bind_rows)) %>%
  unnest(categories, names_repair = 'check_unique')


categories %>% filter(goal_type == 'NEED') ## Plan your spending.
# No information on how often they are repeated.


accounts <- idfy(budget$accounts)
# No information of interest!

transactions <- bind_rows(budget$transactions)

categories %>% filter(goal_type == 'DEBT') %>% select(id, name)

scheduled_transactions <- bind_rows(budget$scheduled_transactions) %>%
  inner_join(select(categories, category_name=name, category_id=id), by=c('category_id'))

#' @param category_id,goal_target,balance Character/numeric values, all refer to those in a DEBT category.
#' @return Number of payments left on the debt.
category.estimate.remaining.payments.DEBT <- function(category_id, goal_target, balance, accounts, transactions) {
  category_id <- as.character(category_id)
  assert_that(
    length(category_id) == length(goal_target),
    length(goal_target) == length(balance),
    rlang::is_integerish(goal_target, finite=TRUE),
    rlang::is_integerish(balance, finite=TRUE)
  )
  assert_that(is.list(accounts), all(sapply(accounts, is.account)), is.data.frame(transactions), is.transaction(transactions))

  browser()
  ## get account to get remaining balance:
  estimate.debt.account(category_id, transactions)
  remaining <- accounts[[account_id]]$balance

  (remaining - category$balance) / category$goal_target
}

category.estimate.remaining.payments.DEBT <- function(df, accounts, transactions) {
  assert_that(is.list(accounts), all(sapply(accounts, is.account)), is.data.frame(transactions), is.transaction(transactions))
  df_ <- df %>% filter(goal_type == 'DEBT')
  account_ids <- estimate.debt.account(df_$id, transactions)
  inner_join(df_, account_ids, by=c('id'='category_id')) %>%
    mutate(
      remaining_balance = purrr::map_dbl(transfer_account_id, ~accounts[[.]]$balance),
      payments_left = (remaining_balance + balance) * -1 / goal_target,
      payments_left = pmax(payments_left, 0)
    ) %>%
    select(id, transfer_account_id, name, budgeted, activity, balance, goal_target, remaining_balance, payments_left)
}

categories %>% category.estimate.remaining.payments.DEBT(accounts, transactions) %>%
  mutate_if(is.numeric, ~./1000) %>% View
