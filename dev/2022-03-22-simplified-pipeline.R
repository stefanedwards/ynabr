library(dplyr)
library(purrr)
library(tidyr)
library(glue)

if (FALSE) {
ynab <- YNAB$new(readLines(here::here('secret')))$load(include.accounts=TRUE)
budgets <- sapply(ynab$Budgets, `[[`, 'name')
names(budgets) <- sapply(ynab$Budgets, `[[`, 'id')

budget_id <- ynab$Budgets[[which(budgets == 'Stefan')]]$id
budget_id <- "7bce0db0-b7b0-4a1a-9986-2828dbf2cea6" # test budget
budget <- ynab$Query(paste0('/budgets/', budget_id))$budget
jsonlite::toJSON(budget, auto_unbox = TRUE, pretty = TRUE) %>%
  writeLines(here::here('dev/test-budget.json'))
}

category_groups <- bind_rows(budget$category_groups)
categories <- bind_rows(budget$categories) %>%
  inner_join(category_groups, c('category_group_id'='id'), suffix=c('','.group')) %>%
  mutate_at(vars(budgeted, activity, balance, goal_target), ~./1000)

categories %>% filter(!is.na(goal_type)) %>% View
# goal_type:
# The type of goal, if the category has a goal (
# TB=’Target Category Balance’,
# TBD=’Target Category Balance by Date’,
# MF=’Monthly Funding’,
# NEED=’Plan Your Spending’

## hairdressers, a repeating goal, every 2 months
hairdresser <- ynab$Query(glue('/budgets/{budget_id}/categories/e99ce16b-f672-4705-b3de-fc38885c71fa'))$category
# not much additional information here.
# no information on repetition

## car loan
categories %>% filter(id == "5e1db2ff-0a63-469c-b3cf-d95ce95f13d0")
# goal_type: DEBT, goal_target is monthly payment, no target_month


categories %>% filter(goal_type == 'MF') # Monthly Funding, i.e. "Saving Target" / "Monthly Savings Builder"
## just a goal_target = monthly "payment"

categories %>% filter(goal_type == 'TB') # Target Balance, i.e. "Savings Balance" (no date)
## goal_target = savings target;
## no target date...

categories %>% filter(goal_type == 'TBB') # Target Balance, i.e. "Savings Balance _by date_".

categories %>% filter(goal_type == 'NEED') ## Plan your spending.
categories %>% filter(goal_type == 'NEED', !is.na(goal_target_month)) ## Plan your spending -- with target date
## these are both recuring and non-recuring
# goal_target is savings target; goal_target_month is *original* target date -- needs information on repetition
## if goal_target_month is in the past, and there are still being budgeted to it, it would be recuring?



categories %>% filter(goal_type == 'NEED', is.na(goal_target_month)) ## Plan your spending -- monthly recuring

categories.all.months <- bind_rows(budget$month) %>%
  select(-budgeted, -activity, -deleted) %>%
  mutate(categories = purrr::map(categories, bind_rows)) %>%
  unnest(categories, names_repair = 'check_unique')
