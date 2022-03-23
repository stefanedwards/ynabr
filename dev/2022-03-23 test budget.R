## Test budget

library(dplyr)
library(purrr)
library(tidyr)
library(glue)

json.file <- here::here('dev/test-budget.json')
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
  mutate_at(vars(budgeted, activity, balance, goal_target), ~./1000) %>%
  filter(deleted == FALSE)

categories.all.months <- bind_rows(budget$month) %>%
  select(-budgeted, -activity, -deleted) %>%
  mutate(categories = purrr::map(categories, bind_rows)) %>%
  unnest(categories, names_repair = 'check_unique')


categories %>% filter(goal_type == 'NEED') ## Plan your spending.
# No information on how often they are repeated.


