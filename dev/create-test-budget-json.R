library(dplyr)
library(purrr)
library(tidyr)
library(glue)

basedir <- here::here('inst/extdata')
json.file <- file.path(basedir, 'test-budget.json')
readme.file <- file.path(basedir, 'test-budget.txt')

## get your personal access token and place it in a file `secret` in your project's root.
ynab <- YNAB$new(readLines(here::here('secret')))

budget_id <- "7bce0db0-b7b0-4a1a-9986-2828dbf2cea6" # test budget id
budget <- ynab$Query(paste0('/budgets/', budget_id))$budget
jsonlite::toJSON(budget, auto_unbox = TRUE, pretty = TRUE, null='null') %>%
  writeLines(json.file)

glue(.sep='\n',
  "The file '{basename(json.file)}' was created with the script",
  "dev/create-test-budget-json.R on {Sys.time()}."
) %>% writeLines(readme.file)
