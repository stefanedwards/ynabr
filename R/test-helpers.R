## test helpers
mock.YNAB <- function() {
  ynab <- YNAB$new('1234')
  unlockBinding("Query", ynab)
  ynab$Query <- function(...) stop('Query disabled in mock')
  lockBinding("Query", ynab)
  ynab
}

dk.currency.format <- function() list(
  currency_symbol = 'kr.',
  decimal_digits = 2L,
  decimal_separator = ',',
  display_symbol = FALSE,
  example_format = "123.456,78",
  group_separator = '.',
  iso_code = 'DKK',
  symbol_first = FALSE
)

test.budget.1 <- function() list(
  id = '1234',
  name = 'test budget 1',
  last_modified_on = '2022-03-15T08:26:52.361Z',
  first_month = '2022-02-01',
  last_month = '2022-04-01',
  date_format = list(format = 'DD-MM-YYYY'),
  currency_format = dk.currency.format(),
  accounts = list()
)

GET.budgets <- function(include.accounts=FALSE)
  list(
    data = list(
      test.budget.1()
    )
  )

