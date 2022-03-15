## test helpers
## These shouldn't be exposed to the end user.

#' Create a mocked YNAB connection object that doesn't perform queries
#'
#' @param ... Named arguments of public methods to replace with either a function
#'   or a simple return value.
#' @return An YNAB connetion object that shouldn't be able to perform queries.
mock.YNAB <- function(...) {
  mocks <- list(...)
  if (is.null(mocks$Query)) mocks$Query <- function(...) stop('Query disabled in mock')

  accesstoken <- mocks$token %||% '1234'

  ynab <- YNAB$new(accesstoken)

  for (i in seq_along(mocks)) {
    n <- names(mocks)[i]
    base::unlockBinding(n, ynab)
    if (is.function(mocks[[i]])) {
      ynab[[n]] <- mocks[[i]]
    } else {
      ynab[[n]] <- function(...) mocks[[i]]
    }
    lockBinding(n, ynab)
  }

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

GET.budgets <- function(budgets, include.accounts=FALSE) {
  assertthat::assert_that(rlang::is_integerish(budgets, n=1))
  budgets = replicate(budgets, test.budget.1(), simplify=FALSE)
  list(
    budgets = budgets
  )
}
