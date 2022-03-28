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
  iso_code = 'DKK',
  example_format = "123.456,78",
  decimal_digits = 2L,
  decimal_separator = ',',
  symbol_first = FALSE,
  group_separator = '.',
  currency_symbol = 'kr.',
  display_symbol = TRUE
)



#' @export
#' @returns List object, such as the one returned from the YNAB API.
#'   This one is an entirely fictious budget.
load.test.budget <- function(lib.loc=NULL) {
  json.file <- system.file('extdata/test-budget.json', package = 'ynabr', lib.loc=lib.loc, mustWork=TRUE)
  jsonlite::read_json(json.file, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
}
