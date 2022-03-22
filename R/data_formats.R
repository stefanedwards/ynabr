#' @name data_formats
#' @rdname data_formats
#' @aliases milliunits
#' @md
#' @title YNAB API Data formats
#'
#' See <https://api.youneedabudget.com/#formats> for complete/updated information.
#'
#' @section Numbers:
#' Currency amounts returned from the API—such as account balance, category balance, and transaction amounts— use a format we call "milliunits". Most currencies don't have three decimal places, but you can think of it as the number of thousandths of a unit in the currency: 1,000 milliunits equals "one" unit of a currency (one Dollar, one Euro, one Pound, etc.). Here are some concrete examples:
#'
#' | Currency	| Milliunits | Amount |
#' |----------|------------|--------|
#' | USD ($)	| 123930	| $123.93 |
#' | USD ($)	| -220	| -$0.22 |
#' | Euro (€)	| 4924340	| €4.924,34 |
#' | Euro (€)	| -2990	| -€2,99 |
#' | Jordanian dinar	| -395032	-395.032 |
#'
#' @section Dates:
#' All dates returned in response calls use
#' [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601)
#' ([RFC 3339 "full-date"](https://tools.ietf.org/html/rfc3339#section-5.6)) format.
#' For example, "December 30, 2015" is formatted as `2015-12-30`.
#'
#' @section Timezone:
#' All dates use UTC as the timezone.
NULL
