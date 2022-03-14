api.list.budgets <- function(token, baseurl, include.accounts=FALSE) {
  include.account <- as.logical(include.accounts)
  assertthat::assert_that(rlang::is_logical(include.account, n=1))
  r <- httr::GET(
    endpoint('budgets', baseurl),
    query=list(include_accounts=include.accounts),
    httr::timeout(5),
    httr::add_headers(token_as_h(token))
  )

  httr::stop_for_status(r, "Retrieving list of budgets.")
  httr::content(r)$data
}

endpoint <- function(x, baseurl) {
  paste0(trimws(baseurl, 'right', '/'), '/', x)
}


token_as_h <- function(token) {
  assertthat::assert_that(rlang::is_character(token, n=1))
  c('Authorization'=paste('Bearer', token))
}


#' Budget object
#'
#' @export
YnabBudget <- R6::R6Class('YnabBudget',
public = list(
  #' @description
  #' Creates a new YNAB budget-object.
  #'
  #' @param ynab An \code{\link{YNAB-object}}.
  #' @param id,name,last_modified_on,first_month,last_month,date_format,currency_format,accounts
  #'   Values/list
  #' @param is.json.list Logical, when \code{TRUE}, \code{id} can be a list.
  #' @importFrom lubridate as_date
  #' @return A new `YNAB` object.
  initialize = function(
    ynab,
    id, name, last_modified_on, first_month, last_month, date_format,
    currency_format, accounts,
    is.json.list = FALSE) {
    if (is.json.list) {
      call <- id
      call$ynab <- ynab
      call$is.json.list = FALSE
      return(do.call(YnabBudget$new, call))
    }

    private$id <- as.character(id)
    private$name <- as.character(name)
    private$last_modified_on <- parse_utc(last_modified_on)
    private$first_month <- as_date(first_month)
    private$last_month <- as_date(last_month)
    if (is.list(date_format) && !is.null(date_format$format)) {
      private$date_format <- as.character(date_format$form)
    } else {
      private$date_format <- as.character(date_format)
    }

    assertthat::assert_that(is.list(currency_format))
    private$currency_format <- currency_format

    invisible(self)
  }
),
private = list(
  id = NULL,
  name = NULL,
  last_modified_on = NULL,
  first_month = NULL,
  last_month = NULL,
  date_format = NULL,
  currency_format = NULL,
  accounts = list()
),
active = list(
  #' @field Id
  #' Budget id
  Id = function() { private$id },
  #' @field Name
  #' Budget name
  Name = function() { private$name }
)
)
