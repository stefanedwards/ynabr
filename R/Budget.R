#' Budget object
#'
#' Create a YNAB budget object.
#'
#' @export
#' @importFrom assertthat assert_that
#' @importFrom rlang is_missing
#' @include 0_aux.R
Budget <- R6::R6Class('Budget',
public = list(
  #' @description
  #' Creates a new YNAB budget-object.
  #'
  #' @param ynab An \code{\link{YNAB}}-object.
  #' @param id,name,last_modified_on,first_month,last_month,date_format,currency_format,accounts
  #'   Values returned in BudgetSummary-model.
  #' @param ... Additional entries in the json-list, when receiving a
  #'   full budget (with accounts, payees, etc.), cf. the BudgetDetail-model.
  #' @importFrom lubridate as_date
  #' @return A new 'Budget' object.
  initialize = function(
    ynab,
    id, name, last_modified_on, first_month, last_month, date_format,
    currency_format, accounts,

    ...) {
    assert_that(is.ynab(ynab))

    private$ynab <- ynab
    self$Id <- id
    self$Name <- name
    private$last_modified_on <- parse_utc(last_modified_on)
    private$first_month <- lubridate::as_date(first_month)
    private$last_month <- lubridate::as_date(last_month)
    if (is.list(date_format) && !is.null(date_format$format)) {
      private$date_format <- as.character(date_format$form)
    } else {
      private$date_format <- as.character(date_format)
    }
    self$CurrencyFormat <- currency_format
    self$Accounts <- accounts




    invisible(self)
  },
  #' @description
  #' Look up an account with the given id.
  #' @param id Single string with id.
  #' @returns Account objet, if found, else NULL.
  GetAccountById = function(id) {
    assert_that(is_single_string(id))
    x <- private$accounts[[id]]
    if (is.null(x)) {
      i <- which(purrr::map_chr(private$accounts, ~.$Id))
      if (length(i) > 0)
        x <- private$accounts[[i[1]]]
    }
    return(x)
  },
  #' @description
  #' Prints this object.
  #' @param ... Discarded.
  print = function(...) {
    d <- difftime(lubridate::now(), private$last_modified_on)
    cat(glue::glue(
      'Ynab Budget: {private$name}\n',
      'Last modified {sprintf("%.0f", d)} {units(d)} ago.'
    ))
    invisible(self)
  },
  #' @description
  #' Returns copy of object as a list.
  #' @return A list
  ToList = function() {
    stop('`ToList` not implemented in `Budget`.')

    res <- as.list(private)
    res[c(
      'id', 'name','type', 'on_budget', 'closed', 'note', 'balance',
      'cleared_balance', 'uncleared_balance', 'transfer_payee_id', 'direct_import_linked',
      'direct_import_in_error', 'deleted'
    )]
  }
),
private = list(
  ynab = NULL,
  id = NULL,
  name = NULL,
  last_modified_on = NULL,
  first_month = NULL,
  last_month = NULL,
  date_format = NULL,
  currency_format = NULL,
  accounts = list(),
  payees = list(),
  payee_locations = list(),
  category_groups = list(),
  categories = list(),
  months = list(),
  transactions = list(),
  subtransactions = list(),
  scheduled_transactions = list(),
  scheduled_subtransactions = list()
),
active = list(
  #' @field YNAB YNAB-connection object
  YNAB = function() { private$ynab },
  #' @field Id
  #' Budget id
  Id = function(val) {
    if (is_missing(val))
      return( private$id )
    val <- as.character(val)
    assert_that(is_single_string(val))
    private$id <- val
  },
  #' @field Name
  #' Budget name
  Name = function(val) {
    if (is_missing(val))
      return(private$name)
    val <- as.character(val)
    assert_that(is_single_string(val))
    private$name <- val
  },
  #' @field LastModifiedOn
  #' POSIXlt of when the budget was last modified.
  LastModifiedOn = function(val) {
    if (is_missing(val))
      return(private$last_modified_on)
    val <- parse_utc(val)
    assert_that(length(val) == 1, !is.na(val))
    private$last_modified_on <- val
  },
  #' @field FirstMonth String.
  FirstMonth = function() { private$first_month },
  #' @field LastMonth String.
  LastMonth = function() { private$last_month },
  #' @field DateFormat String of how dates are formatted.
  DateFormat = function() { private$date_format },
  #' @field CurrencyFormat An \code{\link{CurrencyFormat}}-object describing how
  #'   to format currencies in YNAB.
  CurrencyFormat = function(val) {
    if (is_missing(val))
      return(private$currency_format)
    private$currency_format <- as.CurrencyFormat(val)
  },
  #' @field Accounts  List of accounts.
  Accounts = function(val) {
    if (missing(val))
      return( private$accounts )
    if (is.list(val))
      val <- as.Account(val, strict=TRUE)
    if (is.Account(val)) {
      val <- list(val)
      names(val) <- val$Id
    }
    assert_that(is.list(val), all(sapply(val, is.Account)))
    names(val) <- purrr::map_chr(val, ~.$Id)
    private$accounts <- val

  },
  #' @field Payees List of payees.
  Payees = function() { private$payees },
  #' @field PayeeLocations List of payee locations.
  PayeeLocations = function() { private$payee_locations},
  #' @field CategoryGroups List of category groups
  CategoryGroups = function() { private$category_groups },
  #' @field Categories List of categories.
  Categories = function() { private$categories },
  #' @field Months List of months with (scheduled?) transactions.
  Months = function() { private$months },
  #' @field Transactions List of transactions
  Transactions = function() { private$transactions },
  #' @field Subtransactions List of sub(?)transactions
  Subtransactions = function() { private$subtransactions },
  #' @field ScheduledTransactions List of scheduled transactions.
  ScheduledTransactions = function() { private$scheduled_transactions },
  #' @field ScheduledSubtransactions List of scheduled sub(?)transactions.
  ScheduledSubtransactions = function() { private$scheduled_subtransactions }
)
)


#' @describeIn Budget Checks whether an object is a Budget object.
#' @export
# @param x Object to check/convert
is.Budget <- function(x) {
  return(R6::is.R6(x) && inherits(x, 'Budget'))
}

#' @describeIn Budget Converts an object to a Budget object.
#' @export
#' @param ynab An \code{\link{YNAB}}-connection object
as.Budget <- function(x, ynab, ...) {
  if (is.list(x)) {
    return(as.Budget.list(x, ynab, ...))
  }
  UseMethod('as.Budget', x)
}

as.Budget.Budget <- function(x, ...) {
  x
}

as.Budget.list <- function(x, ynab, is.list=FALSE) {
  if (is.list) {
    budgets <- lapply(x, as.Budget, ynab=ynab)
    n <- sapply(budgets, function(x) x$Name, simplify=TRUE)
    names(budgets) <- make.unique(n)
    return(budgets)
  }
  x$ynab <- ynab
  return(do.call(Budget$new, x))
}


##' @describeIn Budget Converts a Budget object to a list.
##' @export
as.list.Budget <- function(x, ...) {
  x$ToList()
}
