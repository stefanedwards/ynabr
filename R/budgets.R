#' Budget object
#'
#' Create a YNAB budget object.
#'
#' @export
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
    assertthat::assert_that(is.ynab(ynab))

    # if (is.json.list) {
    #   assertthat::assert_that(is.list(id))
    #   call <- id
    #   call$ynab <- ynab
    #   call$is.json.list = FALSE
    #   return(do.call(YnabBudget$new, call))
    # }

    private$ynab <- ynab
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
  Id = function() { private$id },
  #' @field Name
  #' Budget name
  Name = function() { private$name },
  #' @field LastModifiedOn
  #' POSIXlt of when the budget was last modified.
  LastModifiedOn = function() { private$last_modified_on },
  #' @field FirstMonth String.
  FirstMonth = function() { private$first_month },
  #' @field LastMonth String.
  LastMonth = function() { private$last_month },
  #' @field DateFormat String of how dates are formatted.
  DateFormat = function() { private$date_format },
  #' @field CurrencyFormat A list-object describing how to format currencies in YNAB.
  CurrencyFormat = function() { private$currency_format },
  #' @field Accounts  List of accounts.
  Accounts = function() { private$accounts },
  #' @field Payees List of payees.
  Payees = function() { private$payees },
  #' @field PayeeLocations List of payee locations.
  PayeeLocations = function() { private$payee_locations},
  #' @field CategoryGroups List of category groups
  CategroupGroups = function() { private$category_groups },
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


#' @rdname Budget
#' @export
#' @param x Object to check/convert
#' @param ynab An \code{\link{YNAB}}-connection object
is.Budget <- function(x) {
  return(R6::is.R6(x) && inherits(x, 'Budget'))
}

#' @rdname Budget
#' @export
as.Budget <- function(x, ynab, ...) {
  if (is.list(x)) {
    return(as.YnabBudget.list(x, ynab, ...))
  }
  UseMethod('as.Budget', x)
}

as.YnabBudget.YnabBudget <- function(x, ...) {
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





