#' Account class
#'
#' Describes an account in your YNAB budget.
#'
#' The account-data is not always returned when qurying the YNAB API.
#' In some endpoints, you have to explicitly set \code{include_accounts = TRUE}
#' to have the accounts included in the response.
#'
#' Deleted accounts are only included in delta requests.
#' @rdname Account
#' @export
#' @importFrom assertthat assert_that
#' @importFrom rlang is_missing
#' @include 0_aux.R
Account <- R6::R6Class('Account',
public = list(
  #' @description
  #' Creates a new CurrencyFormat-object.
  #'
  #' @param id,name,type,on_budget,closed,note,balance,cleared_balance,uncleared_balance,transfer_payee_id,direct_import_linked,direct_import_in_error,deleted
  #'   Values; to create the account.
  #' @param is_milliunits Logical, whether the balances are given in \link{milliunits}.
  #'   All responses from the YNAB API are in milliunits!
  #' @return A new 'CurrencyFormat' object.
  initialize = function(id, name, balance, cleared_balance,
    type = 'checking', on_budget = TRUE, note = NA_character_, closed = FALSE,
    uncleared_balance = balance - cleared_balance,
    transfer_payee_id = NA_character_,
    direct_import_linked = FALSE, direct_import_in_error = FALSE,
    deleted = FALSE, is_milliunits = TRUE) {

    self$Id <- id
    self$Name <- name
    self$Balance <- balance
    self$ClearedBalance <- cleared_balance
    self$Type <- type
    self$OnBudget <- on_budget
    self$Note <- note
    self$Closed <- closed
    self$UnclearedBalance <- uncleared_balance
    self$TransferPayeeId <- transfer_payee_id
    self$DirectImportLinked <- direct_import_linked
    self$DirectImportInError <- direct_import_in_error
    self$Deleted <- deleted

    is_milliunits <- as.logical(is_milliunits)
    assert_that(is_single_logical(is_milliunits))
    private$is_milliunits <- is_milliunits

    invisible(self)
  },

  #' @description
  #' Prints this object.
  #' @param currency When provided, must be a \code{\link{CurrencyFormat}}
  #'   to format the currency.
  #' @param ... Discarded.
  print = function(currency, ...) {
    s <- NULL
    args <- list(...)
    if (!is_missing(currency)) {
      assert_that(is.CurrencyFormat(currency))
      s <- "\nBalance: {balances['balance']} ({balances['cleared']} cleared)"
      balances <- currency$format(self$AsCurrency)
    }

    cat(glue::glue(.null='',
      'Account {private$name} (id {private$id})', s
    ))
    invisible(self)
  },
  #' @description
  #' Returns copy of object as a list, with balances set in milliunits!
  #' @return A list
  ToList = function() {
    if (!self$IsMilliunits) {
      x <- self$clone()
      x$IsMilliunits <- TRUE
      return(x$ToList())
    }

    res <- as.list(private)
    res[c(
      'id', 'name','type', 'on_budget', 'closed', 'note', 'balance',
      'cleared_balance', 'uncleared_balance', 'transfer_payee_id', 'direct_import_linked',
      'direct_import_in_error', 'deleted'
    )]
  },

  #' @description
  #' Toggles whether the balances are in \link{milliunits}.
  #' @param condition Logical, whether to flip the balances to/from milliunits.
  ToggleMilliunits = function(condition) {
    if (is_missing(condition)) {
      return(self$ToggleMilliunits(!self$IsMilliunits))
    }
    condition <- as.logical(condition)
    assert_that(is_single_logical(condition))
    if (condition != private$is_milliunits) {
      multiplier <- if (private$is_milliunits) 1/1000 else 1000

      private$balance <- private$balance * multiplier
      private$cleared_balance <- private$cleared_balance * multiplier
      private$uncleared_balance <- private$uncleared_balance * multiplier

      if (condition) {
        private$balance <- round(private$balance)
        private$cleared_balance <- round(private$cleared_balance)
        private$uncleared_balance <- round(private$uncleared_balance)
      }

      private$is_milliunits <- condition
    }

    invisible(self)
  }
),
private = list(
  id = NA_character_,
  name = NA_character_,
  type = NA_character_,
  on_budget = NA,
  closed = NA,
  note = NA_character_,
  balance = NA_integer_,
  cleared_balance = NA_integer_,
  uncleared_balance = NA_integer_,
  transfer_payee_id = NA_character_,
  direct_import_linked = NA,
  direct_import_in_error = NA,
  deleted = NA,
  is_milliunits = NA
),
active = list(
  #' @field Id Internal id of account
  Id = function(val) {
    if (is_missing(val))
      return(private$id)
    val <- as.character(val)
    assert_that(is_single_string(val))
    private$id <- val
  },
  #' @field Name Name of account
  Name = function(val) {
    if (is_missing(val))
      return(private$name)
    val <- as.character(val)
    assert_that(is_single_string(val))
    private$name <- val
  },
  #' @field Type Account type
  Type = function(val) {
    if (is_missing(val))
      return(private$type)
    val <- as.character(val)
    assert_that(is_single_string(val), val %in% self$AccountTypes)
    private$type <- val
  },
  #' @field OnBudget Whether this account is on budget or not.
  OnBudget = function(val) {
    if (is_missing(val))
      return(private$on_budget)
    val <- as.logical(val)
    assert_that(is_single_logical(val))
    private$on_budget <- val
  },
  #' @field Closed Whether this account is closed or not.
  Closed = function(val) {
    if (is_missing(val))
      return(private$closed)
    val <- as.logical(val)
    assert_that(is_single_logical(val))
    private$closed <- val
  },
  #' @field Note Note associated to account. \code{NA} when missing.
  Note = function(val) {
    if (is_missing(val))
      return(private$note)
     val <- as.character(val)
     assert_that(is_single_string(val, na.ok=TRUE))
     private$note <- val
  },
  #' @field Balance Number, the current balance of the account.
  Balance = function(val) {
    if (is_missing(val))
      return(private$balance)
    suppressWarnings(val <- as.numeric(val))
    assert_that(is_single_numeric(val))
    private$balance <- val
  },
  #' @field ClearedBalance Number, the current \emph{cleared} balance of the account.
  ClearedBalance = function(val) {
    if (is_missing(val))
      return(private$cleared_balance)
    suppressWarnings(val <- as.numeric(val))
    assert_that(is_single_numeric(val))
    private$cleared_balance <- val
  },
  #' @field UnclearedBalance Number, the current \emph{uncleared} balance of the account.
  UnclearedBalance = function(val) {
    if (is_missing(val))
      return(private$uncleared_balance)
    suppressWarnings(val <- as.numeric(val))
    assert_that(is_single_numeric(val))
    private$uncleared_balance <- val
  },
  #' @field TransferPayeeId String, the payee id which should be used when transferring to this account.
  TransferPayeeId = function(val) {
    if (is_missing(val))
      return(private$transfer_payee_id)
    val <- as.character(val)
    assert_that(is_single_string(val, na.ok=TRUE))
    private$transfer_payee_id <- val
  },
  #' @field DirectImportLinked Logical, whether or not the account is linked to a financial institution for automatic transaction import.
  DirectImportLinked = function(val) {
    if(is_missing(val))
      return(private$direct_import_linked)
    val <- as.logical(val)
    assert_that(is_single_logical(val))
    private$direct_import_linked <- val
  },
  #' @field DirectImportInError Logical, if an account linked to a financial institution (\code{DirectImportLinked = TRUE}) and the linked connection is not in a healthy state, this will be true.
  DirectImportInError = function(val) {
    if(is_missing(val))
      return(private$direct_import_in_error)
    val <- as.logical(val)
    assert_that(is_single_logical(val))
    private$direct_import_in_error <- val
  },
  #' @field Deleted Logical, whether or not the account has been deleted. Deleted accounts will only be included in delta requests.
  Deleted = function(val) {
    if (is_missing(val))
      return(private$deleted)
    val <- as.logical(val)
    assert_that(is_single_logical(val))
    private$deleted <- val
  },

  #' @field AsCurrency Numeric, named vector of balances. Always "on" unit of a currency.
  #'   Vectors named as "balance", "cleared" and "uncleared".
  AsCurrency = function() {
    res <- c(
      'balance' = private$balance,
      'cleared' = private$cleared_balance,
      'uncleared' = private$uncleared_balance
    )
    if (self$IsMilliunits)
      res <- res / 1000

    res
  },
  #' @field AsMilliunits Numeric, named vector of balances. Always as \link{milliunits}.
  #'   Vectors named as "balance", "cleared" and "uncleared".
  AsMilliunits = function() {
    res <- c(
      'balance' = private$balance,
      'cleared' = private$cleared_balance,
      'uncleared' = private$uncleared_balance
    )
    if (!self$IsMilliunits)
      res <- res * 1000.0

    res
  },
  #' @field IsCleared Logical, when the current balance is fully cleared.
  IsCleared = function() { abs(private$balance - private$cleared_balance) < 0.0001 },
  #' @field IsMilliunits Whether the balances are given in milliunits format.
  IsMilliunits = function(val) {
    if (is_missing(val))
      return(private$is_milliunits)
    self$ToggleMilliunits(val)
  },
  #' @field AccountTypes Returns character vector of allowable account types
  AccountTypes = function() { .account_types }
))

.account_types <- c(
  'checking',
  'savings',
  'cash',
  'creditCard',
  'lineOfCredit',
  'otherAsset',
  'otherLiability',
  'personalLoan',
  'payPal',
  'autoLoan',
  'merchantAccount',
  'investmentAccount',
  'mortgage'
)

#' @describeIn Account Returns vector of allowable account types.
AccountTypes = function() {
  return(.account_types)
}

#' @describeIn Account Checks whether an object is an Account object.
#' @export
is.Account <- function(x) {
  return(R6::is.R6(x) && inherits(x, 'Account'))
}

#' @describeIn Account Converts a list with named elements to a new
#'   Account object.
#' @export
as.Account <- function(x, ...) {
  if (is.Account(x))
    return(x)
  if (is.list(x))
    return(as.Account.list(x, ...))

  UseMethod('as.Account', x)
}


as.Account.list <- function(x, strict=TRUE, ...) {
  if (length(x) == 0) return(x)

  if (all(sapply(x, is.atomic))) {
    return(do.call(Account$new, x))
  }

  nested <- sapply(x, is.list)
  if (strict && !all(nested) && any(nested)) {
    stop('Will not convert a mixed list with both atomic and list elements. Use `strict=FALSE`.')
  }

  if (any(nested)) {
    x[nested] <- lapply(x[nested], as.Account.list, strict=strict)
  }

  return(x)
}


#' @describeIn Account Converts a Account object to a list.
#' @export
#' @param x A Account to check/convert.
as.list.Account <- function(x, ...) {
  x$ToList()
}

#' @describeIn Account Converts a Account object to a data.frame
#' @export
#' @param x An Account object or list of Account objects to convert.
as.data.frame.Account <- function(x, ...) {
  if (is.list(x)) {
    return(dplyr::bind_rows(lapply(x, as.list)))
  }
  dplyr::as_tibble(as.list(x))
}
