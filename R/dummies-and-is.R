

#' Checks that json-lists have required fields
#'
#' @rdname is
#' @importFrom assertthat has_name
#' @export
is.account <- function(x) {
  has_name(x, c('id','name','type','on_budget','closed','balance','cleared_balance','uncleared_balance','transfer_payee_id','deleted'))
}

#' Dummy objects
#'
#' @rdname dummies
#' @export
#' @param ... Named/unnamed values to use in your dummy object.
dummy.account <- function(...) {
  default <- list(
    id = NA_character_,
    name = NA_character_,
    type = NA_character_,
    on_budget = NA,
    closed = FALSE,
    note = NA_character_,
    balance = NA_integer_,
    cleared_balance = NA_integer_,
    uncleared_balance = NA_integer_,
    transfer_payee_id = NA_character_,
    deleted = FALSE
  )
  merge.list(list(...), default)
}

#' @rdname is
#' @importFrom assertthat has_name
#' @export
is.budget <- function(x) {
  has_name(x,
    c('id','name','last_modified_on','first_month','last_month','date_format','currency_format','accounts')
  )
}
## TODO: Dummy budget


#' @rdname is
#' @importFrom assertthat has_name
#' @export
is.category <- function(x) {
  has_name(x, c("id", "category_group_id", "name", "hidden", "original_category_group_id",
                "note", "budgeted", "activity", "balance", "goal_type", "goal_creation_month",
                "goal_target", "goal_target_month", "goal_percentage_complete",
                "deleted")
  )
}

#' @rdname dummies
#' @export
#' @param ... Named/unnamed values to use in your dummy object.
dummy.category <- function(...) {
  default <- list(
    id = NA_character_,
    category_group_id = NA_character_,
    name = NA_character_,
    hidden = FALSE,
    type = NA_character_,
    original_category_group_id = NA_character_,
    note = NA_character_,
    budgeted = NA_integer_,
    activity = NA_integer_,
    balance = NA_integer_,
    goal_type = NA_character_,
    goal_creation_month = NA_character_,
    goal_target = NA_integer_,
    goal_target_month = NA_character_,
    goal_percentage_complete = NA_integer_,
    deleted = FALSE
  )
  merge.list(list(...), default)
}



#' @rdname is
#' @importFrom assertthat has_name
#' @export
is.transaction <- function(x) {
  has_name(x, c("id", "date", "amount", "cleared", "approved", "account_id",
                "payee_id", "deleted", "memo", "category_id", "transfer_account_id",
                "transfer_transaction_id")
  )
}

#' @rdname dummies
#' @export
dummy.transaction <- function(...) {
  default <- list(
    id = NA_character_,
    date = as.character(Sys.Date()),
    amount = NA_integer_,
    memo = '',
    cleared = NA_character_,
    approved = FALSE,
    account_id = NA_character_,
    payee_id = NA_character_,
    category_id = NA_character_,
    deleted = FALSE,
    transfer_account_id = NA_character_,
    transfer_transaction_id = NA_character_
  )
  merge.list(list(...), default)
}


#' @rdnae is
#' @importFrom assertthat has_name
#' @export
is.scheduled_transaction <- function(x) {
  has_name(x,
  c("id", "date_first", "date_next", "frequency", "amount", "memo",
   "account_id", "payee_id", "category_id", "deleted", "transfer_account_id")
  )
}

#' @rdname dummies
#' @export
dummy.transaction <- function(id=NA_character, ...) {
  default <- list(
    date_first = NA_character_,
    date_next = NA_character_,
    frequency = NA_character_,
    amount = NA_int_,
    memo = "",
    account_id = NA_character_,
    payee_id = NA_character_,
    category_id = NA_character_,
    deleted = FALSE,
    transfer_account_id = NA_character_
  )
  merge.list(list(id=id, ...), default)
}
