

#' Checks that json-lists have required fields
#'
#' @rdname is
#' @importFrom assertthat has_name
#' @export
is.account <- function(x) {
  has_name(x, c('id','name','type','on_budget','closed','balance','cleared_balance','uncleared_balance','transfer_payee_id','deleted'))
}

#' Dummy YNAB objects for testing
#'
#' Returns simple list objects with reasonable default values, mostly \code{NA},
#' with some exceptions (deleted defaults to \code{FALSE}, memo to \code{""}, etc).
#'
#' @rdname dummies
#' @keywords internal
#' @export
#' @param id Id of entry.
#' @param ... Named values to use in your dummy object.
dummy.account <- function(id = NA_character_, ...) {
  default <- list(
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
  merge.list(list(id=id, ...), default)
}

#' @rdname is
#' @importFrom assertthat has_name
#' @export
is.budget <- function(x) {
  has_name(x,
    c('id','name','last_modified_on','first_month','last_month','date_format','currency_format','accounts')
  )
}

#' @rdname is
#' @importFrom assertthat has_name
#' @export
is.full_budget <- function(x) {
  is.budget(x) &&
  has_name(x, c(
    'payees','payee_locations','category_groups','categories','months',
    'transactions','subtransactions','scheduled_transactions','scheduled_subtransactions'
  ))
}

#' @rdname dummies
#' @importFrom assertthat assert_that
#' @export
#' @param name,last_modified_on,first_month,last_month,date_format,currency_format,accounts,payees,payee_locations,category_groups,categories,months,transactions,subtransactions,scheduled_transactions,scheduled_subtransactions
#'   Every element in the budget. See their default values for hints as to what they are.
dummy.budget <- function(id=NA_character_, name=NA_character_, last_modified_on=now(),
  first_month=NA_character_, last_month=NA_character_, date_format=list(format='DD-MM-YYYY'),
  currency_format=dk.currency.format(), accounts=list(),
  payees=list(), payee_locations=list(), category_groups=list(), categories=list(),
  months=list(),
  transactions=list(), subtransactions=list(),
  scheduled_transactions=list(), scheduled_subtransactions=list()) {

  last_modified_on = format_ISO8601(as_datetime(last_modified_on), usetz=TRUE)

  if (is_single_string(date_format, TRUE))
    date_format = list(format = date_format)

  assert_that(
    is_single_string(id, TRUE),
    is_single_string(name, TRUE),
    is_single_string(last_modified_on, TRUE),
    is_single_string(first_month, TRUE),
    is_single_string(last_month, TRUE),
    rlang::is_bare_list(date_format, n=1),
    is_single_string(date_format$format, TRUE),
    rlang::is_bare_list(currency_format, n=8),
    is.list(accounts),
    is.list(payees),
    is.list(payee_locations),
    is.list(category_groups),
    is.list(categories),
    is.list(months),
    is.list(transactions),
    is.list(subtransactions),
    is.list(scheduled_transactions),
    is.list(scheduled_subtransactions)
  )

  list(
    id=id,
    name=name,
    last_modified_on=last_modified_on,
    first_month=first_month,
    last_month=last_month,
    date_format=date_format,
    currency_format=currency_format,
    accounts=accounts,
    payees=payees,
    payee_locations=payee_locations,
    category_groups=category_groups,
    categories=categories,
    months=months,
    transactions=transactions,
    subtransactions=subtransactions,
    scheduled_transactions=scheduled_transactions,
    scheduled_subtransactions=scheduled_subtransactions
  )
}




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
is.scheduled_transaction <- function(x) {
  has_name(x,
  c("id", "date_first", "date_next", "frequency", "amount", "memo", "flag_color",
   "account_id", "payee_id", "category_id", "deleted", "transfer_account_id")
  )
}

#' @rdname dummies
#' @export
dummy.scheduled_transaction <- function(id=NA_character_, ...) {
  default <- list(
    date_first = NA_character_,
    date_next = NA_character_,
    frequency = NA_character_,
    amount = NA_integer_,
    memo = "",
    flag_color = NA_integer_,
    account_id = NA_character_,
    payee_id = NA_character_,
    category_id = NA_character_,
    deleted = FALSE,
    transfer_account_id = NA_character_
  )
  merge.list(list(id=id, ...), default)
}

#' @rdname  is
#' @importFrom assertthat has_name
#' @export
is.scheduled_subtransaction <- function(x) {
  has_name(x,
     c("id", "scheduled_transaction_id", "amount", "memo", "payee_id",
       "category_id", "transfer_account_id", "deleted")
   )
}

#' @rdname dummies
#' @export
dummy.scheduled_subtransaction <- function(id=NA_character_, ...) {
  default <- list(
    scheduled_transaction_id = NA_character_,
    amount = NA_integer_,
    memo = "",
    payee_id = NA_character_,
    category_id = NA_character_,
    transfer_account_id = NA_character_,
    deleted = FALSE
  )
  merge.list(list(id=id, ...), default)
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
dummy.transaction <- function(id=NA_character_, ...) {
  default <- list(
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
  merge.list(list(id=id, ...), default)
}

#' @rdname is
#' @importFrom assertthat has_name
#' @export
is.subtransaction <- function(x) {
  has_name(x, c(
    "id", "transaction_id", "amount", "memo", "payee_id", "category_id",
    "transfer_account_id", "deleted"
  ))
}

#' @rdname dummies
#' @export
dummy.subtransaction <- function(id=NA_character_, ...) {
  default <- list(
    transaction_id = NA_character_,
    amount = NA_integer_,
    memo = "",
    payee_id = NA_character_,
    category_id = NA_character_,
    transfer_account_id = NA_character_,
    deleted = FALSE
  )
  merge.list(list(id=id, ...), default)
}
