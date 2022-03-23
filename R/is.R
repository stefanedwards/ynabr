

#' Checks that json-lists have required fields
#'
#' @rdname is
#' @importFrom assertthat has_name
#' @export
is.account <- function(x) {
  has_name(x, c('id','name','type','on_budget','closed','balance','cleared_balance','uncleared_balance','transfer_payee_id','deleted'))
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

## TODO: Test !

#' @rdname is
#' @importFrom assertthat has_name
#' @export
is.transaction <- function(x) {
  has_name(x, c("id", "date", "amount", "cleared", "approved", "account_id",
                "payee_id", "deleted", "memo", "category_id", "transfer_account_id",
                "transfer_transaction_id")
  )
}

