# scheduled transaction
#' @include 0_aux.R
NULL

YnabScheduledTransaction <- R6::R6Class("YnabScheduledTransaction",
private = list(
  id = NULL,
  date_first = NULL,
  date_next = NULL,
  frequency = NULL,
  amount = NULL,
  memo = NULL,
  flag_color = NULL,
  account_id = NULL,
  payee_id = NULL,
  category_id = NULL,
  transfer_account_id = NULL,
  deleted = NULL,
  account_name = NULL,
  payee_name = NULL,
  category_name = NULL,
  subtransactions = list()
),
active = list(
  #' @field Id YNAB's internal ID.
  Id = function() { private$id },
  #' @field DateFirst
  #' The first date for which the Scheduled Transaction was scheduled.
  DateFirst = function() { private$date_first },
  #' @field DateNext
  #' The *next* date for which the Scheduled Transaction is scheduled.
  DateNext = function() { private$date_next },
  #' @field Frequency,Amount
  #' How often the Scheduled Transaction is repeated.
  Frequency = function() { private$frequency },
  Amount = function() { private$amount },
  #' @field memo String
  Memo = function() { private$memo },
  #' @field FlagColor
  #' The Scheduled Transaction's flag.
  FlagColor = function() { private$flag_color },
  #' @field Deleted
  #' Whether or not the Scheduled Transaction has been deleted.
  #' Deleted scheduled transaction will only be included in delta requests.
  Deleted = function() { private$deleted },
  #' @field AccountName String of account name.
  AccountName = function() { private$account_name },
  #' @field PayeeName String of payee's name.
  PayeeName = function() { private$payee_name },
  #' @field CategoryName String of category name.
  CategoryName = function() { private$category_name }
)
)
