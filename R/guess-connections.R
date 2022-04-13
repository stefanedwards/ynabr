## Guess connections between accounts, transactions and categories


#' Guesses which account is the destination of a transfer.
#'
#' @include dummies-and-is.R
#' @importFrom assertthat assert_that
#' @importFrom rlang is_atomic is_missing
#' @import dplyr
#' @importFrom tidyr nest unnest
#' @return Data.frame which maps \code{category_id} to the account (\code{transfer_account_id}).
#' @param transactions List or data.frame of transactions.
#' @param category_id Optional, result is reduced to these categories.
#' @param warnings Logical, gives warnings
guess.debt.account.from.transfer <- function(transactions, category_id, warnings=TRUE) {
  if (is.list(transactions))
    transactions <- bind_rows(transactions)
  assert_that(is.data.frame(transactions), is.transaction(transactions))
  if (!is_missing(category_id))
    assert_that(is_atomic(category_id))

  x <- transactions %>%
    count(category_id, transfer_account_id) %>%
    tidyr::nest(transfers=c(transfer_account_id, n)) %>%
    mutate(transfer_account_id = purrr::map_chr(transfers, ~{if (nrow(.) == 1) .$transfer_account_id else NA}))

  if (!is_missing(category_id))
    x <- left_join(data.frame(category_id = category_id), x, by='category_id')

  if (warnings && is.na(x$transfer_account_id))
    warning('Some transfers point to none or multiple accounts in `guess.debt.account.from.transfer`!')

  x
}
