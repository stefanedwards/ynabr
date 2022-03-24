## Guess connections between accounts, transactions and categories


#' Guesses which account is the destination of a transfer.
#'
#' @include dummies-and-is.R
#' @importFrom assertthat assert_that
#' @importFrom rlang is_character
#' @import dplyr
#' @return Data.frame which maps \code{category_id} to the account (\code{transfer_account_id}).
guess.debt.account.from.transfer <- function(category_id, transactions) {
  if (is.list(transactions))
    transactions <- bind_rows(transactions)
  assert_that(is_character(category_id), is.data.frame(transactions), is.transaction(transactions))
  x <- transactions %>% filter(category_id %in% !!category_id) %>%
    group_by(category_id) %>%
    count(transfer_account_id)
  category_count <- x %>% count(category_id, name='nn') %>% pull('nn')
  assert_that(
    nrow(x) == length(category_id),
    all(category_count == 1)
  )
  x %>% ungroup %>% select(category_id, transfer_account_id)
}
