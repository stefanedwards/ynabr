list.budgets <- function(token, base, include.accounts=FALSE) {
  include.account <- as.logical(include.accounts)
  assertthat::assert_that(is_logical(include.account, n=1))
  r <- httr::GET(
    paste0(base, 'budgets'),
    query=list(include_accounts=include.accounts),
    httr::timeout(5),
    httr::add_headers(token_as_h(token))
  )

}

token_as_h <- function(token) {
  assertthat::assert_that(is_character(token, n=1))
  c('Authorization'=paste('Bearer', token))
}
