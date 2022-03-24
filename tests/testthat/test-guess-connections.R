# guess.debt.account.from.transfer -----

test_that('Get the debt account from a transfer/transactions', {
  ## transfer X monies FROM account A TO account B
  ## amount taken from category Y
  ## transfer X monies FROM account A TO account C, taken from category Z
  ## transfer X monies FROM account D TO account C, taken from category W
  tr <- dplyr::bind_rows(
    dummy.transaction(id='123', account_id='A', transfer_account_id='B', category_id='Y'),
    dummy.transaction(id='124', account_id='A', transfer_account_id='B', category_id='Y'),
    dummy.transaction(id='125', account_id='A', transfer_account_id='C', category_id='Z'),
    dummy.transaction(id='126', account_id='A', transfer_account_id='C', category_id='Z'),
    dummy.transaction(id='127', account_id='B', transfer_account_id='C', category_id='W')
  )

  res <-
  expect_equal(
    guess.debt.account.from.transfer(c('Y','Z','W'), tr),
    tribble(~category_id, ~transfer_account_id,
      'W','C',
      'Y','B',
      'Z','C'
    )
  )
})

test_that('Single category used for multiple debt-accounts fails', {
  ## transfer X monies FROM account A TO account B
  ## amount taken from category Y
  tr <- dplyr::bind_rows(
    dummy.transaction(id='123', account_id='A', transfer_account_id='B', category_id='Y'),
    dummy.transaction(id='124', account_id='A', transfer_account_id='C', category_id='Y')
  )
  expect_error(guess.debt.account.from.transfer('Y', tr))

})
