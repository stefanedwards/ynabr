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
    dummy.transaction(id='127', account_id='B', transfer_account_id='C', category_id='W'),
    ## unrelated transfers below
    dummy.transaction(id='201', account_id='B', transfer_account_id='C', category_id='V'),
    dummy.transaction(id='202', account_id='B', transfer_account_id=NA_character_, category_id='V')
  )

  expect_equal(
    guess.debt.account.from.transfer(tr, c('Y','Z','W')) %>%
      arrange(category_id) %>% select(category_id, transfer_account_id),
    tribble(~category_id, ~transfer_account_id,
      'W','C',
      'Y','B',
      'Z','C'
    ) %>% as.data.frame
  )

  expect_warning(
    expect_equal(
      guess.debt.account.from.transfer(tr) %>%
        arrange(category_id) %>% select(category_id, transfer_account_id),
      tribble(~category_id, ~transfer_account_id,
              'V', NA,
              'W','C',
              'Y','B',
              'Z','C'
      )
    ),
    regexp = 'Some transfers point to none or multiple accounts in `guess.debt.account.from.transfer`!',
    fixed = TRUE
  )

  ## Single category used for multiple debt-accounts fails
  tr <- dplyr::bind_rows(
    dummy.transaction(id='123', account_id='A', transfer_account_id='B', category_id='Y'),
    dummy.transaction(id='124', account_id='A', transfer_account_id='C', category_id='Y')
  )
  expect_warning(
    expect_equal(
      guess.debt.account.from.transfer(tr) %>%
        arrange(category_id) %>% select(category_id, transfer_account_id),
      tribble(~category_id, ~transfer_account_id,
              'Y', NA_character_
      )
    ),
    regexp = 'Some transfers point to none or multiple accounts in `guess.debt.account.from.transfer`!',
    fixed = TRUE
  )

  ## but no warnings.
  expect_equal(
    guess.debt.account.from.transfer(tr, warnings=FALSE) %>%
      arrange(category_id) %>% select(category_id, transfer_account_id),
    tribble(~category_id, ~transfer_account_id,
            'Y', NA_character_
    )
  )

})
