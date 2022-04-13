test_that('Estimate payments left on DEBT', {
  # 2 transfers from account A to account B, from funds in category Y
  # 2 transfers from account A to account C, from funds in category Z
  # 1 transfers from account B to account E, from funds in category W
  tr <- dplyr::bind_rows(
    dummy.transaction(id='123', account_id='A', transfer_account_id='B', category_id='Y'),
    dummy.transaction(id='124', account_id='A', transfer_account_id='B', category_id='Y'),
    dummy.transaction(id='125', account_id='A', transfer_account_id='C', category_id='Z'),
    dummy.transaction(id='126', account_id='A', transfer_account_id='C', category_id='Z'),
    dummy.transaction(id='127', account_id='B', transfer_account_id='E', category_id='W'),
    ## unrelated transfers below
    dummy.transaction(id='201', account_id='B', transfer_account_id='F', category_id='V'),
    dummy.transaction(id='202', account_id='B', transfer_account_id=NA_character_, category_id='V')
  )

  accounts <- list(
    A = dummy.account(id='A', balance=1000),
    B = dummy.account(id='B', balance=-500),
    C = dummy.account(id='C', balance=0),
    D = dummy.account(id='D', balance=1234),
    E = dummy.account(id='E', balance=-1000)
  )

  categories <- bind_rows(
    dummy.category(id='Y', goal_type='DEBT', balance=120, goal_target=20),
    dummy.category(id='Z', goal_type='DEBT', balance=0, goal_target=10),
    dummy.category(id='W', goal_type='DEBT', balance=100, goal_target=5),
    dummy.category(id='V', goal_type=NA_character_, balance=0, goal_target=10)
  )

  suppressWarnings(
  res <- category.estimate.remaining.payments.DEBT(categories, accounts, tr) %>%
    arrange(id) %>%
    select(id, transfer_account_id, category_balance, goal_target, remaining_balance, payments_left)
  )

  expect_equal(res, tribble(
    ~id, ~transfer_account_id, ~category_balance, ~goal_target, ~remaining_balance, ~payments_left,
    'W', 'E', 100,  5, -1000, 180,
    'Y', 'B', 120, 20,  -500,  19,
    'Z', 'C',   0, 10,     0,   0
  ))
})

test_that('Estimate number (and size) of payments of TBD (savings balance with date)', {
  categories <- bind_rows(
    dummy.category(id='A', goal_type='DEBT'),
    dummy.category(id='B'),
    # "new" category in march, nothing has been put towards it target.
    dummy.category(id='C', goal_type='TBD', balance=0, goal_target=3, budgeted=0, goal_target_month='2022-07-01'),
    # some monies already on category's balance, by nothing was been assigned to it this month.
    dummy.category(id='D', goal_type='TBD', balance=5, goal_target=8, budgeted=0, goal_target_month='2022-07-01'),
    # as 'C' above, but a transaction has drained the funds including those budgeted to the category
    dummy.category(id='E', goal_type='TBD', balance=0, goal_target=6, budgeted=4, goal_target_month='2022-07-01'),
    # old category with funds and additional funds budgeted towards it.
    dummy.category(id='F', goal_type='TBD', balance=5, goal_target=8, budgeted=1, goal_target_month='2022-07-01'),
    dummy.category()
  )

  res <- category.estimate.remaining.payments.TBD(categories, current_month='2022-03-18') %>%
    select(id, goal_type, budgeted, balance, goal_target, goal_target_month, first_month, payments, installment) %>%
    arrange(id)
  d2 <- as.Date('2022-04-01')
  expect_equal(res, tribble(
    ~id, ~goal_type, ~budgeted, ~balance, ~goal_target, ~goal_target_month, ~first_month, ~payments, ~installment,
    'C', 'TBD', 0, 0, 3, '2022-07-01', d2, 3, 1,
    'D', 'TBD', 0, 5, 8, '2022-07-01', d2, 3, 1,
    'E', 'TBD', 4, 0, 6, '2022-07-01', d2, 3, 2,
    'F', 'TBD', 1, 5, 8, '2022-07-01', d2, 3, 1
  ))

  categories$goal_target_month <- '2022-06-01'
  res <- category.estimate.remaining.payments.TBD(categories, current_month='2022-03-18', include.target_month=TRUE) %>%
    select(id, goal_type, budgeted, balance, goal_target, goal_target_month, first_month, payments, installment) %>%
    arrange(id)
  expect_equal(res, tribble(
    ~id, ~goal_type, ~budgeted, ~balance, ~goal_target, ~goal_target_month, ~first_month, ~payments, ~installment,
    'C', 'TBD', 0, 0, 3, '2022-06-01', d2, 3, 1,
    'D', 'TBD', 0, 5, 8, '2022-06-01', d2, 3, 1,
    'E', 'TBD', 4, 0, 6, '2022-06-01', d2, 3, 2,
    'F', 'TBD', 1, 5, 8, '2022-06-01', d2, 3, 1
  ))

})

test_that('Estimate number (and size) of payments of TBD (savings balance with date)', {
  categories <- bind_rows(
    dummy.category(id='A', goal_type='TBD'),
    dummy.category(id='B'),
    # "new" category in march, nothing has been put towards it target.
    dummy.category(id='C', goal_type='TBD', balance=0, goal_target=3, budgeted=0, goal_target_month='2022-07-01'),
    # some monies already on category's balance, by nothing was been assigned to it this month.
    dummy.category(id='D', goal_type='NEED', balance=5, goal_target=8, budgeted=0, goal_target_month='2022-07-01', goal_percentage_complete = 5/8*100),
    # as 'C' above, but a transaction has drained the funds including those budgeted to the category
    dummy.category(id='E', goal_type='TBD', balance=0, goal_target=6, budgeted=4, goal_target_month='2022-07-01'),
    # old category with funds and additional funds budgeted towards it.
    dummy.category(id='F', goal_type='TBD', balance=5, goal_target=8, budgeted=1, goal_target_month='2022-07-01'),
    dummy.category()
  )

})
