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

  res <- category.estimate.remaining.payments.DEBT(categories, accounts, tr) %>%
    arrange(id) %>%
    select(id, transfer_account_id, category_balance, goal_target, remaining_balance, payments_left)

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
    dummy.category(id='C', goal_type='TBD', balance=0, goal_target=4, budgeted=0, goal_target_month='2022-07-01'),
    dummy.category(id='D', goal_type='TBD', balance=4, goal_target=8, budgeted=0, goal_target_month='2022-07-01'),
    dummy.category(id='E', goal_type='TBD', balance=0, goal_target=8, budgeted=4, goal_target_month='2022-07-01'), ## if a previous activity has used 3, and then we budgeted 3.
    dummy.category()
  )

  res <- category.estimate.remaining.payments.TDB(categories, current_month='2022-03-18') %>%
    select(id, goal_type, budgeted, balance, goal_target, goal_target_month, first_month, payments, installment)

})
