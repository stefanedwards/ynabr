

test_that('Creating "background grid" of all months between now and then', {
  expect_equal(
    expand.months('2022-02-01', '2022-06-01'),
    tibble(month=as.Date(c('2022-02-01','2022-03-01','2022-04-01','2022-05-01','2022-06-01')), category_id = NA_character_)
  )


  expect_equal(
    expand.months('2022-02-01','2022-02-01', category_id = 'beer'),
    tibble(month=as.Date('2022-02-01'), category_id = 'beer')
  )

  expect_equal(
    expand.months('2022-02-01','2022-02-03', category_id = 'beer', frequency = 'daily'),
    tibble(month=as.Date(c('2022-02-01','2022-02-02','2022-02-03')), category_id = 'beer')
  )

  ## end_date before start_date
  skip("Un-decided what happens when end_date before start_date.")
  expect_equal(
    expand.months('2022-02-01','2022-01-01', category_id = 'beer'),
    tibble(month=as.Date('2022-02-01'), category_id = 'beer')
  )
})


## NEED, DEBT, TBD, TD, MF
test_that('Forecasting expenditures for a monthly NEED.', {
  categories <- list(
    dummy.category('1', name = 'Groceries', goal_type = 'NEED', goal_target = 10, balance = 9)
  ) %>% bind_rows

  expect_equal(
    forecast.category.expenditure.NEED(categories, '2022-03-01', '2022-05-01', 'monthly'),
    tibble(
      month=as.Date(c('2022-03-01','2022-04-01','2022-05-01')),
      id = '1',
      to_budget = c(1.0, 10, 10)
    ))

  ## a NEED with a goal_target_date
  categories <- bind_rows(categories,
    dummy.category('2', goal_type = 'NEED', goal_target=5, balance=0, goal_target_month='2022-04-02')
  )
  expect_equal(
    forecast.category.expenditure.NEED(categories, '2022-03-01','2022-05-01','monthly') %>%
      arrange(id, month),
    tibble(
      month = as.Date(c('2022-03-01','2022-04-01','2022-05-01','2022-03-01','2022-04-01')),
      id = c('1','1','1','2','2'),
      to_budget = c(1, 10, 10, 5, 5)
    )
  )

  ## weekly spending cannot be tested, as it is exported as a MONTHLY spending!

  skip('Test that doesn\'t belong here.')
  ## a scheduled transaction, but its total is less than the goal_target,
  ## is simply included and ignored.
  # st <- list(
  #   dummy.scheduled_transaction('A', date_first = '2022-04-01', date_next='2022-04-01', amount=7, category_id='1')
  # ) %>% as.data.frame()
  ## not here -- this part belongs to a higher level, e.g. forecast.category.expenditures (for all types)

})

test_that('Forecasting expenditures for a DEBT category.', {
  categories <- list(
    dummy.category('1', name='Debt 1', goal_type = 'DEBT', goal_target = 10, balance = 0),
    dummy.category('2', name='Debt 2', goal_type = 'DEBT', goal_target = 10, balance = 5),
    dummy.category('3', name='Debt 3', goal_type = 'DEBT', goal_target = 10, balance = 0)
  ) %>% bind_rows()
  accounts <- list(
    A = dummy.account('A', balance = -200),
    B = dummy.account('B', balance = -15),
    C = dummy.account('C', balance = -15)
  )
  transactions <- list(
    dummy.transaction('11', account_id='X', transfer_account_id='A', category_id='1'),
    dummy.transaction('12', account_id='X', transfer_account_id='A', category_id='1'),
    dummy.transaction('21', account_id='X', transfer_account_id='B', category_id='2'),
    dummy.transaction('31', account_id='X', transfer_account_id='C', category_id='3')
  ) %>% bind_rows

  category.estimate.remaining.payments.DEBT(categories, accounts, transactions)
})




test_that('Forcasting expenditure from a bi-monthly, scheduled transaction', {
  budget <- dummy.budget(scheduled_transactions = list(
    dummy.scheduled_transaction(
      '1', category_id='A', amount = -10,
      date_first = '2022-01-01', date_next = '2022-03-01',
      frequency='everyOtherMonth')
  ))
  transactions <- build.scheduled.transactions(budget) %>%
    repeat.scheduled.transactions('2022-06-01') %>%
    mutate(
      outflow = if_else(amount < 0, -1*amount, NA_real_),
      inflow = if_else(amount >= 0, amount, NA_real_)
    ) %>%
    arrange(date_next)

  expect_equal(transactions$date_next, as.Date(c('2022-03-01','2022-05-01')))
  expect_true(all(is.na(transactions$inflow)))

  all.months <- expand.months('2022-03-01','2022-06-01') %>%
    year.month('month', check.names=FALSE) %>%
    full_join(year.month(transactions, 'date_next'), by=c('month','year'))

})
