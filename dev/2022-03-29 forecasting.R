## forecast balance

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



test_that('Forecasting monthly expenditure, but with slightly larger scheduled bill', {
  budget <- dummy.budget(scheduled_transactions = list(
    dummy.scheduled_transaction(
      '1', category_id='A', amount = -210,
      date_first = '2022-05-01', date_next = '2022-05-01',
      frequency='never')
  ),
  categories = list(
    dummy.category('A', goal_type='TBD', goal_target_month='2022-05-01', goal_target=500, balance=300)
  ))

  transactions <- build.scheduled.transactions(budget) %>%
    repeat.scheduled.transactions('2022-05-01') %>%
    mutate(
      outflow = if_else(amount < 0, -1*amount, NA_real_),
      inflow = if_else(amount >= 0, amount, NA_real_)
    ) %>%
    arrange(date_next)

  future.payments <- category.estimate.remaining.payments.TBD(bind_rows(budget$categories), current_month = '2022-03-28', include.target_month = TRUE)
  expect_equal(future.payments$payments, 2)
  expect_equal(future.payments$installment, 100)


})
