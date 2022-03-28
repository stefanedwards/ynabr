expect_date <- function(object, expected) {
  expect_s3_class(object, 'Date')
  expect_equal(object, as.Date(expected))
}
test_that('Every type of frequency can be sequenced', {
  expect_date(
    .repeat.dates('2022-02-28','2022-01-01','never'),
    '2022-02-28'
  )
  expect_date(
    .repeat.dates('2022-02-26', '2022-03-02', 'daily'),
    c('2022-02-26','2022-02-27','2022-02-28','2022-03-01','2022-03-02')
  )
  expect_date(
    .repeat.dates('2022-02-26', '2022-03-17', 'weekly'), #less than 4 whole weeks!
    c('2022-02-26','2022-03-05','2022-03-12')
  )
  expect_date(
    .repeat.dates('2022-02-26', '2022-03-31', 'everyOtherWeek'), #less than 5 whole weeks!
    c('2022-02-26','2022-03-12','2022-03-26')
  )
  expect_date(
    .repeat.dates('2022-02-26', '2022-05-15', 'twiceAMonth'),
    c('2022-02-26','2022-03-13','2022-03-26','2022-04-13','2022-04-26','2022-05-13')
  )
  expect_date(
    .repeat.dates('2022-02-26', '2022-06-01', 'every4Weeks'),
    c('2022-02-26','2022-03-26','2022-04-23','2022-05-21')
  )
  expect_date(
    .repeat.dates('2022-01-31', '2022-04-01', 'monthly'),
    c('2022-01-31','2022-02-28','2022-03-31')
  )
  expect_date(
    .repeat.dates('2022-01-31', '2022-04-01', 'everyOtherMonth'),
    c('2022-01-31','2022-03-31')
  )
  expect_date(
    .repeat.dates('2022-01-31', '2022-08-01', 'every3Months'),
    c('2022-01-31','2022-04-30','2022-07-31')
  )
  expect_date(
    .repeat.dates('2022-01-31', '2022-10-01', 'every4Months'),
    c('2022-01-31','2022-05-31','2022-09-30')
  )
  expect_date(
    .repeat.dates('2022-02-28', '2023-02-28', 'twiceAYear'),
    c('2022-02-28','2022-08-28','2023-02-28')
  )
  expect_date(
    .repeat.dates('2022-02-28', '2025-02-28', 'yearly'),
    c('2022-02-28','2023-02-28','2024-02-28','2025-02-28')
  )
  expect_date(
    .repeat.dates('2022-02-28', '2026-02-28', 'everyOtherYear'),
    c('2022-02-28','2024-02-28','2026-02-28')
  )
})

test_that('Repeat dates, edge case', {
  expect_date(
    .repeat.dates('2022-03-01','2022-03-01', 'everyOtherYear'),
    '2022-03-01'
  )
})

test_that('Vectorized date repeater works', {
  res <- repeat.dates(c('2022-03-01','2022-03-02'), '2022-03-16', c('weekly','everyOtherWeek'))
  expect_date(res[[1]], c('2022-03-01','2022-03-08','2022-03-15'))
  expect_date(res[[2]], c('2022-03-02','2022-03-16'))
})

test_that('Merging scheduled transactions with its split transactions work', {
  budget <- load.test.budget()
  scheduled <- dplyr::bind_rows(budget$scheduled_transactions)
  res <- build.scheduled.transactions(budget)
  expect_true(is.scheduled_transaction(res))
  expect_equal(sum(res$amount), sum(scheduled$amount))
  expect_false(any(is.na(res$payee_id)))

  budget <- dummy.budget(
    scheduled_transactions = list(
      dummy.scheduled_transaction('1', amount=5, payee_id='foo', frequency='monthly'), ## not split,
      dummy.scheduled_transaction('2', amount=10, payee_id='bar', frequency='daily'), ## split normal (later on)
      dummy.scheduled_transaction('3', amount=10, frequency='weekly'), ## split transfer
      dummy.scheduled_transaction('4', amount=10, payee_id='zoo', frequency='never')  ## combo split (e.g. getting withdrawal with a purchase)
    )
  )
  res <- build.scheduled.transactions(budget) %>%
    arrange(id)
  expect_true(is.scheduled_transaction(res))
  expect_equal(nrow(res), length(budget$scheduled_transactions))
  expect_equal(res$frequency, c('monthly','daily','weekly','never'))

  budget$scheduled_subtransactions <- list(
    dummy.scheduled_subtransaction('2a', amount=3, memo='split part 1', scheduled_transaction_id='2'),
    dummy.scheduled_subtransaction('2b', amount=7, memo='split part 2', scheduled_transaction_id='2'),
    dummy.scheduled_subtransaction('3a', amount=4, memo='split part 1', scheduled_transaction_id='3', transfer_account_id='A'),
    dummy.scheduled_subtransaction('3b', amount=6, memo='split part 2', scheduled_transaction_id='3', transfer_account_id='B'),
    dummy.scheduled_subtransaction('4a', amount=8, memo='purchase', scheduled_transaction_id='4'),
    dummy.scheduled_subtransaction('4b', amount=2, memo='change to wallet', scheduled_transaction_id='4', transfer_account_id='C')
  )
  res <- build.scheduled.transactions(budget) %>%
    arrange(id)
  expect_true(is.scheduled_transaction(res))
  expect_equal(nrow(res), length(budget$scheduled_subtransactions)+1)
  res %>% filter(scheduled_transaction_id == '2') %>%
    select(id, amount, payee_id, frequency) %>%
    expect_equal(tribble(
      ~id, ~amount, ~payee_id, ~frequency,
      '2a', 3, 'bar', 'daily',
      '2b', 7, 'bar', 'daily'
    ))
  res %>% filter(scheduled_transaction_id == '3') %>%
    select(id, amount, payee_id, frequency, transfer_account_id) %>%
    expect_equal(tribble(
      ~id, ~amount, ~payee_id, ~frequency, ~transfer_account_id,
      '3a', 4, NA_character_, 'weekly', 'A',
      '3b', 6, NA_character_, 'weekly', 'B'
    ))
  res %>% filter(scheduled_transaction_id == '4') %>%
    select(id, amount, payee_id, frequency, transfer_account_id) %>%
    expect_equal(tribble(
      ~id, ~amount, ~payee_id, ~frequency, ~transfer_account_id,
      '4a', 8, 'zoo', 'never', NA_character_,
      '4b', 2, 'zoo', 'never', 'C'
    ))

})
