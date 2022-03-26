expect_date <- function(object, expected) {
  expect_s3_class(object, 'Date')
  expect_equal(object, as.Date(expected))
}
test_that('Every type of frequency can be sequenced', {
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
