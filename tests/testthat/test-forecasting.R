

test_that('Creating "background grid" of all months between now and then', {
  x <- expand.months('2022-02-01', '2022-06-01')
  expect_equal(nrow(x), 5)
})

