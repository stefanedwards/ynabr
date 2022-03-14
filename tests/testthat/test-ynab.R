
test_that('YNAB objects work', {
  ynab <- YNAB$new('123')

  expect_true(is.ynab(ynab))
  expect_false(is.ynab(ynab$AccessToken))

  expect_false(is.null(ynab$BaseUrl))
})
