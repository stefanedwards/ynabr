library(mockery)

test_that('YNAB objects work', {
  ynab <- YNAB$new('123')

  expect_true(is.ynab(ynab))
  expect_false(is.ynab("123"))

  expect_false(is.null(ynab$BaseUrl))
})

test_that('stub works on bar', {
  ynab <- YNAB$new('123')
  stub(ynab$bar, 'foo.bar', 'more')
  expect_equal(ynab$bar(), 'more')
})

test_that('YNAB can load several budgets', {
  ynab <- YNAB$new('123')
  stub(ynab$Query, 'httr::GET', "X") #GET.budgets())
  ynab$load()
})

test_that('YNAB can load several budgets', {
  ynab <- YNAB$new('123')
  stub(ynab$load, 'self$Query', GET.budgets())
  ynab$load()
  expect_named(ynab$bu)
})
