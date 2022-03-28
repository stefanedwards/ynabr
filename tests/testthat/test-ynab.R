test_that('YNAB objects work', {
  ynab <- YNAB$new('123')

  expect_true(is.ynab(ynab))
  expect_false(is.ynab("123"))

  expect_false(is.null(ynab$BaseUrl))
})

test_that('YNAB can load several budgets', {
  ynab <- mock.YNAB(Query=function(...) { list(budgets=list(load.test.budget())) })
  l <- ynab$load()$Budgets
  expect_equal(length(l), 1)
  expect_true(is.budget(l[[1]]))
})
