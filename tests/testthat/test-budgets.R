test_that('Budgets refer to same ynab connection', {
  ynab <- mock.YNAB()
  b1 <- as.YnabBudget(test.budget.1(), ynab)
  expect_identical(b1$YNAB, ynab)
})

test_that('Budgets can be created from lists', {
  ynab <- mock.YNAB()

  l <- test.budget.1()

  b1 <- as.YnabBudget(l, ynab)
  expect_true(inherits(b1, 'YnabBudget'))

  expect_error(as.YnabBudget(list(l,l), ynab, is.list=FALSE))

  bb <- as.YnabBudget(list(l,l), ynab, is.list=TRUE)
  expect_equal(bb, list('test budget 1'=b1,'test budget 1.1'=b1))
})


