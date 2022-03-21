test_that('Budgets refer to same ynab connection', {
  ynab <- mock.YNAB()
  b1 <- as.Budget(test.budget.1(), ynab)
  expect_identical(b1$YNAB, ynab)
})

test_that('Budgets can be created from lists', {
  ynab <- mock.YNAB()

  l <- test.budget.1()

  b1 <- as.Budget(l, ynab)
  expect_s3_class(b1, 'Budget')

  expect_error(as.Budget(list(l,l), ynab, is.list=FALSE))

  bb <- as.Budget(list(l,l), ynab, is.list=TRUE)
  expect_equal(bb, list('test budget 1'=b1,'test budget 1.1'=b1))
})


