## Test that dummies and is-checks match up

test_that('Accounts', {
  a <- dummy.account()
  expect_type(a, 'list')
  expect_true(is.account(a))
  expect_true(is.account(as.data.frame(a)))

  b <- dummy.account(id = '1', approved = TRUE, foo=4)
  expect_equal(b$id, '1')
  expect_equal(b$approved, TRUE)
  expect_equal(b$foo, 4)
})

test_that('Budgets', {
  b <- load.test.budget()
  expect_type(b, 'list')
  expect_true(is.full_budget(b))

  b <- dummy.budget()
  expect_type(b, 'list')
  expect_true(is.budget(b))
  expect_true(is.full_budget(b))
})

test_that('Categories', {
  a <- dummy.category()
  expect_type(a, 'list')
  expect_true(is.category(a))
  expect_true(is.category(as.data.frame(a)))

  b <- dummy.category(id = '1', hidden = TRUE, foo = 4)
  expect_equal(b$id, '1')
  expect_equal(b$hidden, TRUE)
  expect_equal(b$foo, 4)
})

test_that('Scheduled transactions', {
  a <- dummy.scheduled_transaction()
  expect_type(a, 'list')
  expect_true(is.scheduled_transaction(a))
  expect_true(is.scheduled_transaction(as.data.frame(a)))

  b <- dummy.scheduled_transaction(id = '1', approved = TRUE, foo = 4)
  expect_equal(b$id, '1')
  expect_equal(b$approved, TRUE)
  expect_equal(b$foo, 4)
})

test_that('Scheduled subtransactions (split scheduled transactions)', {
  a <- dummy.scheduled_subtransaction()
  expect_type(a, 'list')
  expect_true(is.scheduled_subtransaction(a))
  expect_true(is.scheduled_subtransaction(as.data.frame(a)))

  b <- dummy.scheduled_subtransaction(id = '1', approved = TRUE, foo = 4)
  expect_equal(b$id, '1')
  expect_equal(b$approved, TRUE)
  expect_equal(b$foo, 4)
})

test_that('Transactions', {
  a <- dummy.transaction()
  expect_type(a, 'list')
  expect_true(is.transaction(a))
  expect_true(is.transaction(as.data.frame(a)))

  b <- dummy.transaction(id = '1', approved = TRUE, foo = 4)
  expect_equal(b$id, '1')
  expect_equal(b$approved, TRUE)
  expect_equal(b$foo, 4)
})

test_that('Subtransactions (split transactions)', {
  a <- dummy.subtransaction()
  expect_type(a, 'list')
  expect_true(is.subtransaction(a))
  expect_true(is.subtransaction(as.data.frame(a)))

  b <- dummy.subtransaction(id = '1', approved = TRUE, foo = 4)
  expect_equal(b$id, '1')
  expect_equal(b$approved, TRUE)
  expect_equal(b$foo, 4)
})
