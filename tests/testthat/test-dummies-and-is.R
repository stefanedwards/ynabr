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
