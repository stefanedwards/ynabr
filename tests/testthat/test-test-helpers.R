## test test-helpers

test_that('Mocked YNAB does not engage a query', {
  ynab <- mock.YNAB()
  expect_error(ynab$Query(), 'Query disabled in mock')
})

test_that("Mocked YNAB's Query is replaced correctly", {
  ynab <- mock.YNAB(Query = 'hello world')
  expect_equal(ynab$Query(), 'hello world')

  ynab <- mock.YNAB(Query = function(x) {x + 1})
  expect_equal(ynab$Query(2), 3)
})
