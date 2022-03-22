test_that('Budgets refer to same ynab connection', {
  ynab <- mock.YNAB()
  b1 <- as.Budget(test.budget.1(), ynab)
  expect_identical(b1$YNAB, ynab)
})

test_that('Budgets are have setters/getters working', {
  now <- lubridate::now()
  b <- Budget$new(mock.YNAB(), '{123}', 'Test budget', now,
    first_month = '2022-03-01', last_month = '2022-04-01',
    date_format = list('DD-MM-YYYY'), currency_format = CurrencyFormat$new(iso_code='XX'),
    accounts = list(dummy.account())
  )

  expect_equal(b$Id, '{123}')
  expect_error(b$Id <- 1:3)
  b$Id <- 789
  expect_equal(b$Id, '789')

  expect_equal(b$Name, 'Test budget')
  expect_error(b$Name <- 1:2)
  b$Name <- 1
  expect_equal(b$Name, '1')

  expect_equal(b$LastModifiedOn, now)
  expect_error(b$LastModifiedOn <- 1)
  expect_error(b$LastModifiedOn <- '2020-02-02 02:02:02')
  now <- lubridate::now() + lubridate::minutes(5)
  b$LastModifiedOn <- now
  expect_equal(b$LastModifiedOn, now)

  expect_equal(b$CurrencyFormat, CurrencyFormat$new(iso_code='XX'))
  expect_error(b$CurrencyFormat <- NULL)
  cf <- CurrencyFormat$new(iso_code='DKK', symbol_first = TRUE)
  b$CurrencyFormat <- cf
  expect_equal(b$CurrencyFormat, cf)

  expect_equal(b$FirstMonth, as.Date('2022-03-01'))

  expect_equal(b$LastMonth, as.Date('2022-04-01'))

  expect_equal(b$DateFormat, 'DD-MM-YYYY')

  expect_equal(b$Accounts, list(dummy.account()))
  expect_error(b$Accounts <- now)
  expect_error(b$Accounts <- list(dummy.account(), now))
  a <- dummy.account()
  a$Name <- 'Huzzah!'
  b$Accounts <- a
  expect_equal(b$Accounts, list(a))
  b$Accounts[[1]]$Balance <- 55
  expect_equal(b$Accounts[[1]]$Balance, 55)


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

test_that('Accounts and GetAccountById works as intended', {
  a1 <- dummy.account()
  a1$Id <- a1$Name <- 'A1'
  a2 <- dummy.account()
  a2$Id <- a2$Name <- 'A2'
  a3 <- dummy.account()
  a3$Id <- a3$Name <- 'A3'

  b <- test.budget.1()
  b$accounts <- list(a1,a2,a3)
  b <- as.Budget(b, mock.YNAB())

  expect_length(b$Accounts, 3)
  expect_named(b$Accounts, c('A1','A2','A3'))
  expect_equal(b$GetAccountById('A2'), a2)
  expect_equal(b$GetAccountById('A1'), a1)
  expect_equal(b$GetAccountById('A3'), a3)

  a4 <- dummy.account()
  a4$Id <- a4$Name <- 'A4'
  b$Accounts[[4]] <- a4
  expect_equal(b$GetAccountById('A4'), a4)
})


