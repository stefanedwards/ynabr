test_that('Accounts can be created', {
  a <- Account$new('test', 'Test', 123.45, 100, is_milliunits = FALSE)
  expect_false(a$IsCleared)
  a$ClearedBalance <- 123.45
  expect_true(a$IsCleared)

  b <- Account$new('test','Test', balance = 1230, cleared_balance = 456,
    uncleared_balance = 555,
    type = 'cash', on_budget = FALSE,
    note = 'note', closed = TRUE,
    transfer_payee_id = 123,
    direct_import_linked = TRUE,
    direct_import_in_error = FALSE,
    deleted = TRUE,
    is_milliunits = TRUE
  )
  expect_equal(b$Id, 'test')
  expect_equal(b$Name, 'Test')
  expect_equal(b$Balance, 1230)
  expect_equal(b$ClearedBalance, 456)
  expect_equal(b$UnclearedBalance, 555)
  expect_equal(b$Type, 'cash')
  expect_false(b$OnBudget)
  expect_equal(b$Note, 'note')
  expect_true(b$Closed)
  expect_equal(b$TransferPayeeId, '123')
  expect_true(b$DirectImportLinked)
  expect_false(b$DirectImportInError)
  expect_true(b$Deleted)
  expect_true(b$IsMilliunits)
})

test_that('Accounts getters/setters works', {
  a <- Account$new('test', 'Test', 123.45, 100, is_milliunits = FALSE)

  expect_error(a$Id <- list())
  expect_error(a$Id <- c('a','b'))
  a$Id <- 1234
  expect_equal(a$Id, '1234')

  expect_error(a$Name <- list())
  expect_error(a$Name <- c('a','b'))
  a$Name <- 'foo'
  expect_equal(a$Name, 'foo')

  expect_error(a$Type <- 123)
  expect_error(a$Type <- 'wah wah')
  expect_error(a$Type <- c('a','b'))
  a$Type <- 'cash'
  expect_equal(a$Type, 'cash')

  expect_error(a$OnBudget <- 'bar')
  expect_error(a$OnBudget <- c(TRUE,FALSE))
  a$OnBudget <- 'TRUE'
  expect_true(a$OnBudget)
  a$OnBudget <- 0
  expect_false(a$OnBudget)

  expect_error(a$Closed <- 'bar')
  expect_error(a$Closed <- c(TRUE,FALSE))
  a$Closed <- 'TRUE'
  expect_true(a$Closed)
  a$Closed <- 0
  expect_false(a$Closed)

  expect_error(a$Note <- list())
  expect_error(a$Note <- c('a','b'))
  a$Note <- 1234
  expect_equal(a$Note, '1234')
  a$Note <- NA
  expect_equal(a$Note, NA_character_)

  expect_error(a$Balance <- 'bar')
  expect_error(a$Balance <- 1:3)
  a$Balance <- '123'
  expect_equal(a$Balance, 123)

  expect_error(a$ClearedBalance <- 'bar')
  expect_error(a$ClearedBalance <- 1:3)
  a$ClearedBalance <- '123'
  expect_equal(a$ClearedBalance, 123)

  expect_error(a$UnclearedBalance <- 'bar')
  expect_error(a$UnclearedBalance <- 1:3)
  a$UnclearedBalance <- '123'
  expect_equal(a$UnclearedBalance, 123)

  expect_error(a$TransferPayeeId <- list())
  expect_error(a$TransferPayeeId <- c('a','b'))
  a$TransferPayeeId <- 1234
  expect_equal(a$TransferPayeeId, '1234')
  a$TransferPayeeId <- NA
  expect_equal(a$TransferPayeeId, NA_character_)

  expect_error(a$DirectImportLinked <- 'bar')
  expect_error(a$DirectImportLinked <- c(TRUE,FALSE))
  a$DirectImportLinked <- 'TRUE'
  expect_true(a$DirectImportLinked)
  a$DirectImportLinked <- 0
  expect_false(a$DirectImportLinked)

  expect_error(a$DirectImportInError <- 'bar')
  expect_error(a$DirectImportInError <- NA)
  expect_error(a$DirectImportInError <- c(TRUE,FALSE))
  a$DirectImportInError <- 'TRUE'
  expect_true(a$DirectImportInError)
  a$DirectImportInError <- 0
  expect_false(a$DirectImportInError)

  expect_error(a$Deleted <- 'bar')
  expect_error(a$Deleted <- NA)
  expect_error(a$Deleted <- c(TRUE,FALSE))
  a$Deleted <- 'TRUE'
  expect_true(a$Deleted)
  a$Deleted <- 0
  expect_false(a$Deleted)
})

test_that('Flipping milliunits back and forth works', {
  a <- Account$new('test', 'Test', 123.45, 100, is_milliunits = FALSE)
  expect_false(a$IsMilliunits)
  expect_false(a$IsCleared)
  expect_equal(a$Balance, 123.45)
  expect_equal(a$ClearedBalance, 100)
  expect_equal(a$UnclearedBalance, 23.45)

  expect_equal(a$AsCurrency, c(123.45, 100.0, 23.45), ignore_attr =TRUE)
  expect_named(a$AsCurrency, c('balance','cleared','uncleared'))
  expect_equal(a$AsMilliunits, c(123450, 100000, 23450), ignore_attr =TRUE)
  expect_named(a$AsMilliunits, c('balance','cleared','uncleared'))

  a$IsMilliunits <- TRUE
  expect_true(a$IsMilliunits)
  expect_false(a$IsCleared)
  expect_equal(a$Balance, 123450)
  expect_equal(a$ClearedBalance, 100000)
  expect_equal(a$UnclearedBalance, 23450)

  expect_equal(a$AsCurrency, c(123.45, 100.0, 23.45), ignore_attr =TRUE)
  expect_named(a$AsCurrency, c('balance','cleared','uncleared'))
  expect_equal(a$AsMilliunits, c(123450, 100000, 23450), ignore_attr =TRUE)
  expect_named(a$AsMilliunits, c('balance','cleared','uncleared'))
})


test_that('Printing accounts', {
  a <- Account$new('test', 'Test', 123.45, 100, is_milliunits = FALSE)
  expect_output(print(a), 'Account Test (id test)', fixed=TRUE)
  expect_output(
    print(a, as.CurrencyFormat(dk.currency.format())),
    'Account Test (id test)\nBalance: 123,45kr. (100,00kr. cleared)',
    fixed=TRUE
  )
})


test_that('Generics works', {
  a <- Account$new('test', 'Test', 123.45, 100, is_milliunits = FALSE)
  expect_equal(as.Account(as.list(a)), a)

  l1 <- list(id='test', name='Test', balance=123.45, cleared_balance=100, is_milliunits=FALSE)
  expect_equal(as.Account(l1), a)

  expect_equal(as.Account(list()), list())
  expect_equal(as.Account(a), a)
  expect_equal(as.Account(list(a)), list(a))

  strict.list <- list(l1=l1, l2=l1)
  not.strict <- strict.list
  not.strict$response <- 'yess'
  not.strict$nested <- strict.list

  expect_equal(as.Account(strict.list), list(l1=a, l2=a))
  expect_error(as.Account(not.strict))

  expect_equal(as.Account(not.strict, strict=FALSE),
    list(l1=a, l2=a, response='yess', nested=as.Account(strict.list))
  )

  df <- as.data.frame(a)
  expect_s3_class(df, 'tbl_df')
  expect_named(df, c('id','name','type','on_budget','closed','note','balance','cleared_balance','uncleared_balance','transfer_payee_id','direct_import_linked','direct_import_in_error','deleted'))
  expect_equal(nrow(df), 1)

  df <- as.data.frame.Account(list(l1=a, l2=a))
  expect_s3_class(df, 'tbl_df')
  expect_named(df, c('id','name','type','on_budget','closed','note','balance','cleared_balance','uncleared_balance','transfer_payee_id','direct_import_linked','direct_import_in_error','deleted'))
  expect_equal(nrow(df), 2)
})
