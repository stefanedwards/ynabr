# test CurrencyFormat

test_that('We construct a currency format', {
  cn <- CurrencyFormat$new(
    'DKK', 'kr. 123.456,78', 2, ',',
    TRUE, '.', 'kr. ', TRUE
  )
  expect_true(is.CurrencyFormat(cn))

  expect_equal(cn$format(1015.88), 'kr. 1.015,88')
  expect_output(print(cn), 'Currency DKK: kr. 123.456,78', fixed=TRUE)

  cn$SymbolFirst <- FALSE
  expect_equal(cn$format(15.88), '15,88kr.')

  cn <- as.CurrencyFormat(dk.currency.format())
  expect_s3_class(cn, 'CurrencyFormat')
  expect_equal(as.list(cn), dk.currency.format())

  expect_output(print(cn), 'Currency DKK: 123.456,78') ## dk.currency.format is not prefixed by kr.
  cn$ExampleFormat <- NA
  expect_equal(cn$format(123456.78), '123.456,78kr.')
  expect_output(print(cn), 'Currency DKK: 123.456,79kr.', fixed=TRUE)
})

test_that('Restrictions are caught', {
  cn <- as.CurrencyFormat(dk.currency.format())
  expect_error(cn$CurrencySymbol <- c(1:2))
  expect_warning(
    expect_error(cn$DecimalDigits <- 'no'),
    'NAs introduced by coercion',
    fixed=TRUE
  )
  expect_error(cn$DecimalDigits <- 3:4)
  expect_error(cn$DecimalSeparator <- '--')
  expect_error(cn$DecimalSeparator <- LETTERS[5:6])
  expect_error(cn$DisplaySymbol <- LETTERS[7:8])
  expect_error(cn$GroupSeparator <- '++')
  expect_error(cn$GroupSeparator <- LETTERS[9:10])
  expect_error(cn$IsoCode <- LETTERS[11:12])
  expect_error(cn$SymbolFirst <- 'yes')
  expect_error(cn$SymbolFirst <- c(TRUE,TRUE))
})

test_that('Conversions works', {
  cn <- as.CurrencyFormat(dk.currency.format())
  expect_equal(as.CurrencyFormat(as.list(cn)), cn)
})

test_that('Formatting retains names', {
  cn <- CurrencyFormat$new('')
  expect_output(print(cn), 'Currency : 123,456.79')
  x <- c(77, 0.01, 118492.9999)
  names(x) <- LETTERS[seq_along(x)]
  res <- cn$format(x)
  expect_equal(res, c('77.00','0.01','118,493.00'), ignore_attr=TRUE)
  expect_named(res, names(x))

})
