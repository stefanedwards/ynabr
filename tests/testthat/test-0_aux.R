require(lubridate, quietly = TRUE, warn.conflicts = FALSE)

test_that('Parsing UTC datetimes', {
  ## UTC
  x <- parse_utc("2022-03-14T11:45:57.821Z")
  expect_s3_class(x, 'POSIXlt')

  expect_equal(lubridate::date(x), as.Date('2022-03-14'))
  expect_equal(tz(x), 'UTC')
  expect_equal(second(x), 57.821)
  expect_equal(hour(x), 11)

  ## Danish timezone
  x <- parse_utc("2022-03-14T06:34:25+00:00")
  expect_equal(lubridate::date(x), as.Date('2022-03-14'))
  expect_equal(tz(x), 'UTC')
  expect_equal(second(x), 25)
  expect_equal(hour(x), 6)

})

test_that('Endpoints are constructed nicely', {
  baseurl <- 'http://foo/'
  expect_equal(url.endpoint(baseurl, 'bar'), 'http://foo/bar')
  expect_equal(url.endpoint(baseurl, 'bar', 5, 'nicely'), 'http://foo/bar/5/nicely')
  expect_equal(url.endpoint(baseurl, c('zul',42,'X')), 'http://foo/zul/42/X')

  expect_equal(url.endpoint(baseurl, baseurl, 'bar'), 'http://foo/bar')
})

test_that('Type predicate wrappers, logicals', {
  expect_true(is_single_logical(TRUE, na.ok=FALSE))
  expect_true(is_single_logical(NA_integer_, na.ok=TRUE))
  expect_true(is_single_logical(TRUE, na.ok=TRUE))
  expect_true(is_single_logical(NA, na.ok=TRUE))
  expect_false(is_single_logical(NA, na.ok=FALSE))
  expect_false(is_single_logical(NA_integer_, na.ok=FALSE))

  expect_false(is_single_logical(logical(0), na.ok=FALSE))
  expect_false(is_single_logical(logical(0), na.ok=TRUE))
  expect_false(is_single_logical(c(NA, TRUE), na.ok=FALSE))
  expect_false(is_single_logical(c(NA, TRUE), na.ok=TRUE))
  expect_false(is_single_logical(1, na.ok=FALSE))
  expect_false(is_single_logical(1, na.ok=TRUE))
  expect_false(is_single_logical(list(), na.ok=FALSE))
  expect_false(is_single_logical(list('a'), na.ok=FALSE))
  expect_false(is_single_logical(list('a','b'), na.ok=FALSE))
  expect_false(is_single_logical(list(), na.ok=TRUE))
  expect_false(is_single_logical(list('a'), na.ok=TRUE))
  expect_false(is_single_logical(list('a','b'), na.ok=TRUE))
})

test_that('Type predicate wrappers, numerics', {
  expect_true(is_single_numeric(NA, na.ok=TRUE))
  expect_true(is_single_numeric(NA_real_, na.ok=TRUE))
  expect_true(is_single_numeric(1, na.ok=TRUE))
  expect_true(is_single_numeric(1L, na.ok=TRUE))
  expect_true(is_single_numeric(1.5, na.ok=TRUE))

  expect_false(is_single_numeric(NA, na.ok=FALSE))
  expect_false(is_single_numeric(NA_real_, na.ok=FALSE))
  expect_true(is_single_numeric(1, na.ok=FALSE))
  expect_true(is_single_numeric(1L, na.ok=FALSE))
  expect_true(is_single_numeric(1.5, na.ok=FALSE))

  expect_false(is_single_numeric(Inf, na.ok=FALSE))
  expect_false(is_single_numeric(Inf, na.ok=TRUE))


  expect_false(is_single_numeric('1', na.ok=FALSE))
  expect_false(is_single_numeric('1', na.ok=TRUE))
  expect_false(is_single_numeric(integer(0), na.ok=FALSE))
  expect_false(is_single_numeric(integer(0), na.ok=TRUE))
  expect_false(is_single_numeric(character(0), na.ok=FALSE))
  expect_false(is_single_numeric(character(0), na.ok=TRUE))
  expect_false(is_single_numeric(TRUE, na.ok=FALSE))
  expect_false(is_single_numeric(TRUE, na.ok=TRUE))
  expect_false(is_single_numeric(list(), na.ok=FALSE))
  expect_false(is_single_numeric(list('a'), na.ok=FALSE))
  expect_false(is_single_numeric(list('a','b'), na.ok=FALSE))
  expect_false(is_single_numeric(list(), na.ok=TRUE))
  expect_false(is_single_numeric(list('a'), na.ok=TRUE))
  expect_false(is_single_numeric(list('a','b'), na.ok=TRUE))
})

test_that('Type predicate wrappers, strings', {
  expect_true(is_single_string('1', na.ok=FALSE))
  expect_true(is_single_string('1', na.ok=TRUE))
  expect_true(is_single_string(NA, na.ok=TRUE))
  expect_false(is_single_string(NA, na.ok=FALSE))
  expect_true(is_single_string(NA_character_, na.ok=TRUE))
  expect_false(is_single_string(NA_character_, na.ok=FALSE))

  expect_false(is_single_string(character(0)))
  expect_false(is_single_string(character(0), na.ok=TRUE))
  expect_false(is_single_string(TRUE))
  expect_false(is_single_string(TRUE, na.ok=TRUE))
  expect_false(is_single_string(1))
  expect_false(is_single_string(1, na.ok=TRUE))
  expect_false(is_single_string(list()))
  expect_false(is_single_string(list('a')))
  expect_false(is_single_string(list('a','b')))
  expect_false(is_single_string(list(), na.ok=TRUE))
  expect_false(is_single_string(list('a'), na.ok=TRUE))
  expect_false(is_single_string(list('a','b'), na.ok=TRUE))
})
