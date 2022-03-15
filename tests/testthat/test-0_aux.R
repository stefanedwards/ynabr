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
