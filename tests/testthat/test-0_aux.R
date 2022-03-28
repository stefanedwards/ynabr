require(lubridate, quietly = TRUE, warn.conflicts = FALSE)


# list.coalesce ---------------
test_that('NULLs in a list are replaced', {
  l <- list(a=1:3, b=NULL, c='hello')
  expect_equal(list.coalesce(l), list(
    a=1:3, b=NA_character_, c='hello'
  ))
  expect_equal(list.coalesce(l, replace='null'), list(
    a=1:3, b='null', c='hello'
  ))
  l$b <- 'not null'
  expect_equal(list.coalesce(l), list(
    a=1:3, b='not null', c='hello'
  ))

  ## coalse 1
  ll <- list(
    x = list(a=1:3, b=NULL, c='hello'),
    y = list(a=1:3, b=NULL, c='hello')
  )
  expect_equal(list.coalesce_1(ll), list(
    x = list(a=1:3, b=NA_character_, c='hello'),
    y = list(a=1:3, b=NULL, c='hello')
  ))
  expect_equal(ll, list( ## input was not taken by reference!
    x=list(a=1:3, b=NULL, c='hello'),
    y=list(a=1:3, b=NULL, c='hello')
  ))
})

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

# Idfy --------------------

test_that('Idfy works on single level', {
  expect_equal(idfy(list()), list())
  x <- list(id='a',v='b',1:3)
  expect_equal(idfy(x), x)
  y <- list(id='b',v='x', NA_integer_, b='b?')
  z <- list('a',letters[1:4])
  res <- idfy(list(x,y,z, 42, list()))
  expect_named(res, c('a','b','', '', ''))
  expect_equal(res, list(a=x, b=y, z, 42, list()))

  expect_error(idfy(1:4))

  expect_named(idfy(list(x,y,nested=list(x,y,list(x,y))), max.depth=0), c('a','b','nested'))

  ## does not overwrite, if id is missing
  expect_named(idfy(list(x,b=z, 42, x=43)), c('a','b','','x'))

  ## indexes -- also checks if index i larger than list!
  expect_equal(idfy(list(x,y,z, 42, list()), el=1),
    list(a=x, b=y, a=z, 42, list())
  )
})

test_that('Idfy name repairing works', {
  x <- list(id='a',v='b',1:3)
  y <- list(id='b',v='x', NA_integer_, b='b?')
  expect_named(idfy(list(x, y, a=NA, 8), names_repair=FALSE), c('a','b','a',''))
  expect_named(idfy(list(x, y, a=NA, 8), names_repair='minimal'), c('a','b','a',''))
  expect_error(idfy(list(x, y, a=NA, 8), names_repair='check_unique'), 'Names can\'t be empty', fixed=TRUE)
  expect_named(idfy(list(x, y, a=NA, 8), names_repair='unique', quiet=TRUE), c('a...1','b','a...3','...4'))
  expect_named(idfy(list(x, y, a=NA, 8, list(id='if')), names_repair='universal', quiet=TRUE), c('a...1','b','a...3','...4', '.if'))
})

test_that('Idfy recursion works', {
  x <- list(id='a')
  y <- list(id='b')
  l <- list(
    L1=list(x,y),
    L2=list(),
    L3=list(
      x,
      y,
      nested=list(x,y,list(x,y))
    )
  )
  res <- idfy(l)
  expect_named(res, c('L1','L2','L3'))
  expect_named(res$L1, c('a','b'))
  expect_named(res$L2, NULL)
  expect_named(res$L3, c('a', 'b', 'nested'))
  expect_named(res$L3$nested, c('a', 'b', ''))
  expect_named(res$L3$nested[[3]], c('a', 'b'))

  res <- idfy(l, max.depth=0)
  expect_named(res, c('L1','L2','L3'))
  expect_named(res$L1, NULL)
  expect_named(res$L2, NULL)
  expect_named(res$L3, c('', '', 'nested'))
  expect_named(res$L3$nested, NULL)
  expect_named(res$L3$nested[[3]], NULL)

  res <- idfy(l, max.depth=1)
  expect_named(res, c('L1','L2','L3'))
  expect_named(res$L1, c('a','b'))
  expect_named(res$L2, NULL)
  expect_named(res$L3, c('a', 'b', 'nested'))
  expect_named(res$L3$nested, NULL)
  expect_named(res$L3$nested[[3]], NULL)
})

# Test type predicate wrappers ------------------
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
