#' Currency format for YNAB budget
#'
#' The currency format setting for the budget.
#'
#' In some cases the format will not be available and will be specified as null.
#'
#' @export
#' @examples
#'
#' cf <- CurrencyFormat$new(
#'   iso_code='USD',
#'   decimal_digits=2, decimal_separator='.',
#'   symbol_first=TRUE, group_separator=',',
#'   currency_symbol='$', display_symbol=TRUE
#' )
#' as.list(cf)
#' @importFrom assertthat assert_that
#' @importFrom rlang is_missing is_scalar_character is_scalar_integer is_scalar_logical
CurrencyFormat <- R6::R6Class('CurrencyFormat',
public = list(
  #' @description
  #' Creates a new CurrencyFormat-object.
  #'
  #' @param iso_code,example_format,decimal_digits,decimal_separator,symbol_first,group_separator,currency_symbol,display_symbol
  #'   Values; most are string except
  #'   \code{decimal_digits} is integer, and \code{symbol_first} and \code{display_symbol} are logical.
  #' @return A new 'CurrencyFormat' object.
  initialize = function(
    iso_code, example_format=NA, decimal_digits=2, decimal_separator='.',
    symbol_first=FALSE, group_separator=',', currency_symbol='', display_symbol=FALSE
  ) {

    private$iso_code <- as.character(iso_code)
    private$example_format <- as.character(example_format)
    private$decimal_digits <- as.integer(decimal_digits)
    private$decimal_separator <- as.character(decimal_separator)
    private$symbol_first <- as.logical(symbol_first)
    private$group_separator <- as.character(group_separator)
    private$currency_symbol <- as.character(currency_symbol)
    private$display_symbol <- as.logical(display_symbol)

	  private$validate()

    invisible(self)
  },
  #' @description
  #' Formats \code{x}.
  #' @param x Numeric vector
  #' @return Character vector formatted cf. object.
  format = function(x) {
    s <- formatC(x = as.numeric(x), format = 'f',
      digits = private$decimal_digits,
      big.mark = private$group_separator,
      big.interval = 3,
      decimal.mark = private$decimal_separator
    )
    if (private$display_symbol && private$symbol_first) {
      s <- paste0(private$currency_symbol, s)
    } else if (private$display_symbol && !private$symbol_first) {
      s <- paste0(s, private$currency_symbol)
    }
    names(s) <- names(x)
    return(trimws(s))
  },

  #' @description
  #' Prints this object.
  #' @param ... Discarded.
  print = function(...) {
    ex <- private$example_format
    if (is.na(ex))
      ex <- self$format(123456.789)

    cat(glue::glue(
      'Currency {private$iso_code}: {ex}'
    ))
    invisible(self)
  },
  #' @description
  #' Returns copy of object as a list.
  #' \code{as.list} is a wrapper for this method.
  #' @return A list
  ToList = function() {
    res <- as.list(private)
    res[c(
      'iso_code','example_format','decimal_digits','decimal_separator',
      'symbol_first','group_separator','currency_symbol','display_symbol'
    )]
  }
),
private = list(
  iso_code = NA_character_,
  example_format = NA_character_,
  decimal_digits = NA_integer_,
  decimal_separator = NA_character_,
  symbol_first = NA,
  group_separator = NA_character_,
  currency_symbol = NA_character_,
  display_symbol = NA,
  validate = function() {
    assert_that(
      is_scalar_character(private$iso_code) && !is.na(private$iso_code),
      is_scalar_character(private$example_format), # & !is.na(private$example_format),
      is_scalar_integer(private$decimal_digits) && !is.na(private$decimal_digits),
      is_scalar_character(private$decimal_separator) && !is.na(private$decimal_separator) && nchar(private$decimal_separator) == 1,
      is_scalar_logical(private$symbol_first) && !is.na(private$symbol_first),
      is_scalar_character(private$group_separator) && !is.na(private$group_separator) && nchar(private$decimal_separator) == 1,
      is_scalar_character(private$currency_symbol) && !is.na(private$currency_symbol),
      is_scalar_logical(private$display_symbol) && !is.na(private$display_symbol)
    )
  }
),
active = list(
  #' @field IsoCode Iso code
  IsoCode = function(val) {
    if (missing(val))
      return(private$iso_code)
    private$iso_code <- as.character(val)
    private$validate()
  },
  #' @field ExampleFormat Example string of format
  ExampleFormat = function(val) {
    if (missing(val))
      return(private$example_format)
    private$example_format <- as.character(val)
    private$validate()
  },
  #' @field DecimalDigits Number of decimal digits
  DecimalDigits = function(val) {
    if (missing(val))
      return(private$decimal_digits)
    private$decimal_digits <- as.integer(val)
    private$validate()
  },
  #' @field DecimalSeparator Decimal separator
  DecimalSeparator = function(val) {
    if (missing(val))
      return(private$decimal_digits)
    private$decimal_digits <- as.character(val)
    private$validate()
  },
  #' @field SymbolFirst Logical, whether to display the symbol in front or after value.
  SymbolFirst = function(val) {
    if (missing(val))
      return(private$symbol_first)
    private$symbol_first <- as.logical(val)
    private$validate()
  },
  #' @field GroupSeparator Separator between every 3 digits.
  GroupSeparator = function(val) {
    if (missing(val))
      return(private$group_separator)
    private$group_separator <- as.character(val)
    private$validate()
  },
  #' @field CurrencySymbol Separator between every 3 digits.
  CurrencySymbol = function(val) {
    if (missing(val))
      return(private$currency_symbol)
    private$currency_symbol <- as.character(val)
    private$validate()
  },
  #' @field DisplaySymbol Separator between every 3 digits.
  DisplaySymbol = function(val) {
    if (missing(val))
      return(private$display_symbol)
    private$display_symbol <- as.logical(val)
    private$validate()
  }
))



#' @describeIn CurrencyFormat Checks whether an object is a CurrencyFormat object.
#' @export
is.CurrencyFormat <- function(x) {
  return(R6::is.R6(x) && inherits(x, 'CurrencyFormat'))
}

#' @describeIn CurrencyFormat Converts a list with named elements to a new
#'   CurrencyFormat object.
#' @export
as.CurrencyFormat <- function(x, ...) {
  if (is.list(x)) {
    return(as.CurrencyFormat.list(x, ...))
  }
  UseMethod('as.CurrencyFormat', x)
}

as.CurrencyFormat.CurrencyFormat <- function(x, ...) {
  x
}

as.CurrencyFormat.list <- function(x, ...) {
  return(do.call(CurrencyFormat$new, x))
}


#' @describeIn CurrencyFormat Converts a CurrencyFormat object to a list.
#' @export
#' @param x A CurrencyFormat to check/convert.
as.list.CurrencyFormat <- function(x, ...) {
	x$ToList()
}

