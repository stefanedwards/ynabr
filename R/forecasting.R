# forecasting

#' @include transactions.R
expand.months <- function(start_date, end_date, category_id=NA_character_, frequency='monthly') {
  tidyr::crossing(
    month=.repeat.dates(start_date, end_date, frequency = frequency),
    category_id = category_id
  )
}

#' @importFrom rlang sym
year.month <- function(df, column, month_name='month', year_name='year', check.names=TRUE) {
  ## check existing names
  if (check.names && c(month_name, year_name) %in% names(df)) {
    stop('Names for new columns month and year already exists in input data.frame.')
  }

  m_ <- sym(month_name)
  y_ <- sym(year_name)

  df %>%
    mutate(local({
      d <- cur_data()[,column,drop=TRUE]
      d <- as.Date(d)
      tibble(!!m_:=month(d), !!y_:=year(d))
    }))

}
