# auxiliary functions

parse_utc <- function(x) {
  lubridate::fast_strptime(x, '%Y-%m-%dT%H:%M:%OS%z')
}
