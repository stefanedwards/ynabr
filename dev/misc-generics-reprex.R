Numbers <- R6::R6Class('Numbers',
public = list(
  initialize = function(x1, x2) {
    private$x1 <- as.numeric(x1)
    private$x2 <- as.numeric(x2)
  }
),
private = list(x1=NA,x2=NA),
active = list(
  Sum = function() { private$x1+private$x2 }
)
)

n <- Numbers$new(3,4)
stopifnot(n$Sum == 7)

as.Numbers <- function(x, ...) {
  UseMethod('as.Numbers', x)
}

as.Numbers.numeric <- function(x, ...) {
  stopifnot(length(x) > 1)
  Numbers$new(x[1], x[2])
}

as.Numbers.list <- function(x, ...) {
  stopifnot(length(x) > 1)
  Numbers$new(x[[1]], x[[2]])
}

as.Numbers(3:4)$Sum

as.Numbers(list(3,4))
