Test <- R6::R6Class('Test',
private = list(
  bla = list(a = 'b')
),
active = list(
  Bla = function(value) { if (missing(value)) return(private$bla); str(value); private$bla <- value; }
))

test <- Test$new()
test$Bla
test$Bla$a <- 'c'
test$Bla$b <- 'x'
