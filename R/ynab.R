#' YNAB connection object
#'
#' Use this object for every request to YNAB -- it's similar to an ordinary
#' database connection.
#'
#' To obtain a token, visit \link{https://app.youneedabudget.com/settings/developer}
#' and create a "Personal Access Token".
#'
#' @section Warning:
#' \emph{\strong{WARNING!}} A personal access token gives full access to your financial
#' records stored in your YNAB budgets! \strong{Never share your personal access token with anybody!}
#' @rdname YNAB-object
#' @name ynab
#' @export
YNAB <- R6::R6Class("YNAB",
public = list(
  #' @description
  #' Creates a new "YNAB connection"-object, which keeps
  #'
  #' @param token Personal access token.
  #' @return A new `YNAB` object.
  initialize = function(token) {
    self$AccessToken <- token

    invisible(self)
  }
),
private = list(
  accessToken = NA_character_
),
active = list(
  #' @field AccessToken
  #' Sets/gets the stored, personal access token.
  AccessToken = function(value) {
    if (rlang::is_missing(value)) {
      return(private$accessToken)
    } else {
      assertthat::asssert_that(is_character(value, 1))
      private$accessToken <- value
    }
  }
)
)
