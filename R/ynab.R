#' YNAB connection object
#'
#' Use this object for every request to YNAB -- it's similar to an ordinary
#' database connection.
#'
#' To obtain a token, visit
#' \href{https://app.youneedabudget.com/settings/developer}
#' and create a "Personal Access Token".
#'
#' @section Warning:
#' \emph{\strong{WARNING!}} A personal access token gives full access to your financial
#' records stored in your YNAB budgets! \strong{Never share your personal access token with anybody!}
#' @rdname YNABobject
#' @name ynab
#' @include exceptions.R
#' @export
#' @examples
#' ## create a new connection object:
#' ynab <- YNAB$new('<personal access token>') ## see warning about token!
YNAB <- R6::R6Class("YNAB",
public = list(
  #' @description
  #' Creates a new "YNAB connection"-object, which keeps track of your budget.
  #'
  #' @param token Personal access token.
  #' @return A new 'YNAB' object.
  initialize = function(token) {
    self$AccessToken <- trimws(token)

    invisible(self)
  },
  #' @description
  #' Gets an overview of the available budgets.
  #' @param include.accounts Logical, whether to include the list of budget accounts.
  load = function(include.accounts=FALSE) {
    assertthat::assert_that(rlang::is_logical(include.accounts, n=1))
    response <- self$Query('budgets', method='GET', query=list(include_accounts=include.accounts))

    ## response may also include `default_budget`.
    budgets <- response$budgets
    assertthat::assert_that(is.list(budgets))
    if (length(budgets) == 0) {
      warning('YNAB returned 0 budgets.')
      return(invisible(self))
    }
    private$budgets <- idfy(budgets, max.depth=0)
    private$budget_names <- structure(names(private$budgets), .Names=sapply(budgets, `[[`, 'name'))

    invisible(self)
  },
  #' @description
  #' Prints object
  #' @param ... Discarded
  print = function(...) {
    cat(glue::glue(
      'YNAB connection to {private$baseUrl}\n',
      'Requests remaining this hour: {private$limit - private$requests}'
    ))
  },
  #' @description
  #' Queries the YNAB endpoint
  #'
  #' @param endpoint The endpoint, e.g. \code{/budgets/{budget_id}/settings} (with appropriate fill-in of id).
  #'   Can also be a character vector of path-elements to the endpoint.
  #' @param query Named-list of query.
  #' @param method Whether to perform \link[httr]{GET} OR POST (only GET is supported).
  #' @param timeout Timeout in seconds.
  #' @return Unserialised response or throws an error.
  Query = function(endpoint, query=list(), method=c('GET'), timeout = 10) {
    method <- match.arg(method)
    if (is.null(query)) query <- list()
    assertthat::assert_that(is.list(query))

    endpoint <- url.endpoint(private$baseUrl, endpoint)


    latest_knowledge <- self$LatestServerKnowledge(endpoint)
    if (latest_knowledge > 0)
      query$last_knowledge_of_server <- as.integer(latest_knowledge)

    call <- switch(method, GET=httr::GET)

    response <- call(
      endpoint,
      query=query,
      httr::timeout(timeout),
      add_headers(token=private$accessToken)
    )

    if (response$status_code == 429) { ## rate-limited
      stop(rate.limited.exception(endpoint, response))
    }
    httr::stop_for_status(response, paste('request', endpoint))
    requests <- response$headers$`X-Rate-Limit`
    if (!is.null(requests)) {
      requests <- strsplit(requests, '/', fixed=TRUE)[[1]]
      private$requests <- as.integer(requests[1])
      private$limit <- as.integer(requests[[2]])
    }

    content <- httr::content(response)
    if (!is.null(content$data$server_knowledge)) {
      private$deltas[[endpoint]] <- as.integer(content$data$server_knowledge)
    }

    content$data
  },
  #' @description
  #' Returns the latest server knowledge for a given endpoint.
  #' Used for delta requests.
  #' @param endpoint Single string of endpoint to look up.
  #' @return The server knowledge (delta request) or 0 if none is known.
  LatestServerKnowledge = function(endpoint) {
    assertthat::assert_that(rlang::is_character(endpoint, n=1))
    k <- private$deltas[[endpoint]]
    if (is.null(k))
      return(0L)
    return(as.integer(k))
  },
  #' @description
  #' Gets a budget by it's name.
  #' @param name Name of budget to find.
  GetBudgetByName = function(name) {
    private$budgets[[private$budget_names[name]]]
  }
),
private = list(
  accessToken = NA_character_,
  baseUrl = 'https://api.youneedabudget.com/v1',
  deltas = list(),
  requests = 0L,
  limit = 200L,
  budgets = list(),
  budget_names = character(0)
),
active = list(
  #' @field AccessToken
  #' Sets/gets the stored, personal access token.
  AccessToken = function(value) {
    if (rlang::is_missing(value)) {
      return(private$accessToken)
    } else {
      value <- as.character(value)
      assertthat::assert_that(rlang::is_character(value, 1))
      private$accessToken <- trimws(value)
    }
  },
  #' @field BaseUrl
  #' The base url of the YNAB v.1 API
  BaseUrl = function() { private$baseUrl },
  #' @field Requests
  #' Number of \emph{Requests} out of the hourly \emph{Limit} done so far.
  Requests = function() { private$requests },
  #' @field Limit
  #' Number of \emph{Requests} out of the hourly \emph{Limit} done so far.
  Limit = function() { private$limit },
  #' @field Budgets
  #' Returns loaded budgets.
  Budgets = function() {
    if (length(private$budgets) == 0)
      stop('No budgets loaded.')
    private$budgets
  },
  #' @field BudgetNames
  #' Character vector of the names of the budgets.
  BudgetNames = function() {
    names(private$budget_names)
  }
)
)

#' @rdname YNABobject
#' @export
is.ynab <- function(x) {
  return(R6::is.R6(x) && inherits(x, 'YNAB'))
}
