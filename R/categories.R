#' Categories and category groups
#' @rdname category
#' @export
YnabCategoryGroup <- R6::R6Class("YnabCategoryGroup",
public = list(
  #' @description
  #' Creates a new YNAB category group-object.
  #'
  #' @param budget A \code{\link{YnabBudget}}-object.
  #' @param id,name,hidden,deleted
  #'   Values/list
  #' @param is.json.list Logical, when \code{TRUE}, \code{id} can be a list.
  #' @return A new `YnabCategoryGroup` object.
  initialize = function(budget, id, name, hidden, deleted, is.json.list=FALSE) {
    assertthat::assert_that(is.ynab.budget(budget))
    if (is.json.list) {
      call <- id
      call$budget <- budget
      call$is.json.list <- FALSE
      return(do.call(YnabCategoryGroup$new, call))
    }

    private$budget <- budget
    private$id <- id
    private$name <- name
    private$hidden <- hidden
    private$deleted <- deleted

    invisible(self)
  }
),
private = list(
  budget = NULL,
  id = NULL,
  name = NULL,
  hidden = NULL,
  deleted = NULL
),
active = list(
  #' @field Budget Budget-object, to which this category is part of.
  Budget = function() { private$budget },
  #' @field YNAB YNAB-connection object
  YNAB = function() { private$budget$YNAB },
  #' @field Id Internal id
  Id = function() { private$id },
  #' @field Name Category group's name
  Name = function() { private$name },
  #' @field Hidden Logical
  Hidden = function() { private$hidden },
  #' @field Deleted Logical
  Deleted = function() { private$deleted }
)
)

#' @rdname is
#' @export
is.ynabcategorygroup <- function(x) {
  return(R6::is.R6(x) && inherits(x, 'YnabCategoryGroup'))
}



#' @rdname category
#' @export
YnabCategory <- R6::R6Class("YnabCategory",
  public = list(
    #' @description
    #' Creates a new YNAB category-object.
    #'
    #' @param budget A \code{\link{YnabBudget}}-object.
    #' @param id,category_group_id,name,hidden,original_category_group_id,note,budgeted,activity,balance,goal_type,goal_creation_month,goal_target,goal_target_month,goal_percentage_complete,goal_months_to_budget,goal_under_funded,goal_overall_funded,goal_overall_left,deleted
    #'   Values/list
    #' @param is.json.list Logical, when \code{TRUE}, \code{id} can be a list.
    #' @return A new `YnabCategory` object.
    initialize = function(
      budget,
      id, category_group_id, name, hidden, original_category_group_id,
      note, budgeted, activity, balance, goal_type, goal_creation_month,
      goal_target, goal_target_month, goal_percentage_complete,
      goal_months_to_budget, goal_under_funded,
      goal_overall_funded, goal_overall_left,
      deleted, is.json.list=FALSE) {


      if (is.json.list) {
        call <- id
        call$budget <- budget
        call$is.json.list <- FALSE
        return(do.call(YnabCategoryGroup$new, call))
      }


     private$id <- id
     private$name <- name
     private$hidden <- hidden
     private$deleted <- deleted

     invisible(self)
   }
  ),
  private = list(
   id = NULL,
   name = NULL,
   hidden = NULL,
   deleted = NULL
  ),
  active = list(
   #' @field Id Internal id
   Id = function() { private$id },
   #' @field Name Category group's name
   Name = function() { private$name },
   #' @field Hidden Logical
   Hidden = function() { private$hidden },
   #' @field Deleted Logical
   Deleted = function() { private$deleted }
  )
)

#' @rdname is
#' @export
is.ynabcategory <- function(x) {
  return(R6::is.R6(x) && inherits(x, 'YnabCategory'))
}
