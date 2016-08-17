SimmerOptim<-R6::R6Class(
  "SimmerOptim",
  public=list(
    sim_expr = NULL,
    objective = NULL,
    .results = NULL,
    initialize = function(sim_expr, objective){
      self$sim_expr <- sim_expr
      self$objective <- objective
    },

    run_instance = function(...){
      run_instance(self$sim_expr, ...)
    },

    results = function(objective, params, best_run, ...){
      if(missing(objective)) return(self$.results)
      else{
        self$.results <- c(list(objective = objective,
                                params = params),
                           list(...))
      }
    }
  )
)


#' Instantiate a simmer expression
#'
#' @param sim_expr the simmer expression
#' @param ... named parameters to be passed to the simmer expression
#'
#' @export
run_instance <- function(sim_expr, ...){
  temp_env <- new.env(parent=globalenv())
  assign(".opt", opt_func(...), envir=temp_env)
  eval(body(sim_expr), envir=temp_env)
}
