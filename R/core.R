SimmerOptim<-R6::R6Class(
  "SimmerOptim",
  public=list(
    results=list(),
    sim_expr=NA,
    objective=NA,

    initialize = function(sim_expr, objective){
      self$sim_expr <- sim_expr
      self$objective <- objective
    },
    run_instance = function(...){
      temp_env = new.env(parent=globalenv())
      assign(".opt", opt_func(...), envir=temp_env)
      res<-eval(self$sim_expr(), envir=temp_env)
    }
  )
)


GridOptim<-R6::R6Class(
  "GridOptim",
  inherit=SimmerOptim,
  public=list(
    search_grid=NA,

    initialize = function(sim_expr, objective = c("min","max"), ...){
      super$objective <- match.arg(objective)
      super$initialize(sim_expr, objective)
      if(length(list(...)) == 0) stop("Please supply parameters to optimize over.")

      self$search_grid<- data.frame(expand.grid(list(...)))

      self$optimize()
    },
    optimize = function(){
      self$results<-
        lapply(1:NROW(self$search_grid), function(i){
          res<-do.call(super$run_instance, self$search_grid[i, ])
          res$index <- i
          res
        })
    },
    get_results = function(){
      # remove constraints
      results_filtered <-
        Filter(function(x) all(unlist(x$constraints)), self$results)
      # extract objective values
      objs<-sapply(results_filtered, function(x) x$objective)

      # selector func
      select_func <-
        switch(self$objective,
               "min" = which.min,
               "max" = which.max)

      best_grid_row <- results_filtered[[select_func(objs)]]$index
      as.list(self$search_grid[best_grid_row,])
    }
  ))


#' A simmer grid optimizer
#'
#' @param sim_expr a function which runs a simmer env and returns a list with at least the keys \code{objective} and \code{constraints}
#' @param objective the type of objective to focus on, for now only \code{minimize} and \code{maximize}
#' @param ... the named arguments and related vectors to optimize over, e.g. \code{var1=1:5, var2=3:5}
#'
#' @return the optimal combination of the variable possibilities supplied in \code{...}
#' @import R6
#' @export
grid_optim<-function(sim_expr, objective = c("min", "max"), ...){
  GridOptim$new(sim_expr, objective, ...)
}
