
GridOptim<-R6::R6Class(
  "GridOptim",
  inherit=SimmerOptim,
  public=list(
    search_grid=NA,
    print = function(){
      cat("A SimmerOptim instance of type GridOptim \n\n")
      if(is.null(super$results()))
        cat("Optimization procedure hasn't run yet")
      else
        cat("Optimization procedure has been run.\nCheck the results by calling results(this_object)")
    },

    initialize = function(sim_expr, objective = c("min","max"), ...){
      objective <- match.arg(objective)
      super$initialize(sim_expr, objective)
      if(length(list(...)) == 0) stop("Please supply parameters to optimize over.")

      self$search_grid<-data.frame(expand.grid(list(...)))

      self$optimize()
      self
    },
    optimize = function(){
      intermediary_results<-
        lapply(1:NROW(self$search_grid), function(i){
          res<-do.call(super$run_instance, self$search_grid[i, , drop=FALSE])
          res$index <- i
          res
        })

      # remove constraint violations
      results_filtered <-
        Filter(function(x) all(unlist(x$constraints)), intermediary_results)

      # stop if no instance available that satisfies constriants
      if(length(results_filtered) == 0) stop("No instance(s) available were constraints were satisfied.")

      # extract objective values
      objs<-sapply(results_filtered, function(x) x$objective)

      # selector func

      select_func <-
        switch(self$objective,
               "min" = which.min,
               "max" = which.max)

      best_run <- results_filtered[[select_func(objs)]]
      best_grid_row <- results_filtered[[select_func(objs)]]$index

      super$results(objective = best_run$objective,
                    params = as.list(self$search_grid[best_grid_row, , drop=FALSE]),
                    details = best_run)
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
