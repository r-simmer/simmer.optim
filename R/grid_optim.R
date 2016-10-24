#' A simmer grid optimizer
#'
#' @param model a function which runs a simmer model
#' @param direction optmization direction to focus on, for now only \code{min} and \code{max}
#' @param objective the objective measure
#' @param constraints a list of constraint functions
#' @param ... the named arguments and related vectors to optimize over, e.g. \code{var1=1:5, var2=3:5}
#'
#' @return the optimal combination of the variable possibilities supplied in \code{...}
#' @import R6
#' @export
grid_optim <- function(model, direction = c("min", "max"), objective, constraints, params, control) {
  direction <- match.arg(direction)
  search_grid <- data.frame(expand.grid(params))

  if(control$verbose){
    cat("Running grid optimization procedure", "\n")
    pb <- txtProgressBar(style=3)
  }


  # construct different envs
  intermediary_results <-
    lapply(1:NROW(search_grid), function(i) {

      args <- list(model=model,
                   control = control,
                   params = as.list(search_grid[i, , drop = FALSE]))

      envs <- do.call(run_instance, args)

      res <- list(
        envs = envs,
        objective_value = objective_evaluator(envs, objective),
        constraints_satisfied = constraints_evaluator(envs, constraints),
        index = i
      )

      if(control$verbose) setTxtProgressBar(pb, i/NROW(search_grid))
      res
    })

  if(control$verbose){
    close(pb)
    cat("\n\n")
  }

  # remove constraint violations
  results_filtered <-
    Filter(function(x)
      all(unlist(x$constraints_satisfied)), intermediary_results)

  # stop if no instance available that satisfies constriants
  if (length(results_filtered) == 0)
    stop("No instance(s) available where constraints are satisfied.")

  # extract objective values
  objs <- sapply(results_filtered, function(x)
    x$objective_value)

  # selector func
  select_func <-
    switch(direction,
           "min" = which.min,
           "max" = which.max)

  best_run <- results_filtered[[select_func(objs)]]

  best_grid_row <- results_filtered[[select_func(objs)]]$index

  method_results(
    method = "grid_optim",
    objective_value = best_run$objective_value,
    constraints_satisfied = all(unlist(best_run$constraints_satisfied)),
    params = as.list(search_grid[best_grid_row, , drop=FALSE]),
    envs = best_run$envs
  )
}

