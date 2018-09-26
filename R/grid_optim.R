#' A simmer grid optimizer
#'
#' Executes an exhaustive search over the solution space
#'
#' @param model the simmer model encapsulated in a function
#' @param direction optimization direction (\code{max} or \code{min})
#' @param objective the objective function
#' @param constraints a list of constraint functions
#' @param params a list of parameters to optimize over
#' @param control a control list created by a call to \code{optim_control()}
#'
#' @export
grid_optim <- function(model, direction = c("min", "max"), objective, constraints, params, control) {
  direction <- match.arg(direction)
  search_grid <- data.frame(expand.grid(params))

  if(control$verbose){
    cat("Running grid optimization procedure", "\n")
    pb <- utils::txtProgressBar(style=3)
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

      if(control$verbose) utils::setTxtProgressBar(pb, i/NROW(search_grid))
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

  # stop if no instance available that satisfies constraints
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

