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
grid_optim <- function(model, direction = c("min", "max"), objective, constraints, rep=1, ...) {
  direction <- match.arg(direction)

  if (length(list(...)) == 0)
    stop("Please supply parameters to optimize over.")

  search_grid <- data.frame(expand.grid(list(...)))

  # construct different envs
  intermediary_results <-
    lapply(1:NROW(search_grid), function(i) {
      envs <- do.call(run_instance, c(list(model=model, rep=rep), as.list(search_grid[i, , drop = FALSE])))
      list(
        envs = envs,
        objective_value = objective_evaluator(envs, objective),
        constraints_satisfied = constraints_evaluator(envs, constraints),
        index = i
        )

    })

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

