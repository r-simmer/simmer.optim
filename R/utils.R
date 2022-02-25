#' @import simmer
NULL

#' Function to pass results of simmer evaluation back to the optimization framework
#'
#' @param objective the value of the objective
#' @param constraints a list with named objectives containing only TRUE and FALSE values
#' @param envs the simmer env (OPTIONAL)
#'
#' @export
optim_results <-
  function(objective,
           constraints = list(),
           envs = NULL) {
    res <- list(objective = objective,
                constraints = constraints,
                envs = envs)
    attr(res, "class") <- "OptimResults"
    res
  }


#' Value function generator
#'
#' Internal usage
#'
#' @param params a named list of params
#' @export
opt_func <- function(params) {
  function(name) params[[name]]
}


#' Show the results of an optimization procedure
#'
#' @param optim_obj the optimization object
#' @export
results <- function(optim_obj) {
  res <- optim_obj$results()
  class(res) <- c("OptimResult", class(res))
  res
}

#' @export
print.OptimResult <- function(x, ...) {
  cat("Optimization result\n")
  cat("========================\n")
  cat("Objective value: ", x$objective, "\n\n")
  cat("Params:\n")
  for (p in names(x$params)) {
    cat(" -- ", p, ": ", x$params[[p]], "\n", sep = "")
  }
  invisible(x)
}

#' (re)run a simmer expression using the optimized parameter list
#'
#' For this to work an \code{envs} object has to be returned with the \code{optim_results}
#'
#' @param optim_obj the optimization object
#'
#' @seealso results
#' @export
run_optimized <- function(optim_obj) {
  message("Running sim expression with optimized parameters")
  message("------------------------------------------------")
  params <- results(optim_obj)$params
  for (p in names(params)) {
    message(p, ": ", params[p])
  }
  sim_expr <- optim_obj$sim_expr
  do.call(run_instance, c(params, list(sim_expr = sim_expr)))$envs
}
