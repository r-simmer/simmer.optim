

DEOptim <- R6::R6Class(
  "DEOptim",
  inherit = SimmerOptim,
  public = list(
    big_M = 1e9,
    obj_coeff = NULL,
    lower_bounds = NULL,
    upper_bounds = NULL,
    integer_vals = NULL,
    deoptim_control = NULL,
    print = function() {
      cat("A SimmerOptim instance of type Differential Evolution\n")
    },
    initialize = function(sim_expr,
                          objective = c("min", "max"),
                          deoptim_control = RcppDE::DEoptim.control(),
                          ...) {
      objective <- match.arg(objective)
      super$initialize(sim_expr, objective)

      self$obj_coeff <- switch(objective,
                               max = -1,
                               min = 1)

      if (length(list(...)) == 0)
        stop("Please supply parameters to optimize over.")

      self$deoptim_control <- deoptim_control

      args <- list(...)
      self$lower_bounds <- sapply(args, function(x)
        x[1])
      self$upper_bounds <- sapply(args, function(x)
        x[2])
      self$integer_vals <- sapply(self$lower_bounds, is.integer)

      self$optimize()
      self
    },
    convert_obj_value = function(results) {
      constraints_met <- all(unlist(results$constraints))
      if (!constraints_met) {
        self$big_M * -self$obj_coeff
      } else {
        results$objective * self$obj_coeff
      }
    },
    optimize = function() {
      fn <- function(func_params) {
        arg_names <- names(self$lower_bounds)
        named_args <- list()
        for (i in seq_along(arg_names)) {
          if (self$integer_vals[i])
            named_args[[arg_names[i]]] <- round(func_params[[i]])
          else
            named_args[[arg_names[i]]] <- func_params[[i]]
        }

        obj <- do.call(super$run_instance, named_args)
        self$convert_obj_value(obj)
      }

      # create fnMap func to take into account integer constraints

      res <- RcppDE::DEoptim(
        fn,
        lower = self$lower_bounds,
        upper = self$upper_bounds,
        control = self$deoptim_control
      )

      params <- res$optim$bestmem
      for (i in seq_along(params)) {
        if (self$integer_vals[i])
          params[i] <- round(params[i])
      }

      params <- as.list(params)

      super$results(
        objective = res$optim$bestval * self$obj_coeff,
        params = params,
        iters = res$optim$iter
      )
    }
  )
)


#' A simmer differential evoltion optimizer
#'
#' Implements the functionality of the \code{DEoptim} package.
#'
#' @param sim_expr a function which runs a simmer env and returns a list with at least the keys \code{objective} and \code{constraints}
#' @param objective the type of objective to focus on, for now only \code{minimize} and \code{maximize}
#' @param deoptim_control an object of \code{RcppDE::DEoptim.control} to control the \code{DEoptim method}
#' @param ... the named arguments and per argument an vector of length two defining the lower and upper bound of the search space, e.g. \code{var1=c(1,5), var2=c(3,5)}
#'
#' @return the optimal combination of the variable possibilities supplied in \code{...}
#' @import R6
#' @export
de_optim <-
  function(sim_expr,
           objective = c("min", "max"),
           deoptim_control = RcppDE::DEoptim.control(),
           ...) {
    dep <- requireNamespace("RcppDE")
    if (!dep)
      stop("Please install package 'RcppDE' before continuing")
    DEOptim$new(sim_expr, objective, deoptim_control = deoptim_control, ...)
  }
