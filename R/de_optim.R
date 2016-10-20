

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
de_optim <- function(model,
                     direction = c("min", "max"),
                     objective,
                     constraints,
                     params,
                     control,
                     big_m = 1e6){

  dep <- requireNamespace("RcppDE")
  if (!dep)
    stop("Please install package 'RcppDE' before continuing")

  obj_coeff <- switch(direction,
                      max = -1,
                      min = 1)

  # interpret controls (or add defaults)
  de_control <- RcppDE::DEoptim.control()
  if(is.list(control$de_optim)){
    de_control <- control$de_optim
  }

  lower_bounds <- sapply(params, function(x) x[1])
  upper_bounds <- sapply(params, function(x) x[length(x)])
  param_types <- sapply(params, function(x){
    if(is(x, "ParDiscrete")){
      "ParDiscrete"
    } else {
      "ParContinuous"
    }
  })
  param_names <- names(params)

  fn <- function(func_params) {
    func_params <- lapply(1:length(func_params), function(i){
      if(param_types[[i]] == "ParDiscrete"){
        round(func_params[[i]])
      } else {
        func_params[[i]]
      }
    })

    names(func_params) <- param_names

    args <- list(model=model,
                 control = control,
                 params = func_params)

    envs <- do.call(run_instance, args)
    cons <- constraints_evaluator(envs, constraints)
    obj_val <- objective_evaluator(envs, objective) * obj_coeff
    if(!all(unlist(cons))){
      obj_val <- obj_val + (control$big_m * -obj_coeff)
    }

    obj_val
  }

  r <- RcppDE::DEoptim(fn,
                       lower = lower_bounds,
                       upper = upper_bounds,
                       control = de_control)

  best_params <- as.list(r$optim$bestmem)

  for(k in names(best_params)){
    if(param_types[[k]] == "ParDiscrete"){
      best_params[[k]] <- round(best_params[[k]])
    }
  }

  method_results(method = "de_optim",
                 objective_value = r$optim$bestval * obj_coeff,
                 constraints_satisfied =  abs(r$optim$bestval) < big_m,
                 params = best_params,
                 envs = NULL)


}


