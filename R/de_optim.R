#' A simmer differential evoltion optimizer
#'
#' Implements the functionality of the \code{DEoptim} package.
#'
#' @param model the simmer model encapsulated in a function
#' @param direction optimization direction (\code{max} or \code{min})
#' @param objective the objective function
#' @param constraints a list of constraint functions
#' @param params a list of parameters to optimize over
#' @param control a control list created by a call to \code{optim_control()}
#' @param big_m a penalty value for solutions with non-satisfied constraints
#'
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
    if(methods::is(x, "ParDiscrete")){
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
      obj_val <- big_m * -obj_coeff
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
                 constraints_satisfied = r$optim$bestval != big_m * -obj_coeff,
                 params = best_params,
                 envs = NULL)


}


