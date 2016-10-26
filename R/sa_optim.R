#' A simmer simulated annealing optimizer
#'
#' Implements the functionality of the \code{GenSA} package.
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
sa_optim <- function(model,
                     direction = c("min", "max"),
                     objective,
                     constraints,
                     params,
                     control,
                     big_m = 1e6){

    dep <- requireNamespace("GenSA")
    if (!dep)
      stop("Please install package 'GenSA' before continuing")

    if(is.list(control$sa_optim)){
      sa_optim_ctrl <- control$sa_optim
    } else {
      sa_optim_ctrl <- list()
    }

    obj_coeff <- switch(direction,
                        max = -1,
                        min = 1)

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
        obj_val <- big_m * -obj_coeff
      }

      obj_val
    }

    res <- GenSA::GenSA(
      par = NULL,
      fn = fn,
      lower = as.numeric(lower_bounds),
      upper = as.numeric(upper_bounds),
      control = sa_optim_ctrl
    )


    best_params <- as.list(res$par)
    names(best_params) <- param_names

    for(k in names(best_params)){
      if(param_types[[k]] == "ParDiscrete"){
        best_params[[k]] <- round(best_params[[k]])
      }
    }

    method_results(
      method = "sa_optim",
      objective_value = res$value * obj_coeff,
      constraints_satisfied = res$value != big_m * -obj_coeff,
      params = best_params,
      envs = NULL
    )
  }
