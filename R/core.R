#' @export
simmer_optim <- function(model,
                         method,
                         direction = "max",
                         objective = msr_arrivals_finished,
                         constraints,
                         params = list(),
                         control = optim_control(),
                         ...){

  if (length(params) == 0)
    stop("Please supply parameters to optimize over.")

  control <- modifyList(optim_control(), control)

  r<-method(model = model,
            direction = direction,
            objective = objective,
            constraints = constraints,
            params = params,
            control = control,
            ...)

  if(!is(r, "MethodResults"))
    stop("Optimization method should return a MethodResults object (created by 'method_results')")

  r

}

#' Run n simmer models
#'
#' @param model the simmer model
#' @param control the \code{optim_control} object
#' @param params a list of named parameters to be passed to the simmer expression and accessbile for the model through the \code{.opt} variable
#'
#' @import simmer
#' @export
run_instance <- function(model, control, params){
  rep <- control$replications
  run_args <- control$run_args

  temp_env <- new.env(parent = globalenv())
  assign(".opt", opt_func(params), envir = temp_env)

  if(control$parallel){
    envs <- parallel::mclapply(1:rep, function(i){
      env <- do.call(simmer::run, c(
        list(env = eval(body(model), envir = temp_env)),
        run_args))
      simmer::wrap(env)
    }, mc.set.seed = F)
  } else {
    envs <- lapply(1:rep, function(i){
      do.call(simmer::run, c(
        list(env = eval(body(model), envir = temp_env)),
        run_args))
    })
  }
  envs

}

#' Evaluator for the objective function
#'
#' @param envs a list of \code{envs} as produced by \code{run_instance}
#' @param objective an objective function
#'
#' @export
objective_evaluator<-function(envs, objective){
  if(is(objective, "funcArgs")){
    do.call(objective$f, c(list(envs=envs), objective$args))
  } else {
    do.call(objective, list(envs=envs))
  }

}

#' Evaluator for the constraint functions
#' @param envs a list of \code{envs} as produced by \code{run_instance}
#' @param constraints a list of constraint functions
#'
#' @export
constraints_evaluator<-function(envs, constraints){

  if(!is.list(constraints))
    stop("Please supply a list of constraint evaluation functions")

  lapply(1:length(constraints), function(i){
    if(is(constraints[[i]], "funcArgs")){
      do.call(constraints[[i]]$f, c(list(envs = envs), constraints[[i]]$args))
    } else {
      do.call(constraints[[i]], list(envs=envs))
    }
  })
}

#' Helper function to run objective / constraint functions with specified arguments
#'
#' @param f the objective / constraint function
#' @param ... a list of named arguments which will be used in the call to \code{f}
#'
#' @export
with_args<-function(f, ...){
  x <- list(f = f,
            args = list(...))
  class(x)<-c("funcArgs", class(x))
  x
}


#' A helper function to return the results of an optimization method to the optimization framework
#'
#' @param method the name of the optimization function (string)
#' @param objective_value the value of the objective
#' @param constraints_satisfied boolean indicating whether or not all constraints where satisfied
#' @param params the found parameters
#' @param envs a copy of the generated envs (optional)
#' @param extra_info a list of extra information (optional)
#'
#' @export
method_results<-function(method, objective_value, constraints_satisfied, params, envs = NULL, extra_info = list()){
  r <- list(method = method,
            objective_value = objective_value,
            constraints_satisfied = constraints_satisfied,
            params = params,
            envs = envs,
            extra_info = extra_info)
  class(r) <- c("MethodResults", class(r))
  r
}



#' @export
print.MethodResults<-function(x, ...){
  cat("simmer.optim result", "\n")
  cat("\n")
  cat("method:                ", x$method, "\n")
  cat("objective value:       ", x$objective_value, "\n")
  cat("constraints satisfied: ", x$constraints_satisfied, "\n\n")
  cat("params:", "\n")
  if(length(x$params)>0){
    for(key in names(x$params)){
      cat("> ", key, ": ",  x$params[[key]], "\n")
    }
  }
  cat("\n")
  if(length(x$extra_info) > 0){
    cat("extra details", "\n")
    for(key in names(x$extra_info)){
      cat("  ", key, ": ",  x$extra_info[[key]], "\n")
    }
  }
}

