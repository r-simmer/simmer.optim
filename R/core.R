#' Instantiate a simmer expression
#'
#' @param model the simmer model
#' @param rep the number of replications
#' @param ... named parameters to be passed to the simmer expression and accessbile through the \code{.opt} variable
#'
#' @export
run_instance <- function(model, rep=1, ...){

  temp_env <- new.env(parent = globalenv())
  assign(".opt", opt_func(...), envir = temp_env)
  lapply(1:rep, function(i){
    simmer::run(eval(body(model), envir = temp_env))
  })
}



#' @export
objective_evaluator<-function(envs, objective){
  if(is(objective, "funcArgs")){
    do.call(objective$f, c(list(envs=envs), objective$args))
  } else {
    do.call(objective, list(envs=envs))
  }

}

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

#' @export
with_args<-function(f, ...){
  x <- list(f = f,
            args = list(...))
  class(x)<-c("funcArgs", class(x))
  x
}


#' @export
method_results<-function(method, objective_value, constraints_satisfied, params = list(), envs = NULL, extra_info = list()){
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
simmer_optim <- function(model,
                         method,
                         direction = "max",
                         objective = msr_arrivals_finished,
                         constraints,
                         rep = 1,
                         ...){


  r<-method(model, direction, objective, constraints, rep, ...)
  if(!is(r, "MethodResults"))
    stop("Optimization method should return a MethodResults object (created by 'method_results')")

}

#' @export
print.MethodResults<-function(x, ...){
  cat("simmer.optim result", "\n")
  cat("===\n")

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
