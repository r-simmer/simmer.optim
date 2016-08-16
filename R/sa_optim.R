
SAOptim<-R6::R6Class(
  "SAOptim",
  inherit=SimmerOptim,
  public=list(
    big_M=1e9,
    obj_coeff = NULL,
    lower_bounds = NULL,
    upper_bounds = NULL,
    print = function(){
      cat("A SimmerOptim instance of type Simulated Annealing\n")
    },
    initialize = function(sim_expr, objective = c("min","max"), control = list(), ...){
      objective <- match.arg(objective)
      super$initialize(sim_expr, objective)

      self$obj_coeff <- switch(objective,
                               max=-1,
                               min=1)

      if(length(list(...)) == 0) stop("Please supply parameters to optimize over.")

      args<-list(...)
      self$lower_bounds <- sapply(args, function(x) x[1])
      self$upper_bounds <- sapply(args, function(x) x[2])

      self$optimize(control)
    },
    convert_obj_value = function(results){
      constraints_met = all(unlist(results$constraints))
      if(!constraints_met) {
        self$big_M * -self$obj_coeff
      } else {
        results$objective * self$obj_coeff
      }
    },
    optimize = function(control){

      fn <- function(func_params){
        arg_names = names(self$lower_bounds)
        named_args = list()
        for(i in seq_along(arg_names)){
          named_args[[arg_names[i]]] = round(func_params[[i]])
        }
        obj <- do.call(super$run_instance, named_args)

        self$convert_obj_value(obj)
      }

      res<-GenSA::GenSA(par = NULL,
                        fn = fn,
                        lower = self$lower_bounds,
                        upper = self$upper_bounds,
                        control = control)

      params <- round(res$par)
      names(params) <- names(self$lower_bounds)

      super$results(objective = res$value * self$obj_coeff,
                    params = params,
                    iters = res$counts)
    }
  )
)


#' A simmer differential evoltion optimizer
#'
#' Implements the functionality of the \code{DEoptim} package.
#'
#' @param sim_expr a function which runs a simmer env and returns a list with at least the keys \code{objective} and \code{constraints}
#' @param objective the type of objective to focus on, for now only \code{minimize} and \code{maximize}
#' @param ... the named arguments and per argument an vector of length two defining the lower and upper bound of the search space, e.g. \code{var1=c(1,5), var2=c(3,5)}
#'
#' @return the optimal combination of the variable possibilities supplied in \code{...}
#' @import R6
#' @export
sa_optim<-function(sim_expr, objective = c("min", "max"), ...){
  SAOptim$new(sim_expr, objective, ...)
}
