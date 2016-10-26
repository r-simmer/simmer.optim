#' Control object to configure the optimization procedure
#'
#' @param run_args the run arguments, a list with the following arguments: \code{until}
#' @param replications the number of replications
#' @param parallel whether or not replications should be run in parallel (leveraging \code{mclapply})
#' @param ... extra named arguments added to the control object
#'
#' @export
optim_control<-function(run_args = list(until = 1000),
                        replications = 1,
                        parallel = FALSE,
                        verbose = FALSE,
                        ...){
  default_args<-
    list(run_args = run_args,
         replications = replications,
         parallel = parallel,
         verbose = verbose)

  optional_args<-
    list(...)

  c(default_args,
    optional_args)
}

