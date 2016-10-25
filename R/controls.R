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

