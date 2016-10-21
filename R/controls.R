#' @export
optim_control<-function(run_args = list(until = 1000),
                        replications = 1,
                        parallel = FALSE,
                        ...){
  default_args<-
    list(run_args = run_args,
         replications = replications,
         parallel = parallel)

  optional_args<-
    list(...)

  c(default_args,
    optional_args)
}

