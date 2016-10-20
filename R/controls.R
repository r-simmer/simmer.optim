#' @export
optim_control<-function(run_args = list(until = 1000),
                        replications = 1,
                        ...){
  default_args<-
    list(run_args = run_args,
         replications = replications)

  optional_args<-
    list(...)

  c(default_args,
    optional_args)
}

