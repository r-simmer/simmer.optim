#' A parameter vector of type discrete
#'
#' @param vec the original vector
#' @export
par_discrete<-function(vec){
  class(vec) <- c("ParDiscrete", class(vec))
  vec
}

#' A parameter vector of type continuous
#'
#' @param vec the original vector
#' @export
par_continuous<-function(vec){
  vec <- as.numeric(vec)
  class(vec) <- c("ParContinuous", class(vec))
  vec
}
