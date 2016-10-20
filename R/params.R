#' @export
par_discrete<-function(vec){
  class(vec) <- c("ParDiscrete", class(vec))
  vec
}

#' @export
par_continuous<-function(vec){
  vec <- as.numeric(vec)
  class(vec) <- c("ParContinuous", class(vec))
  vec
}
