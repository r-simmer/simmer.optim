

#' Function to pass results of simmer evaluation back to the optimization framework
#'
#' @param objective the value of the objective
#' @param constraints a list with named objectives containing only TRUE and FALSE values
#' @param envs the simmer env (OPTIONAL)
#'
#' @export
#'
#' @examples
optim_results <- function(objective, constraints = list(), envs=NA){
  res<-list(objective=objective,
            constraints=constraints,
            envs=envs)
  class(res) <- "OptimResults"
  res
}




#' Value function generator
#'
#' Internal usage
#'
#' @param search_grid the grid to search over
#' @param index the requested index
#'
#' @return a function which supplies the variables to test
opt_func <- function(...){
  opts = list(...)
  function(name) opts[[name]]
}
