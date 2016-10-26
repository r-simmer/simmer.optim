#' Assert that waiting time < max_val
#'
#' @param envs a list of \code{envs} as produced by \code{run_instance}
#' @param max_val the max waiting time duration
#'
#' @export
assert_waiting_time_max<-function(envs, max_val){
  tmp <-
    get_mon_arrivals(envs) %>%
    mutate_("waiting_time" = "end_time" - "start_time")

  max(tmp$waiting_time) < max_val

}
