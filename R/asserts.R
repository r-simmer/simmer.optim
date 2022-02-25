#' Assert that waiting time < max_val
#'
#' @param envs a list of \code{envs} as produced by \code{run_instance}
#' @param max_val the max waiting time duration
#'
#' @export
assert_waiting_time_max<-function(envs, max_val){
  tmp <-
    get_mon_arrivals(envs) %>%
    dplyr::mutate_("waiting_time" = "end_time - (start_time + activity_time)")

  max(tmp$waiting_time) < max_val

}


#' Assert that average waiting time < max_val
#'
#' @param envs a list of \code{envs} as produced by \code{run_instance}
#' @param max_val the max waiting time duration
#'
#' @export
assert_avg_waiting_time_max<-function(envs, max_val){
  tmp <-
    get_mon_arrivals(envs) %>%
    dplyr::mutate_("waiting_time" = "end_time - (start_time + activity_time)") %>%
    dplyr::summarise_("waiting_time_avg" = "mean(waiting_time)")

  tmp$waiting_time_avg < max_val
}

