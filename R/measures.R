library(dplyr)
msr_arrivals_finished<-function(envs, agg = mean){
  get_mon_arrivals(envs) %>%
    group_by(replication) %>%
    summarise(finished = n()) %>%
    .$finished %>%
    agg
}


assert_waiting_time_max<-function(envs, max_val){
  get_mon_arrivals(envs) %>%
    mutate(waiting_time = end_time - start_time) %>%
    {
     max(.$waiting_time) < max_val
    }
}
