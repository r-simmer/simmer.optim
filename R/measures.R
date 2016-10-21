#' @export
#' @import dplyr
msr_arrivals_finished<-function(envs, agg = mean){
  get_mon_arrivals(envs) %>%
    group_by(replication) %>%
    summarise(finished = n()) %>%
    .$finished %>%
    agg
}


#' @export
msr_runtime<-function(envs, agg = mean){
  lapply(envs, simmer::now) %>%
    unlist %>%
    agg
}


#' @export
msr_resource_amount<-function(envs, name, agg = mean){
  lapply(envs, function(e) simmer::get_capacity(e, name)) %>%
    unlist %>%
    agg
}

#' @export
msr_resource_utilization<-function(envs, name, agg=mean){
  tmp<-
    get_mon_resources(envs) %>%
    group_by(resource, replication) %>%
    arrange(resource, time, replication) %>%
    mutate(server_prev  = lag(server, default = 0),
           next_time = lead(time),
           step_time = next_time - time,
           usage_time_weighted = step_time * server,
           non_usage_time_weighted = ifelse(server == 0, step_time, 0)) %>%
    summarise(utilization = sum(usage_time_weighted, na.rm=T) / sum(non_usage_time_weighted, usage_time_weighted, na.rm=T)) %>%
    group_by(resource) %>%
    summarise(utilization = agg(utilization)) %>%
    filter(resource==name) %>%
    .$utilization
}
