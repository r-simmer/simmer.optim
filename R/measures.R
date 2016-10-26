#' Measure the number of finished arrivals
#'
#' @param envs a list of \code{envs} as produced by \code{run_instance}
#' @param agg the method of aggregation of per replication results
#'
#' @export
#' @import dplyr
msr_arrivals_finished<-function(envs, agg = mean){
  get_mon_arrivals(envs) %>%
    group_by_("replication") %>%
    summarise(finished = n()) %>%
    .$finished %>%
    agg
}


#' Measure the runtime of the model
#'
#' @param envs a list of \code{envs} as produced by \code{run_instance}
#' @param agg the method of aggregation of per replication results
#'
#' @export
msr_runtime<-function(envs, agg = mean){
  lapply(envs, simmer::now) %>%
    unlist %>%
    agg
}


#' Measure the capacity of a resource type
#'
#' @param envs a list of \code{envs} as produced by \code{run_instance}
#' @param name the name of the resource
#' @param agg the method of aggregation of per replication results
#'
#' @export
msr_resource_capacity<-function(envs, name, agg = mean){
  lapply(envs, function(e) simmer::get_capacity(e, name)) %>%
    unlist %>%
    agg
}

#' Measure the utilization of a resource type
#'
#' @param envs a list of \code{envs} as produced by \code{run_instance}
#' @param name the name of the resource
#' @param agg the method of aggregation of per replication results
#'
#' @export
msr_resource_utilization<-function(envs, name, agg=mean){
  tmp <-
    get_mon_resources(envs) %>%
    group_by_("resource", "replication") %>%
    arrange_("resource", "time", "replication") %>%
    mutate_("server_prev"  = lag("server", default = 0),
            "next_time" = lead("time"),
            "step_time" = "next_time" - "time",
            "usage_time_weighted" = "step_time" * "server",
            "non_usage_time_weighted" = ifelse("server" == 0, "step_time", 0)) %>%
    summarise_(utilization = sum("usage_time_weighted", na.rm=T) / sum("non_usage_time_weighted", "usage_time_weighted", na.rm=T)) %>%
    group_by_("resource") %>%
    summarise_(utilization = agg("utilization")) %>%
    filter(resource==name)

  tmp$utilization
}
