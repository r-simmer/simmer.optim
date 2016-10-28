#' Measure the number of finished arrivals
#'
#' @param envs a list of \code{envs} as produced by \code{run_instance}
#' @param agg the method of aggregation of per replication results
#'
#' @export
msr_arrivals_finished<-function(envs, agg = mean){
  tmp <- simmer::get_mon_arrivals(envs) %>%
    dplyr::group_by_("replication") %>%
    dplyr::summarise(finished = n())

  tmp$finished %>%
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
    simmer::get_mon_resources(envs) %>%
    dplyr::group_by_("resource", "replication") %>%
    dplyr::arrange_("resource", "time", "replication") %>%
    dplyr::mutate_("server_prev"  = lag("server", default = 0),
            "next_time" = dplyr::lead("time"),
            "step_time" = "next_time" - "time",
            "usage_time_weighted" = "step_time" * "server",
            "non_usage_time_weighted" = ifelse("server" == 0, "step_time", 0)) %>%
    dplyr::summarise_(utilization = sum("usage_time_weighted", na.rm=T) / sum("non_usage_time_weighted", "usage_time_weighted", na.rm=T)) %>%
    dplyr::group_by_("resource") %>%
    dplyr::summarise_(utilization = agg("utilization")) %>%
    dplyr::filter_("resource"==name)

  tmp$utilization
}
