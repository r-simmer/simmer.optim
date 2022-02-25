msr_arrivals_<-function(envs, agg){
  get_mon_arrivals(envs, ongoing = TRUE) %>%
    dplyr::group_by_("replication", "finished") %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    tidyr::complete_(list("replication", "finished" = "c(TRUE, FALSE)"),
                     fill = list(n = 0)) %>%
    dplyr::group_by_("finished") %>%
    dplyr::summarise_(.dots = stats::setNames(list(~agg(n)), "agg"))
}

#' Measure the number of finished arrivals
#'
#' @param envs a list of \code{envs} as produced by \code{run_instance}
#' @param agg the method of aggregation of per replication results
#'
#' @export
msr_arrivals_finished<-function(envs, agg = mean){
  tmp <- msr_arrivals_(envs, agg) %>%
    dplyr::filter_("finished == TRUE")

  tmp$agg
}

#' Measure the number of rejected arrivals
#'
#' @param envs a list of \code{envs} as produced by \code{run_instance}
#' @param agg the method of aggregation of per replication results
#'
#' @export
msr_arrivals_rejected<-function(envs, agg = mean){
  tmp <- msr_arrivals_(envs, agg) %>%
    dplyr::filter_("finished == FALSE")

  tmp$agg
}


#' Measure the runtime of the model
#'
#' @param envs a list of \code{envs} as produced by \code{run_instance}
#' @param agg the method of aggregation of per replication results
#'
#' @export
msr_runtime<-function(envs, agg = mean){
  lapply(envs, now) %>%
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
  lapply(envs, function(e) get_capacity(e, name)) %>%
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
    dplyr::group_by_("resource", "replication") %>%
    dplyr::arrange_("resource", "time", "replication") %>%
    dplyr::mutate_("server_prev"  = "lag(server, default = 0)",
                   "next_time" = "lead(time)",
                   "step_time" = "next_time - time",
                   "usage_time_weighted" = "step_time" * "server",
                   "non_usage_time_weighted" = ifelse("server" == 0, "step_time", 0)) %>%
    dplyr::summarise_(utilization = sum("usage_time_weighted", na.rm=T) / sum("non_usage_time_weighted", "usage_time_weighted", na.rm=T)) %>%
    dplyr::group_by_("resource") %>%
    dplyr::summarise_(utilization = agg("utilization")) %>%
    dplyr::filter_("resource"==name)

  tmp$utilization
}
