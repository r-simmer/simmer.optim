sim_prob_1<- function(){
  t0<-create_trajectory() %>%
    seize("nurse") %>%
    timeout(function() rpois(1, 10)) %>%
    release("nurse") %>%
    seize("cardiologist") %>%
    timeout(function() rpois(1, 20)) %>%
    release("cardiologist")

  env<-simmer() %>%
    add_generator("patient", t0, at(rep(0, 1000))) %>%
    add_resource("nurse", .opt("nurse")) %>%
    add_resource("cardiologist", .opt("cardiologist")) %>%
    run(until=8*60)

  cost_nurse <- 40
  cost_cardiologist <- 100

  arr_times <- get_mon_arrivals(env)
  wait_times <- arr_times$end_time - arr_times$start_time - arr_times$activity_time

  optim_results(
    objective = NROW(get_mon_arrivals(env)),
    constraints = list(
      # note that this example doesnt take into account hours effectively worked but instead runtime
      budget = .opt("nurse") * cost_nurse * (now(env) / 60) + .opt("cardiologist") * cost_cardiologist * (now(env) / 60)  <= 2000
    ),
    envs = env
  )

}
