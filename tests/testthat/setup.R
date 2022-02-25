library(simmer)

sim_prob_1 <- function() {
  t0 <- trajectory() %>%
    seize("nurse") %>%
    timeout(function()
      rpois(1, 10)) %>%
    release("nurse") %>%
    seize("cardiologist") %>%
    timeout(function()
      rpois(1, 20)) %>%
    release("cardiologist")

  env <- simmer() %>%
    add_generator("patient", t0, at(rep(0, 1000))) %>%
    add_resource("nurse", .opt("nurse")) %>%
    add_resource("cardiologist", .opt("cardiologist"))

}

sim_prob_1_constraint<-
  function(envs){
    cost_nurse <- 40
    cost_cardiologist <- 100

    total_cost <-
      msr_runtime(envs)/60 * msr_resource_capacity(envs, "nurse") * cost_nurse +
      msr_runtime(envs)/60 * msr_resource_capacity(envs, "cardiologist") * cost_cardiologist

    total_cost < 2000
  }

sim_prob_2 <- function() {
  t0 <- trajectory() %>%
    seize("nurse") %>%
    timeout(function()
      rpois(1, 10)) %>%
    release("nurse") %>%
    seize("cardiologist") %>%
    timeout(function()
      rpois(1, 20)) %>%
    release("cardiologist")

  envs <- lapply(1:20, function(i) {
    simmer() %>%
      add_generator("patient", t0, at(rep(0, 1000))) %>%
      add_resource("nurse", .opt("nurse")) %>%
      add_resource("cardiologist", .opt("cardiologist"))
      # run(until = 8 * 60)
  })

  # cost_nurse <- 40
  # cost_cardiologist <- 100
  #
  # finished_avg <-
  #   mean(sapply(envs, function(env)
  #     NROW(get_mon_arrivals(env))))
  # now_avg <- mean(sapply(envs, now))
  #
  # optim_results(
  #   objective = finished_avg,
  #   constraints = list(
  #     # note that this example doesnt take into account hours effectively worked but instead runtime
  #     budget = .opt("nurse") * cost_nurse * (now_avg / 60) +
  #       .opt("cardiologist") * cost_cardiologist * (now_avg / 60)  <= 2000
  #   ),
  #   envs = envs
  # )

}


sim_prob_3 <- function() {
  t0 <- trajectory() %>%
    seize("nurse") %>%
    timeout(function()
      rpois(1, 10)) %>%
    release("nurse") %>%
    seize("cardiologist") %>%
    timeout(function()
      rpois(1, 20)) %>%
    release("cardiologist") %>%
    branch(
      function()
        sample(c(1, 2), 1),
      merge = c(T, T),
      trajectory() %>%
        seize("physiotherapist") %>%
        timeout(function()
          rpois(1, 45)) %>%
        release("physiotherapist"),
      trajectory() %>%
        timeout(0)
    )

  env <- simmer() %>%
    add_generator("patient", t0, at(seq(0, 60 * 4, .opt("IAT")))) %>%
    add_resource("nurse", .opt("nurse")) %>%
    add_resource("cardiologist", .opt("cardiologist")) %>%
    add_resource("physiotherapist", .opt("physiotherapist"))

  # standard_cost <- 40
  # arr_times <- get_mon_arrivals(env)
  # wait_times <-
  #   arr_times$end_time - arr_times$start_time - arr_times$activity_time
  #
  # optim_results(
  #   objective = NROW(get_mon_arrivals(env)),
  #   constraints = list(
  #     # note that it doesnt take into account hours effectively worked but instead runtime
  #     budget = (.opt("nurse") + .opt("physiotherapist") + .opt("cardiologist")) * (now(env) / 60) * standard_cost <= 2000,
  #     max_waiting = wait_times <=  30
  #   ),
  #   envs = env
  # )

}
