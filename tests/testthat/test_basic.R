library(simmer.json)
library(simmer)

context("basic functionality")

example_simmer_prob <- function(){
  t0<-create_trajectory() %>%
    seize("nurse") %>%
    timeout(function() rpois(1, 10)) %>%
    release("nurse") %>%
    seize("cardiologist") %>%
    timeout(function() rpois(1, 20)) %>%
    release("cardiologist") %>%
    branch(function() sample(c(1,2), 1),
           merge=c(T,T),
           create_trajectory() %>%
             seize("physiotherapist") %>%
             timeout(function() rpois(1, 45)) %>%
             release("physiotherapist"),
           create_trajectory() %>%
             timeout(0))

  env<-simmer() %>%
    add_generator("patient", t0, at(seq(0,60*4, .opt("IAT")))) %>%
    add_resource("nurse", .opt("nurse")) %>%
    add_resource("cardiologist", .opt("cardiologist")) %>%
    add_resource("physiotherapist", .opt("physiotherapist")) %>%
    run

  standard_cost <- 40
  arr_times <- get_mon_arrivals(env)
  wait_times <- arr_times$end_time - arr_times$start_time - arr_times$activity_time

  optim_results(
    objective = NROW(get_mon_arrivals(env)),
    constraints = list(
      # note that it doesnt take into account hours effectively worked but instead runtime
      budget = (.opt("nurse") + .opt("physiotherapist") + .opt("cardiologist")) * (now(env) / 60) * standard_cost <= 2000,
      max_waiting = wait_times <=  30
    ),
    envs = env
  )


}

test_that("no errors are returned",{
  r<-grid_optim(example_simmer_prob,
                objective = "max",
                nurse = 1:4,
                cardiologist = 1:4,
                physiotherapist = 1:4,
                IAT = c(10, 15, 20, 25))
  print(r$get_results())
})
