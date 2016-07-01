## ------------------------------------------------------------------------
library(simmer)

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
  add_generator("patient", t0, at(seq(0,60*4, 10))) %>%
  add_resource("nurse", 1) %>%
  add_resource("cardiologist", 1) %>%
  add_resource("physiotherapist", 1) %>%
  run(until=4*60)

## ------------------------------------------------------------------------
plot_evolution_arrival_times(env, "waiting_time")

## ------------------------------------------------------------------------
# number of patients
get_mon_arrivals(env) %>% NROW

## ------------------------------------------------------------------------
expand.grid(nurse = 1:4,
            cardiologist = 1:4,
            physiotherapist = 1:4,
            IAT = c(10, 15, 20, 25)) %>% NROW


## ------------------------------------------------------------------------
library(simmer.optim)

the_simmer_env<-function(){
  
  env<-simmer() %>%
    add_generator("patient", t0, at(seq(0,60*4, .opt("IAT")))) %>%
    add_resource("nurse", .opt("nurse")) %>%
    add_resource("cardiologist", .opt("cardiologist")) %>%
    add_resource("physiotherapist", .opt("physiotherapist")) %>%
    run(until=4*60)
  
  standard_cost <- 40
  arr_times <- get_mon_arrivals(env)
  wait_times <- arr_times$end_time - arr_times$start_time - arr_times$activity_time
  
  list(
    objective = NROW(get_mon_arrivals(env)),
    constraints = list(
      # note that it doesn't take into account the hours effectively worked but instead runtime
      budget = (.opt("nurse") + .opt("physiotherapist") + .opt("cardiologist")) * (now(env) / 60) * standard_cost <= 2000,
      max_waiting = wait_times <=  30
    )
  )
}

optimal_grid<-
  grid_optim(the_simmer_env, objective = "max", 
             nurse = 1:4,
             cardiologist = 1:4,
             physiotherapist = 1:4,
             IAT = c(10, 15, 20, 25))

optimal_params<-
  optimal_grid$get_results()

optimal_params

## ------------------------------------------------------------------------
env<-simmer() %>%
    add_generator("patient", t0, at(seq(0,60*4, optimal_params$IAT))) %>%
    add_resource("nurse", optimal_params$nurse) %>%
    add_resource("cardiologist", optimal_params$cardiologist) %>%
    add_resource("physiotherapist", optimal_params$physiotherapist) %>%
    run(until=4*60)

# number of patients
get_mon_arrivals(env) %>% NROW

# waiting time
plot_evolution_arrival_times(env, "waiting_time")

