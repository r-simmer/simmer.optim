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


