*generated with version 0.1.0*

DES & parameter optimization
----------------------------

Discrete-event simulation (DES) is very useful in analyzing processes
(amongst other application). Using DES one could for example give an
answer to the question:

> If we have *x* amount of resources of type *A*, what will the average
> waiting time in the process be?

Using `simmer` this becomes a problem that is quite easy to answer, but
what if we rephrase the question?

> What amount *x* of resources of type *A* minimizes the waiting time,
> while still maintaining a utilization level of *ρ*<sub>*A*</sub>?

This question is less straight forward to answer. One way of going about
this is to manually adjust the value of *x* and rerun the simulation for
each value of *x*<sub>*i*</sub>, ..., *x*<sub>*n*</sub> and evaluate the
results after each run. While definitely not impossilbe to do this for
*x*, it becomes quite bothersome when doing the same exercise not only
for *x*, but also for *y* and *z*.

Another approach would be to use a parameter optimization method.
`simmer.optim` is an implementation of a parameter optimization method
specifically built as a plugin for `simmer`.

For the purpose of this vignette, we work with the following key
definitions:

-   objective: a variable in the simulated process that we want to
    minimize or maximize
-   constraints: upper and lower boundaries on variables in the
    simulation
-   inputs: the simulation parameters that we want to manipulate in
    order to achieve a minimization / maximization of the objective

A minimal example
-----------------

See the basic `simmer` example below.

    library(simmer)
    library(dplyr)

    t0<-create_trajectory() %>%
      seize("nurse") %>%
      timeout(function() rpois(1, 10)) %>%
      release("nurse") %>%
      seize("cardiologist") %>%
      timeout(function() rpois(1, 20)) %>%
      release("cardiologist")

    env <- simmer() %>%
      add_generator("patient", t0, at(seq(0,60*4, 15))) %>%
      add_resource("nurse", 1) %>%
      add_resource("cardiologist", 1) %>%
      run()

In this example, a patient first sees a nurse, followed by a visit to
the cadiologist. The consultation is planned to last 4 hours (or 240
minutes).

In the plot below, we can see that the process isn't very stable.
Moreover it lasts a good bit more than 4 hours.

    plot_evolution_arrival_times(env, "waiting_time")

![](/Users/bart/src/simmer.optim/README_files/figure-markdown_strict/unnamed-chunk-3-1.png)

The number of patients served in this process is:

    get_mon_arrivals(env) %>%
      nrow()

    ## [1] 17

While the number of patients served before the planned end of the
consultation (after 4 hours) is:

    get_mon_arrivals(env) %>%
      filter(end_time < 4*60) %>%
      nrow()

    ## [1] 12

### Optimizing the situation

The hospital wants to reorganize the process. The process requirements
are specified as follows:

-   a maximization of the number of patients served before the 4 hour
    mark has been reached (= **objective**)
-   an employee budget of 1000€ is available for the consultation (=
    **constraint**)
-   rate nurse: 50€ / hr
-   rate cardiologist: 100€ / hr
-   a maximum average waiting time of 30 minutes is allowed (=
    **constraint**)

To optimize the scenario we are going to check a number of different
values for the following variables.

-   the resource capacity: a range of `1:4` for each resource
-   the inter-arrival time: a vector of `c(10, 15, 20, 25)`

For this small problem, testing all the possiblities by hand will become
somewhat labourious:

    expand.grid(nr_nurses = 1:4,
                nr_cardiologists = 1:4,
                interarrival_time = c(10, 15, 20, 25)) %>% nrow()

    ## [1] 64

In other words, we would have to re-run the simulation 64 times to test
each possible combinations. By hand this is quite cumbersome, but
`simmer.optim` tries to ease this process.

### Finding the optimal combination

At the moment there are three types of parameter optimization methods
available: grid optimization (extensively testing all combinations),
differential evolution and simulated annealing. For this example we will
apply the grid optimization method.

We start by taking the same simulation defintion, but wrapping it in a
function. We also add make sure that it returns an `optim_result`
object. In the `optim_result` we send back the value of the objective
and the constraints.

The objective in this case is the value of
`patients_served_before_4hrs`. For the constraints, a `TRUE` or `FALSE`
value has to be returned. A constraint which returns `TRUE` means that
the constraint is satisfied.

    library(simmer.optim)

    sim_case<-function(){
      t0<-create_trajectory() %>%
        seize("nurse") %>%
        timeout(function() rpois(1, 10)) %>%
        release("nurse") %>%
        seize("cardiologist") %>%
        timeout(function() rpois(1, 20)) %>%
        release("cardiologist")
      
      env<-simmer() %>%
        add_generator("patient", t0, at(seq(0,60*4, .opt("interarrival_time")))) %>%
        add_resource("nurse", .opt("nr_nurses")) %>%
        add_resource("cardiologist", .opt("nr_cardiologists")) %>%
        run()
      
      cost_nurse <- 4 * 50 * .opt("nr_nurses")
      cost_cardiologist <- 4 * 100 * .opt("nr_cardiologists")
      
      mon_arrivals <- get_mon_arrivals(env)
      
      patients_served_before_4hrs <- 
        mon_arrivals %>% filter(end_time < 4*60) %>% nrow()
      
      patients_waiting_time <-
        mean(mon_arrivals$end_time - mon_arrivals$start_time - mon_arrivals$activity_time)
        
      
      optim_results(
        objective = patients_served_before_4hrs,
        constraints = list(
          employee_budget = (cost_nurse + cost_cardiologist) <= 1000,
          avg_waiting_time = patients_waiting_time < 30
        )
      )
      
    }

Below, we use the grid optimization method to find the best combination
of the three variables `nr_nurses`, `nr_cardiologists` and
`interarrival_time` in order to maximize the object.

    set.seed(2016)
    r<-grid_optim(sim_expr = sim_case,
                  objective = "max",
                  nr_nurses = 1:4,
                  nr_cardiologists = 1:4,
                  interarrival_time = c(10, 15, 20, 25))


    results(r)          

    ## Optimization result
    ## ========================
    ## Objective value:  20 
    ## 
    ## Params:
    ##  -- nr_nurses: 1
    ##  -- nr_cardiologists: 2
    ##  -- interarrival_time: 10

We see from calling `results(r)` that, given the constraints, the
maximum number of patients can be served before the 4 hours are up is
21. The output also shows the paramaters which generate this output.

    results(r)$params

    ## $nr_nurses
    ## [1] 1
    ## 
    ## $nr_cardiologists
    ## [1] 2
    ## 
    ## $interarrival_time
    ## [1] 10

### Replicated simulations

Often you want to optimize the parameters to for a specific run of the
simulation, but over *n* replications of the simulation. Below the
example of above is adapted to show how this can be achieved.

    sim_case_replicated<-function(){
      t0<-create_trajectory() %>%
        seize("nurse") %>%
        timeout(function() rpois(1, 10)) %>%
        release("nurse") %>%
        seize("cardiologist") %>%
        timeout(function() rpois(1, 20)) %>%
        release("cardiologist")
      
      envs <- lapply(1:10, function(i){
        simmer() %>%
          add_generator("patient", t0, at(seq(0,60*4, .opt("interarrival_time")))) %>%
          add_resource("nurse", .opt("nr_nurses")) %>%
          add_resource("cardiologist", .opt("nr_cardiologists")) %>%
          run()
      })
      
      cost_nurse <- 4 * 50 * .opt("nr_nurses")
      cost_cardiologist <- 4 * 100 * .opt("nr_cardiologists")
      
      mon_arrivals <- get_mon_arrivals(envs)
      
      patients_served_before_4hrs <- 
        mon_arrivals %>%
        group_by(replication) %>%
        filter(end_time < 4*60) %>% 
        summarise(nr_patients = n()) %>%
        .$nr_patients %>% mean

      patients_waiting_time <-
        mean(mon_arrivals$end_time - mon_arrivals$start_time - mon_arrivals$activity_time)
        
      
      optim_results(
        objective = patients_served_before_4hrs,
        constraints = list(
          employee_budget = (cost_nurse + cost_cardiologist) <= 1000,
          avg_waiting_time = patients_waiting_time < 30
        )
      )
      
    }


    set.seed(2017)
    r<-grid_optim(sim_expr = sim_case_replicated,
                  objective = "max",
                  nr_nurses = 1:4,
                  nr_cardiologists = 1:4,
                  interarrival_time = c(10, 15, 20, 25))


    results(r) 

    ## Optimization result
    ## ========================
    ## Objective value:  19.9 
    ## 
    ## Params:
    ##  -- nr_nurses: 1
    ##  -- nr_cardiologists: 2
    ##  -- interarrival_time: 10

As we can see, the results are pretty similar and seem to confirm our
earlier findings.

Available optimizers
--------------------

For now, three different optimizers are available:

### Grid optimization

The grid optimization
[algorithm](https://en.wikipedia.org/wiki/Hyperparameter_optimization#Grid_search)
starts by building a grid with all the possible combination of the
values you supply to it. In essence this comes down to an exhausive
search of the parameter space. The upside of this is that it will always
return the most optimum value, the downside is that it can take a long
time to find this optimal value.

    r <- grid_optim(sim_case,
                    objective = "max",
                    nr_nurses = 1:2,
                    nr_cardiologists = 1:2,
                    interarrival_time = c(10, 15))

    results(r)

    ## Optimization result
    ## ========================
    ## Objective value:  20 
    ## 
    ## Params:
    ##  -- nr_nurses: 1
    ##  -- nr_cardiologists: 2
    ##  -- interarrival_time: 10

### Simulated annealing

Simulated annealing is a [heuristic
method](https://en.wikipedia.org/wiki/Simulated_annealing) that *tries*
to find a good solution. Finding the optimum combination of parameters
is however not guaranteed. `simmer.optim` leverages the `GenSA` package
to perform simmulated annealing. Note that it is important to
explicitely pass `integer` type values if the value has to be
interpreted as an integer, this is due to the fact that a cardinality
constraint is applied to the parameter in the optimization procedure.

    r <- sa_optim(sim_case,
                  objective = "max",
                  nr_nurses = 1:2,
                  nr_cardiologists = 1:2,
                  interarrival_time = c(10, 15) %>% as.integer,
                  control = list(maxit = 10))

    results(r)

    ## Optimization result
    ## ========================
    ## Objective value:  19 
    ## 
    ## Params:
    ##  -- nr_nurses: 1
    ##  -- nr_cardiologists: 2
    ##  -- interarrival_time: 12

### Differential evolution

Differential evolition is a [heuristic
method](https://en.wikipedia.org/wiki/Differential_evolution) that
*tries* to find a good solution. Finding the optimum combination of
parameters is however not guaranteed. `simmer.optim` leverages the
`RcppDE` package to perform differential evolution. Note that it is
important to explicitely pass `integer` type values if the value has to
be interpreted as an integer, this is due to the fact that a cardinality
constraint is applied to the parameter in the optimization procedure.

    r <- de_optim(sim_case,
                  objective = "max",
                  nr_nurses = 1:2,
                  nr_cardiologists = 1:2,
                  interarrival_time = c(10, 15) %>% as.integer,
                  deoptim_control = RcppDE::DEoptim.control(itermax = 10, 
                                                            trace=F))

    results(r)

    ## Optimization result
    ## ========================
    ## Objective value:  22 
    ## 
    ## Params:
    ##  -- nr_nurses: 1
    ##  -- nr_cardiologists: 2
    ##  -- interarrival_time: 10
