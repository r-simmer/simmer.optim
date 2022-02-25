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

    t0<-trajectory() %>%
      seize("nurse") %>%
      timeout(function() rpois(1, 10)) %>%
      release("nurse") %>%
      seize("cardiologist") %>%
      timeout(function() rpois(1, 20)) %>%
      release("cardiologist")

    envs <- lapply(1:100, function(i){
      simmer() %>%
        add_generator("patient", t0, at(seq(0,60*4, 15))) %>%
        add_resource("nurse", 1) %>%
        add_resource("cardiologist", 1) %>%
        run()
    })

In this example, a patient first sees a nurse, followed by a visit to
the cadiologist. The consultation is planned to last 4 hours (or 240
minutes).

In the plot below, we can see that the process isn't very stable.
Moreover it lasts a good bit more than 4 hours.

    library(simmer.plot)

    plot(get_mon_arrivals(envs), "waiting_time")

![](README_files/figure-markdown_strict/unnamed-chunk-3-1.png)

The number of patients served in this process is:

    get_mon_arrivals(envs) %>%
      group_by(replication) %>%
      summarise(n=n()) %>%
      .$n %>% mean

    ## [1] 17

While the number of patients served before the planned end of the
consultation (after 4 hours) is:

    get_mon_arrivals(envs) %>%
      filter(end_time < 4*60) %>%
      group_by(replication) %>%
      summarise(n=n()) %>%
      .$n %>% mean

    ## [1] 11.04

And the average waiting time of the patients served before the planned
end of the consultation:

    get_mon_arrivals(envs) %>%
      filter(end_time < 4*60) %>%
      mutate(waiting_time = end_time - start_time ) %>%
      .$waiting_time %>% mean

    ## [1] 54.44837

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

We start by taking the same definition of the simulation model, but
wrapping it in a function. We wait with calling `run(env)` on the model,
as the optimization framework willt take care of this for us.

    library(simmer.optim)

    sim_model<-function(){
      t0<-trajectory() %>%
        seize("nurse") %>%
        timeout(function() rpois(1, 10)) %>%
        release("nurse") %>%
        seize("cardiologist") %>%
        timeout(function() rpois(1, 20)) %>%
        release("cardiologist")
      
      env<-simmer() %>%
        add_generator("patient", t0, function() .opt("interarrival_time")) %>%
        add_resource("nurse", .opt("nr_nurses")) %>%
        add_resource("cardiologist", .opt("nr_cardiologists"))
      
      env
    }

The objective in this case is the *number of patients served within 4
hours*. The helper function `msr_arrivals_finished` will return this
value for us.

We want to define two constraints, one constraint on the maximum waiting
time and a second one on the total incurred employee costs. For the
first one, the helper function `assert_waiting_time_max` will help to
assert that the maximum waiting time is satisfied. For the second one we
need to create a custom constraint evaluation function. This custom
constraint evaluation function is created below.

    assert_within_budget<-function(envs, budget){
      runtime_hrs <- msr_runtime(envs) / 60
      number_nurses <- msr_resource_capacity(envs, "nurse")
      number_cardiologists <- msr_resource_capacity(envs, "cardiologist")
      
      (number_nurses * runtime_hrs * 50 +
        number_cardiologists * runtime_hrs * 100 ) <= budget
    }

Below, we use the above specifications to find the best combination of
parameters (`nr_nurses`, `nr_cardiologists` and `interarrival_time`) by
leveraging the `grid_optim` optimization method and setting the `until`
runtime to 4 hours.

    set.seed(2016)
    r <- simmer_optim(
      model = sim_model,
      method = grid_optim,
      direction = "max",
      objective = msr_arrivals_finished,
      constraints = list(with_args(assert_within_budget, budget = 1000),
                         with_args(assert_avg_waiting_time_max, max_val = 30)),
      params = list(nr_nurses = par_discrete(1:4),
                    nr_cardiologists = par_discrete(1:4),
                    interarrival_time = par_discrete(c(10, 15, 20, 25))
      ),
      control = optim_control(run_args = list(until=4*60)))

    r

    ## simmer.optim result 
    ## 
    ## method:                 grid_optim 
    ## objective value:        14 
    ## constraints satisfied:  TRUE 
    ## 
    ## params: 
    ## >  nr_nurses :  1 
    ## >  nr_cardiologists :  2 
    ## >  interarrival_time :  15

We see from the output that, when we respect the constraints, the
maximum number of patients that can be served before the 4 hours are up
is 14. The output also shows the paramaters which generate this output
and which can be accessed through `$params`.

    r$params

    ## $nr_nurses
    ## [1] 1
    ## 
    ## $nr_cardiologists
    ## [1] 2
    ## 
    ## $interarrival_time
    ## [1] 15

Replicated simulations
----------------------

Often you do want to optimize the parameters to for a specific run of
the simulation, but instead want to test a paramater combination on *n*
number of replications. We can easily achieve this by specifying the
`replications` argument in the control object. In the below example we
run each parameter configuration 20 times.

    set.seed(2016)
    r <- simmer_optim(
      model = sim_model,
      method = grid_optim,
      direction = "max",
      objective = msr_arrivals_finished,
      constraints = list(with_args(assert_within_budget, budget = 1000),
                         with_args(assert_avg_waiting_time_max, max_val = 30)),
      params = list(nr_nurses = par_discrete(1:4),
                    nr_cardiologists = par_discrete(1:4),
                    interarrival_time = par_discrete(c(10, 15, 20, 25))
      ),
      control = optim_control(run_args = list(until=4*60),
                              replications = 20))

    r

    ## simmer.optim result 
    ## 
    ## method:                 grid_optim 
    ## objective value:        9.85 
    ## constraints satisfied:  TRUE 
    ## 
    ## params: 
    ## >  nr_nurses :  1 
    ## >  nr_cardiologists :  2 
    ## >  interarrival_time :  20

As we can see, the results are different from our earlier findings.
While the single run finds quite a good objective valuen, we see that
when we run 20 replications of the model, the earlier found solutions
was probably a very 'optimisitic' run, which will not be achieved on most
days.

Replications can become quite time consuming when run sequentially. We
you use `optim_control(parallel = TRUE)`, the replications will be run
in parallel by leveraging `parallel::mclapply`.

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

(see previous example)

### Simulated annealing

Simulated annealing is a [heuristic
method](https://en.wikipedia.org/wiki/Simulated_annealing) that *tries*
to find a good solution. Finding the optimum combination of parameters
is however not guaranteed. `simmer.optim` leverages the `GenSA` package
to perform simmulated annealing. Note that it is important to
explicitely pass `par_discrete` type values if the value has to be
interpreted as an integer, this is due to the fact that a cardinality
constraint is applied to the parameter in the optimization procedure.
`GenSA` works with lower and upper bounds on variables, so passing e.g
`c(10, 20, 30)` will translate in a lower bound of 10 and upper bound of
30, while allowing for all values in between (converted to integer if
supplied through `par_discrete()`).

    simmer_optim(
      model = sim_model,
      method = sa_optim,
      direction = "max",
      objective = msr_arrivals_finished,
      constraints = list(with_args(assert_within_budget, budget = 1000),
                         with_args(assert_avg_waiting_time_max, max_val = 30)),
      params = list(nr_nurses = par_discrete(1:4),
                    nr_cardiologists = par_discrete(1:4),
                    interarrival_time = par_discrete(c(10:25))
      ),
      control = optim_control(run_args = list(until=4*60),
                              sa_optim = list(maxit = 15)))

### Differential evolution

Differential evolition is a [heuristic
method](https://en.wikipedia.org/wiki/Differential_evolution) that
*tries* to find a good solution. Finding the optimum combination of
parameters is however not guaranteed. `simmer.optim` leverages the
`RcppDE` package to perform differential evolution. Note that it is
important to explicitely pass `par_discrete` type values if the value
has to be interpreted as an integer, this is due to the fact that a
cardinality constraint is applied to the parameter in the optimization
procedure. `GenSA` works with lower and upper bounds on variables, so
passing e.g `c(10, 20, 30)` will translate in a lower bound of 10 and
upper bound of 30, while allowing for all values in between (converted
to integer if supplied through `par_discrete()`).

    simmer_optim(
      model = sim_model,
      method = de_optim,
      direction = "max",
      objective = msr_arrivals_finished,
      constraints = list(with_args(assert_within_budget, budget = 1000),
                         with_args(assert_avg_waiting_time_max, max_val = 30)),
      params = list(nr_nurses = par_discrete(1:4),
                    nr_cardiologists = par_discrete(1:4),
                    interarrival_time = par_discrete(c(10:25))
      ),
      control = optim_control(run_args = list(until=4*60),
                              de_optim = RcppDE::DEoptim.control(itermax = 10,
                                                                 trace = T)))
