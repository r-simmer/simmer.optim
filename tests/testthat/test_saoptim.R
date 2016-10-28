source("sim_probs.R")
context("sa_optim optimization")



test_that("no errors are returned", {
  expect_error({
    r <- simmer_optim(model = sim_prob_1,
                      method = sa_optim,
                      direction = "max",
                      objective = msr_arrivals_finished,
                      constraints = list(sim_prob_1_constraint),
                      control = optim_control(run_args = list(until = 8 * 60),
                                              sa_optim = list(maxit = 1)),
                      params = list(
                        nurse = par_discrete(1:4),
                        cardiologist = par_discrete(1:2)))
  }, NA)
})


test_that("no errors are with multiple envs", {
  expect_error({
    r <- simmer_optim(model = sim_prob_1,
                      method = sa_optim,
                      direction = "max",
                      objective = msr_arrivals_finished,
                      constraints = list(sim_prob_1_constraint),
                      control = optim_control(run_args = list(until = 8 * 60),
                                              rep = 2,
                                              sa_optim = list(maxit = 1)),
                      params = list(
                        nurse = par_discrete(1:4),
                        cardiologist = par_discrete(1:4)))
  }, NA)
})


test_that("converges correctly", {
  r <- simmer_optim(model= sim_prob_1,
                    method = sa_optim,
                    direction = "max",
                    objective = msr_arrivals_finished,
                    constraints = list(sim_prob_1_constraint),
                    control = optim_control(run_args = list(until = 8 * 60),
                                            rep = 2,
                                            sa_optim = list(max.call = 200)),
                    params = list(
                      nurse = par_discrete(1:4),
                      cardiologist = par_discrete(1:4)
                    ))

  expect_true(r$objective_value >= 38 &&
                r$objective_value <= 50)
})


test_that("converges correctly with parallel runs", {
  r <- simmer_optim(model= sim_prob_1,
                    method = sa_optim,
                    direction = "max",
                    objective = msr_arrivals_finished,
                    constraints = list(sim_prob_1_constraint),
                    control = optim_control(run_args = list(until = 8 * 60),
                                            rep = 2,
                                            sa_optim = list(max.call = 200),
                                            parallel = TRUE),
                    params = list(
                      nurse = par_discrete(1:4),
                      cardiologist = par_discrete(1:4)
                    ))

  expect_true(r$objective_value >= 30 &&
                r$objective_value <= 50)
})

test_that("constraints never met returns a 'constraints satisfied==FALSE'", {
  r<-simmer_optim(model=sim_prob_1,
                  method = sa_optim,
                  direction = "max",
                  objective = msr_arrivals_finished,
                  constraints = list(sim_prob_1_constraint),
                  control = optim_control(run_args = list(until = 8 * 60),
                                          rep = 1,
                                          sa_optim = list(max.call = 1)),
                  params = list(
                    nurse = par_discrete(99:100),
                    cardiologist = par_discrete(99:100)
                  ))
  expect_false(r$constraints_satisfied)
})
