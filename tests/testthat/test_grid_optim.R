source("sim_probs.R")
library(dplyr)
context("grid optimization")



test_that("no errors are returned", {
  expect_error({
    r <- simmer_optim(model = sim_prob_1,
                      method = grid_optim,
                      direction = "max",
                      objective = msr_arrivals_finished,
                      constraints = list(function(envs){
                        cost_nurse <- 40
                        cost_cardiologist <- 100

                        total_cost <-
                          msr_runtime(envs)/60 * msr_resource_amount(envs, "nurse") * cost_nurse +
                          msr_runtime(envs)/60 * msr_resource_amount(envs, "cardiologist") * cost_cardiologist

                        total_cost < 2000
                      }),
                      control = optim_control(run_args = list(until = 8 * 60)),
                      params = opt_params(
                        nurse = par_discrete(1:4),
                        cardiologist = par_discrete(1)))
  }, NA)
})


test_that("no errors are with multiple envs", {
  expect_error({
    r <- simmer_optim(model = sim_prob_1,
                      method = grid_optim,
                      direction = "max",
                      objective = msr_arrivals_finished,
                      constraints = list(function(envs){
                        cost_nurse <- 40
                        cost_cardiologist <- 100

                        total_cost <-
                          msr_runtime(envs)/60 * msr_resource_amount(envs, "nurse") * cost_nurse +
                          msr_runtime(envs)/60 * msr_resource_amount(envs, "cardiologist") * cost_cardiologist

                        total_cost < 2000
                      }),
                      control = optim_control(run_args = list(until = 8 * 60),
                                              rep = 100),
                      params = list(
                        nurse = par_discrete(1:4),
                        cardiologist = par_discrete(1)))
  }, NA)
})


test_that("converges correctly", {
  r <- simmer_optim(model=sim_prob_1,
                    method = grid_optim,
    direction = "max",
    params = list(
      nurse = par_discrete(1:4),
      cardiologist = par_discrete(1:4)
    )
  )
  expect_true(results(r)$objective >= 40 &&
                results(r)$objective <= 50)
})

test_that("constraints never met is handled correctly", {
  expect_error({
    grid_optim(
      sim_prob_1,
      objective = "max",
      nurse = 99,
      cardiologist = 99
    )
  })
})
