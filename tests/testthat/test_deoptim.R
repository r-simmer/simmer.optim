source("sim_probs.R")
context("grid optimization")



test_that("no errors are returned", {
  expect_error({
    r <- simmer_optim(model = sim_prob_1,
                      method = de_optim,
                      direction = "max",
                      objective = msr_arrivals_finished,
                      constraints = list(sim_prob_1_constraint),
                      control = optim_control(run_args = list(until = 8 * 60),
                                              de_optim = RcppDE::DEoptim.control(itermax = 2)),
                      params = list(
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
                      constraints = list(sim_prob_1_constraint),
                      control = optim_control(run_args = list(until = 8 * 60),
                                              rep = 2,
                                              de_optim = RcppDE::DEoptim.control(itermax = 2)),
                      params = list(
                        nurse = par_discrete(1:4),
                        cardiologist = par_discrete(1:4)))
  }, NA)
})


test_that("converges correctly", {
  r <- simmer_optim(model= sim_prob_1,
                    method = de_optim,
                    direction = "max",
                    objective = msr_arrivals_finished,
                    constraints = list(sim_prob_1_constraint),
                    control = optim_control(run_args = list(until = 8 * 60),
                                            rep = 2,
                                            de_optim = RcppDE::DEoptim.control(itermax = 10),
                                            parallel = F),
                    params = list(
                      nurse = par_discrete(1:4),
                      cardiologist = par_discrete(1:4)
                    ))

  expect_true(r$objective_value >= 38 &&
                r$objective_value <= 50)
})

test_that("constraints never met is handled correctly", {
  expect_error({
    simmer_optim(model=sim_prob_1,
                 method = grid_optim,
                 direction = "max",
                 objective = msr_arrivals_finished,
                 constraints = list(sim_prob_1_constraint),
                 control = optim_control(run_args = list(until = 8 * 60),
                                         rep = 1),
                 params = list(
                   nurse = par_discrete(99),
                   cardiologist = par_discrete(99)
                 ))
  })
})
