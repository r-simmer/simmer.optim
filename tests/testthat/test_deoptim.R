set.seed(42)

test_that("no errors are returned", {
  skip_if_not_installed("RcppDE")
  expect_error({
    r <- simmer_optim(model = sim_prob_1,
                      method = de_optim,
                      direction = "max",
                      objective = msr_arrivals_finished,
                      constraints = list(sim_prob_1_constraint),
                      control = optim_control(run_args = list(until = 8 * 60),
                                              de_optim = RcppDE::DEoptim.control(itermax = 2,
                                                                                 trace = F)),
                      params = list(
                        nurse = par_discrete(1:4),
                        cardiologist = par_discrete(1)))
  }, NA)
})


test_that("no errors are with multiple envs", {
  skip_if_not_installed("RcppDE")
  expect_error({
    r <- simmer_optim(model = sim_prob_1,
                      method = grid_optim,
                      direction = "max",
                      objective = msr_arrivals_finished,
                      constraints = list(sim_prob_1_constraint),
                      control = optim_control(run_args = list(until = 8 * 60),
                                              rep = 2,
                                              de_optim = RcppDE::DEoptim.control(itermax = 2,
                                                                                 trace = F)),
                      params = list(
                        nurse = par_discrete(1:4),
                        cardiologist = par_discrete(1:4)))
  }, NA)
})


test_that("converges correctly", {
  skip_if_not_installed("RcppDE")
  r <- simmer_optim(model= sim_prob_1,
                    method = de_optim,
                    direction = "max",
                    objective = msr_arrivals_finished,
                    constraints = list(sim_prob_1_constraint),
                    control = optim_control(run_args = list(until = 8 * 60),
                                            rep = 2,
                                            de_optim = RcppDE::DEoptim.control(itermax = 10,
                                                                               trace = F)),
                    params = list(
                      nurse = par_discrete(1:4),
                      cardiologist = par_discrete(1:4)
                    ))

  expect_true(r$objective_value >= 38 &&
                r$objective_value <= 50)
})


test_that("converges correctly with parallel runs", {
  skip_if_not_installed("RcppDE")
  r <- simmer_optim(model= sim_prob_1,
                    method = de_optim,
                    direction = "max",
                    objective = msr_arrivals_finished,
                    constraints = list(sim_prob_1_constraint),
                    control = optim_control(run_args = list(until = 8 * 60),
                                            rep = 2,
                                            de_optim = RcppDE::DEoptim.control(itermax = 10,
                                                                               trace = F),
                                            parallel = TRUE),
                    params = list(
                      nurse = par_discrete(1:4),
                      cardiologist = par_discrete(1:4)
                    ))

  expect_true(r$objective_value >= 38 &&
                r$objective_value <= 50)
})

test_that("constraints never met returns a 'constraints satisfied==FALSE'", {
  skip_if_not_installed("RcppDE")
    r<-simmer_optim(model=sim_prob_1,
                 method = de_optim,
                 direction = "max",
                 objective = msr_arrivals_finished,
                 constraints = list(sim_prob_1_constraint),
                 control = optim_control(run_args = list(until = 8 * 60),
                                         rep = 1,
                                         de_optim = RcppDE::DEoptim.control(itermax = 1,
                                                                            trace = F)),
                 params = list(
                   nurse = par_discrete(99),
                   cardiologist = par_discrete(99)
                 ))
    expect_false(r$constraints_satisfied)
})
