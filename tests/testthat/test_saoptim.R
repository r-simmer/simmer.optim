source("sim_probs.R")
context("differential evolution optimization")

test_that("no errors are returned",{
  expect_error({
    r<-sa_optim(sim_prob_1,
                objective = "max",
                nurse = c(1,4),
                cardiologist = c(1,4),
                control = list(max.call = 5))
  }, NA)
})


test_that("converges correctly",{
  r<-sa_optim(sim_prob_1,
              objective = "max",
              nurse = c(1,4),
              cardiologist = c(1,4),
              control = list(max.call = 100))

  expect_true(results(r)$objective >= 40 && results(r)$objective <= 50)
})
