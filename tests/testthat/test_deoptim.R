source("sim_probs.R")
context("differential evolution optimization")

test_that("no errors are returned",{
  expect_error({
    r<-de_optim(sim_prob_1,
                objective = "max",
                nurse = c(1,4),
                cardiologist = c(1,4),
                deoptim_control = DEoptim::DEoptim.control(
                  itermax = 2
                ))
  }, NA)
})


test_that("converges correctly",{
  r<-de_optim(sim_prob_1,
              objective = "max",
              nurse = c(1,4),
              cardiologist = c(1,4),
              deoptim_control = DEoptim::DEoptim.control(itermax = 10)
  )
  expect_true(results(r)$objective >= 40 && results(r)$objective <= 50)
})
