source("sim_probs.R")
context("simulated annealing optimization")

test_that("no errors are returned",{
  expect_error({
    r<-sa_optim(sim_prob_1,
                objective = "max",
                nurse = as.integer(c(1,4)),
                cardiologist = as.integer(c(1,4)),
                control = list(max.call = 5))
  }, NA)
})


test_that("converges correctly",{
  r<-sa_optim(sim_prob_1,
              objective = "max",
              nurse = as.integer(c(1,4)),
              cardiologist = as.integer(c(1,4)),
              control = list(max.call = 200))

  expect_true(results(r)$objective >= 40 && results(r)$objective <= 50)
})


test_that("works correctly with numeric values",{
  fun<-function(){
    optim_results(objective = .opt("x"))
  }
  r<-sa_optim(fun,
              objective = "min",
              x = c(.6, 2)
              )
  expect_equal(results(r)$params$x, .6)

})

test_that("works correctly with integer values",{
  fun<-function(){
    optim_results(objective = .opt("x"))
  }
  r<-sa_optim(fun,
              objective = "min",
              x = as.integer(c(1, 2))
  )
  expect_equal(results(r)$params$x, 1)

})

