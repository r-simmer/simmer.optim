source("sim_probs.R")
context("differential evolution optimization")

test_that("no errors are returned",{
  expect_error({
    r<-de_optim(sim_prob_1,
                objective = "max",
                nurse = as.integer(c(1,4)),
                cardiologist = as.integer(c(1,4)),
                deoptim_control = RcppDE::DEoptim.control(itermax = 2,
                                                          trace = F)
                )
  }, NA)
})


test_that("converges correctly",{
  r<-de_optim(sim_prob_1,
              objective = "max",
              nurse = as.integer(c(1,4)),
              cardiologist = as.integer(c(1,4)),
              deoptim_control = RcppDE::DEoptim.control(itermax = 10,
                                                        trace = F)
  )
  expect_true(results(r)$objective >= 40 && results(r)$objective <= 50)
})


test_that("works correctly with numeric values",{
  fun<-function(){
    optim_results(objective = .opt("x"))
  }
  r<-de_optim(fun,
              objective = "min",
              x = c(.6, 2),
              deoptim_control = RcppDE::DEoptim.control(trace = F)
  )
  expect_equal(results(r)$params$x, .6)

})

test_that("works correctly with integer values",{
  fun<-function(){
    optim_results(objective = .opt("x"))
  }
  r<-de_optim(fun,
              objective = "min",
              x = as.integer(c(1, 2)),
              deoptim_control = RcppDE::DEoptim.control(trace = F)
  )
  expect_equal(results(r)$params$x, 1)

})

test_that("works correctly with maximization and minimization", {
  fun<-function(){
    optim_results(objective = .opt("x"))
  }
  r_max<-de_optim(fun,
              objective="max",
              x=c(1,2),
              deoptim_control = RcppDE::DEoptim.control(trace = F))
  r_min<-de_optim(fun,
                  objective="min",
                  x=c(1,2),
                  deoptim_control = RcppDE::DEoptim.control(trace = F))


  expect_equal(results(r_max)$objective, 2)
  expect_equal(results(r_min)$objective, 1)
})
