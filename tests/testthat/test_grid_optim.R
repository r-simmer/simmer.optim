source("sim_probs.R")
context("grid optimization")



test_that("no errors are returned",{
  expect_error({
  r<-grid_optim(sim_prob_1,
                objective = "max",
                nurse = 1:4,
                cardiologist = 1)
  }, NA)
})


test_that("no errors are with multiple envs",{
  expect_error({
    r<-grid_optim(sim_prob_2,
                  objective = "max",
                  nurse = 1:4,
                  cardiologist = 1)
  }, NA)
})


test_that("converges correctly outside simmer env",{
  test_fun<-function(){
    optim_results(
      objective = abs(.opt("x")-.opt("y"))
    )
  }

  r<-grid_optim(test_fun,
                objective = "min",
                x=0:4,
                y=0:4)

  expect_equal(r$results()$params$x, r$results()$params$y)
})


test_that("converges correctly",{
  r<-grid_optim(sim_prob_1,
                objective = "max",
                nurse = 1:4,
                cardiologist = 1:4)
  expect_true(results(r)$objective >= 40 && results(r)$objective <= 50)
})

test_that("constraints never met is handled correctly", {
  expect_error({
    grid_optim(sim_prob_1,
               objective = "max",
               nurse = 99,
               cardiologist = 99)
  })
})
