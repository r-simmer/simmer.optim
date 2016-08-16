library(simmer.optim)

context("core functionality")

test_that("variable infusion works through the core func",{
  expect_equal(run_instance(function(){
    .opt("x")  + .opt("y")
  }, x=3, y=5), 8)
})

test_that("variable infusion works through the R6 SimmerOptim obj",{
  c1<-
    simmer.optim:::SimmerOptim$new(
      function(){
        .opt("test")
      }, "max")
  expect_equal(c1$run_instance(test=2), 2)

})
