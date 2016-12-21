library(testthat)
library(simmer.optim)

if (-1 < utils::compareVersion("3.5.1", as.character(utils::packageVersion("simmer"))))
  trajectory <- create_trajectory

test_check("simmer.optim")
