library(testthat)
library(calculator)

test_dir(
  "./testthat",
  env = shiny::loadSupport(),
  reporter = c("progress", "fail")
)

test_check("calculator")
