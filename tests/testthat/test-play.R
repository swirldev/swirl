# # Experimental tests for learning testthat
# 
# # install.packages("testthat") # (if necessary)
# # install.packages("devtools") # (if necessary)
# # require(testthat)
# # require(devtools)
# # > load_all()
# # > test_dir("tests")
# 
context("Learning testthat")

test_that("runTest.newVar and runTest.result can handle random vectors.", {
  # Code in curly brackets, the second argument to test_that
  e <- new.env()
  # Simulate that the user has entered a new variable
  # consisting of 5 uniform and 5 normal random numbers.
  x <- c(runif(5), rnorm(5))
  e$expr <- quote(x <- c(runif(5), rnorm(5)))
  e$val <- x
  e$les <- "stub"
  e$delta <- list(x=x)
  attr(e$les, "course_name") <- "Test Lessons"
  e$snapshot <- new.env()
  expect_that(testMe(keyphrase="omnitest('x <- c(runif(5), rnorm(5))')", e=e), is_true())
  invisible()
})
