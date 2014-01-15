# Experimental tests for learning testthat

# install.packages("testthat") # (if necessary)
# install.packages("devtools") # (if necessary)
# require(testthat)
# require(devtools)
# > load_all()
# > test_dir("inst/tests/")

context("Learning testthat")

test_that("runTest.newVar and runTest.result can handle random vectors.", {
  # Code in curly brackets, the second argument to test_that
  e <- new.env()
  # Simulate that the user has entered a new variable
  # consisting of 5 uniform and 5 normal random numbers.
  x <- c(runif(5), rnorm(5))
  e$expr <- quote(x <- c(runif(5), rnorm(5)))
  e$val <- x
  # runTest.newVar should return TRUE, indicating a new variable was
  # created. To do so, it evaluates e$expr internally and checks if
  # a new variable was created in its own runtime environment. Because
  # the user has created a random vector, the internally created variable
  # will differ in value from the user's. Luckily, runTest.newVar does not
  # retain the internally generated value, but retains the user's value,
  # e$val, instead.
  expect_that(testMe(keyphrase="newVar", e=e), is_true())
  # In a subsequent question the user is asked to find the
  # mean of the variable created.
  e$expr <- quote(mean(x))
  e$val <- mean(x)
  # mean(newVar) will equal mean(x), since newVar was given the value
  # of x in the previous test.
  expect_that(testMe(keyphrase="result=mean(newVar)", e=e), is_true())
  invisible()
})
