context("rmatch_calls")

test_that("Omitted leading or trailing zeros don't cause mismatch.", {
  testv <- parse(text="seq(1, 10, by=0.5);  seq(1, 10, by=.5); seq(1, 10, by=.50)")
  iscorrect <- is_identical_to(rmatch_calls(testv[[1]]))
  for(v in testv){
    expect_that(rmatch_calls(v), iscorrect)
  }
  invisible()
})

test_that("Omission, inclusion, or order of named arguments doesn't cause mismatch.", {
  testv <- parse(text="seq(1, 10, by=0.5);  seq(to=10, from=1, by=0.5); seq(1, 10, 0.50, length.out=NULL)")
  iscorrect <- is_identical_to(rmatch_calls(testv[[1]]))
  for(v in testv){
    expect_that(rmatch_calls(v), iscorrect)
  }
  invisible()
})

test_that("S4 methods and reference classes raise errors",{
  # For testing reference classes; example from Hadley Wickham, Advanced R
  Person <- setRefClass("Person", methods = list(
    say_hello = function() message("Hi!")
  ))
  person <- Person$new()
  # For testing S4 functions. (logLik(object) in stats4 is distributed with R.)
  library(stats4)
  testv <- parse(text="peep <- Person$new(); person$say_hello(); logLik(obj)")
  for(v in testv){
    expect_that(try(rmatch_calls(v), silent=TRUE), is_a("try-error"))
  }
})

test_that("With default settings, S3 methods with calls as first arguments raise errors.",{
  expr <- quote(print(paste("hi", 5)))
  expect_that(try(rmatch_calls(expr), silent=TRUE), is_a("try-error"))
  expr <- quote(summary(lm(child ~ parent, galton)))
  expect_that(try(rmatch_calls(expr), silent=TRUE), is_a("try-error"))
})

test_that("With eval_for_class=TRUE, S3 methods with calls as first arguments raise errors.",{
  expr <- quote(print(paste("hi", 5)))
  expect_that(is(try(rmatch_calls(expr, eval_for_class=TRUE), silent=TRUE),"try-error"), is_false())
})
