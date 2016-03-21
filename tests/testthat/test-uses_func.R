context("uses_func")

test_that("uses_func works with the current version of testthat", {
  expect_true(swirl:::uses_func("info")(parse(text="info()"))[[1]])
})