context("Content formatting and completeness")

mod <- read.csv(file.choose(), stringsAsFactors=FALSE)
reqCols <- c("Class",
             "Output",
             "AnswerChoices",
             "AnswerTests",
             "Hint",
             "Figure",
             "FigureType",
             "VideoLink")

test_that("all required columns are present", {
  expect_that(all(reqCols %in% names(mod)), is_true())
})

### test_file("./inst/tests/test-content.R")