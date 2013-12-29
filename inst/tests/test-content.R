context("Content formatting and completeness")

#' Maybe put these tests in a loop that runs through all modules stored
#' in the data/ directory - can use info argument to expect_that for more
#' useful/specific messages

mod <- read.csv(file.choose(), stringsAsFactors=FALSE)

###

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

###

# Note: get rid of text questions
classTypes <- c("text",
                "mult_question",
                "cmd_question",
                "exact_question",
                "range_question",
                "video",
                "figure",
                "text_many_question",
                "text_question",
                "text_order_question")

test_that("content classes are correct (assuming only base classes)",{
  expect_that(all(mod$Class %in% classTypes), is_true())
})

###

i <- which(mod$Class == "mult_question")

test_that("answer choices are provided for all multiple choice questions", {
  expect_that(any(mod$AnswerChoices[i] %in% c(NA, "")), is_false())
})

### test_file("./inst/tests/test-content.R")

### Should create new expect_that function that checks whether vector of objects belong to a pretermined set (i.e. factors)