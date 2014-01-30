library(stringr)

## GET CORRECT ANSWER -- GENERIC AND METHODS

get_corr_ans <- function(unit) UseMethod("get_corr_ans")

get_corr_ans.default <- function(unit) {
  NA
}

get_corr_ans.cmd_question <- function(unit) {
  # Find code chunk delimeters
  beg_chunk <- grep("```{r", unit, fixed=TRUE)
  end_chunk <- grep("^```$", unit)
  
  if(length(beg_chunk) == 0 | length(end_chunk) == 0) {
    stop("You forgot to specify the correct answer on a command question!")
  }
  
  # Return everything in between (exclusive)
  unit[seq(beg_chunk + 1, end_chunk - 1)]
}

get_corr_ans.mult_question <- function(unit) {
  corr_ans_ind <- grep("^_[1-9][.].+_$", unit)
  if(length(corr_ans_ind) == 0) {
    stop("You forgot to specify the correct answer on a multiple choice question!")
  }
  gsub("^_[1-9][.]\\s|_$", "", unit[corr_ans_ind])
}

## GET ANSWER CHOICES -- GENERIC AND METHODS

get_ans_choices <- function(unit) UseMethod("get_ans_choices")

get_ans_choices.default <- function(unit) {
  NA
}

get_ans_choices.mult_question <- function(unit) {
  # Find answer choices
  choice_ind <- grep("^_?[1-9][.]", unit)
  if(length(choice_ind) == 0) {
    stop("You forgot to specify answer choices!")
  }
  # Collapse answer choices
  collapse_choices(unit[choice_ind])
}

## GET ANSWER TESTS -- GENERIC AND METHODS

get_ans_tests <- function(unit) UseMethod("get_ans_tests")

get_ans_tests.default <- function(unit) {
  NA
}

get_ans_tests.cmd_question <- function(unit) {
  ans_tests_ind <- grep("*** .ans_tests", unit, fixed = TRUE) + 1
  if(length(ans_tests_ind) == 0) {
    warning("No answer tests specified for a command question!")
    return(paste0("expr_identical=", get_corr_ans(unit)))
  }
  unit[ans_tests_ind]
}

get_ans_tests.mult_question <- function(unit) {
  paste0("word=", get_corr_ans(unit))
}

## GET HINT -- GENERIC AND METHODS

get_hint <- function(unit) UseMethod("get_hint")

# TODO: Don't use this default method once add functionality for videos, etc.
get_hint.default <- function(unit) {
  hint_ind <- grep("*** .hint", unit, fixed = TRUE) + 1
  if(length(hint_ind) == 0) stop("You forgot to specify a hint!")
  hint <- unit[hint_ind]
}

get_hint.text <- function(unit) {
  NA
}

## MAKE ROW

make_row <- function(unit) {
  output <- unit[2]
  corr_ans <- get_corr_ans(unit)
  ans_choices <- get_ans_choices(unit)
  ans_tests <- get_ans_tests(unit)
  hint <- get_hint(unit)
  fig <- NA
  fig_type <- NA
  vid_link <- NA
  
  c(Class = class(unit), Output = output, CorrectAnswer = corr_ans,
       AnswerChoices = ans_choices, AnswerTests = ans_tests, Hint = hint,
       Figure = fig, FigureType = fig_type, VideoLink = vid_link)
}

## UTILITIES

clean_me <- function(rmd) {
  # Remove leading and trailing whitespace
  rmd_clean <- str_trim(rmd)
  
  # Remove empty lines
  rmd_clean <- rmd_clean[which(rmd_clean != "")]
  
  # Remove title
  rmd_clean[-c(1, 2)]
}

into_units <- function(rmd) {
  # Separate rmd into groups based on units of instruction
  unit_num <- cumsum(str_detect(rmd, fixed("---")))
  
  # Return list of units
  split(rmd, unit_num)
}

get_unit_class <- function(unit) {
  cl <- str_split_fixed(unit[1], "&", 2)[2]
  valid_classes <- c("text",
                     "cmd_question",
                     "mult_question")
  if(!cl %in% valid_classes) stop("Invalid unit class used!")
  cl
}

collapse_choices <- function(choices) {
  no_num <- gsub("^_?[1-9][.]\\s|_?$", "", choices)
  paste(no_num, collapse = "; ")
}

rmd2csv <- function(rmd_path, csv_path) {
  my_rmd <- readLines(rmd_path)
  cleaned <- clean_me(my_rmd)
  units <- into_units(cleaned)
  classes <- lapply(units, get_unit_class)
  units_with_class <- mapply(`class<-`, units, classes)
  rows <- sapply(units_with_class, make_row)
  df <- as.data.frame(t(rows))
  
  # Write content data frame to csv file with same name as rmd
  # csv_path <- sub("[.][R|r]md", ".csv", rmd_path)
  write.csv(df, file = csv_path, row.names = FALSE)
}

## Execute?

#rmd_path <- "~/Desktop/my_test_mod.Rmd"
#rmd2csv(rmd_path)