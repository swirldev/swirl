library(stringr)

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
  str_split_fixed(unit[1], "&", 2)[2]
}

get_corr_ans <- function(unit) UseMethod("get_corr_ans")

get_corr_ans.default <- function() {
  message("Unit class not specified!")
}

get_corr_ans.cmd_question <- function(unit) {
  # Find code chunk delimeters
  beg_chunk <- grep("```{r", unit, fixed=TRUE)
  end_chunk <- grep("^```$", unit)
  
  if(length(beg_chunk) == 0 | length(end_chunk) == 0) {
    error("You forgot to specify the correct answer on a command question!")
  }
  
  # Return everything in between (exclusive)
  unit[seq(beg_chunk + 1, end_chunk - 1)]
}

get_corr_ans.mult_question <- function(unit) {
  corr_ans_ind <- grep("^_[1-9][.].+_$", unit)
  if(length(corr_ans_ind) == 0) {
    error("You forgot to specify the correct answer on a multiple choice question!")
  }
  gsub("^_[1-9][.]\\s|_$", "", unit[corr_ans_ind])
}

collapse_choices <- function(choices) {
  no_num <- gsub("^_?[1-9][.]\\s|_?$", "", choices)
  paste(no_num, collapse = "; ")
}

get_ans_choices <- function(unit) {
  # Find answer choices
  choice_ind <- grep("^_?[1-9][.]", unit)
  if(length(choice_ind) == 0) {
    error("You forgot to specify answer choices!")
  }
  # Collapse answer choices
  collapse_choices(unit[choice_ind])
}

get_ans_tests <- function(unit) {
  ans_tests_ind <- grep("*** .ans_tests", unit, fixed = TRUE) + 1
  if(length(ans_tests_ind) == 0) {
    error("You forgot to specify answer tests!")
  }
  unit[ans_tests_ind]
}

get_hint <- function(unit) {
  hint_ind <- grep("*** .hint", unit, fixed = TRUE) + 1
  if(length(hint_ind) == 0) error("You forgot to specify a hint!")
  hint <- unit[hint_ind]
}

make_row <- function(unit) UseMethod("make_row")

make_row.default <- function(unit) {
  message("You forgot to specify a content class!")
}

make_row.text <- function(unit) {
  output <- unit[2]
  
  c(Class = class(unit), Output = output, CorrectAnswer = NA,
       AnswerChoices = NA, AnswerTests = NA, Hint = NA,
       Figure = NA, FigureType = NA, VideoLink = NA)
}

make_row.cmd_question <- function(unit) {
  output <- unit[2]
  corr_ans <- get_corr_ans(unit)
  ans_tests <- get_ans_tests(unit)
  hint <- get_hint(unit)
  
  c(Class = class(unit), Output = output, CorrectAnswer = corr_ans,
       AnswerChoices = NA, AnswerTests = ans_tests, Hint = hint,
       Figure = NA, FigureType = NA, VideoLink = NA)
}

make_row.mult_question <- function(unit) {
  output <- unit[2]
  corr_ans <- get_corr_ans(unit)
  ans_choices <- get_ans_choices(unit)
  ans_tests <- paste0("word=", corr_ans)
  hint <- get_hint(unit)
  
  c(Class = class(unit), Output = output, CorrectAnswer = corr_ans,
       AnswerChoices = ans_choices, AnswerTests = ans_tests, Hint = hint,
       Figure = NA, FigureType = NA, VideoLink = NA)
}

rmd2csv <- function(rmd_path) {
  my_rmd <- readLines(rmd_path)
  cleaned <- clean_me(my_rmd)
  units <- into_units(cleaned)
  classes <- lapply(units, get_unit_class)
  units_with_class <- mapply(`class<-`, units, classes)
  rows <- sapply(units_with_class, make_row)
  df <- as.data.frame(t(rows))
  
  # Write content data frame to csv file with same name as rmd
  csv_path <- sub("[.][R|r]md", ".csv", rmd_path)
  write.csv(df, file = csv_path, row.names = FALSE)
}

#rmd_path <- "./inst/Courses/Intro_to_R/module1/mod1_new.Rmd"
rmd2csv(rmd_path)