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
  
  # Capture everything in between (exclusive)
  corr_ans <- unit[seq(beg_chunk + 1, end_chunk - 1)]
  
  # Check for comments
  if(any(grepl("#", corr_ans))) {
    stop("No comments allowed in correct answer!")
  }
  # Return correct answer
  corr_ans
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
    #warning("No answer tests specified for a command question!")
    return(paste0("omnitest(correctExpr=\'", get_corr_ans(unit), "\')"))
  }
  unit[ans_tests_ind]
}

get_ans_tests.mult_question <- function(unit) {
  paste0("omnitest(correctVal=\'", get_corr_ans(unit), "\')")
}

## GET HINT -- GENERIC AND METHODS

get_hint <- function(unit) UseMethod("get_hint")

get_hint.default <- function(unit) {
  NA
}

get_hint.cmd_question <- function(unit) {
  hint_ind <- grep("*** .hint", unit, fixed = TRUE) + 1
  if(length(hint_ind) == 0) stop("You forgot to specify a hint!")
  hint <- unit[hint_ind]
}

get_hint.mult_question <- function(unit) {
  hint_ind <- grep("*** .hint", unit, fixed = TRUE) + 1
  if(length(hint_ind) == 0) stop("You forgot to specify a hint!")
  hint <- unit[hint_ind]
}

## GET FIGURE FILENAME AND TYPE -- GENERIC AND METHODS

get_fig_filename <- function(unit) UseMethod("get_fig_filename")

get_fig_filename.default <- function(unit) {
  NA
}

get_fig_filename.figure <- function(unit) {
  fig_ind <- grep("*** .figure", unit, fixed = TRUE) + 1
  if(length(fig_ind) == 0) stop("You forgot to specify a figure filename!")
  fig <- unit[fig_ind]
}

get_fig_type <- function(unit) UseMethod("get_fig_type")

get_fig_type.default <- function(unit) {
  NA
}

get_fig_type.figure <- function(unit) {
  figtype_ind <- grep("*** .fig_type", unit, fixed = TRUE) + 1
  if(length(figtype_ind) == 0) stop("You forgot to specify a figure type!")
  figtype <- unit[figtype_ind]
}

## GET VIDEO URL -- GENERIC AND METHODS

get_video_url <- function(unit) UseMethod("get_video_url")

get_video_url.default <- function(unit) {
  NA
}

get_video_url.video <- function(unit) {
  vid_ind <- grep("*** .video_url", unit, fixed = TRUE) + 1
  if(length(vid_ind) == 0) stop("You forgot to specify a video URL!")
  vid <- unit[vid_ind]
}

## MAKE ROW

make_row <- function(unit) {
  output <- unit[2]
  corr_ans <- get_corr_ans(unit)
  ans_choices <- get_ans_choices(unit)
  ans_tests <- get_ans_tests(unit)
  hint <- get_hint(unit)
  fig <- get_fig_filename(unit)
  fig_type <- get_fig_type(unit)
  vid_link <- get_video_url(unit)
  
  c(Class = class(unit), Output = output, CorrectAnswer = corr_ans,
       AnswerChoices = ans_choices, AnswerTests = ans_tests, Hint = hint,
       Figure = fig, FigureType = fig_type, VideoLink = vid_link)
}

## UTILITIES

# Return indices of YAML
yaml_ind <- function(rmd) {
  yaml_end <- min(grep("=======", rmd, value=FALSE))
  seq(1:yaml_end)
}

#' @importFrom yaml yaml.load
get_yaml <- function(rmd) {
  # Find index of end of YAML
  yaml_end <- max(yaml_ind(rmd))
  
  # Return lesson metadata
  sapply(seq(1, yaml_end - 1), function(i) yaml.load(rmd[i]))
}

clean_me <- function(rmd) {
  # Remove leading and trailing whitespace
  rmd_clean <- str_trim(rmd)
  
  # Remove empty lines
  rmd_clean <- rmd_clean[which(rmd_clean != "")]
  
  # Get rid of yaml
  rmd_clean[-yaml_ind(rmd_clean)]
}

into_units <- function(rmd) {
  # Separate rmd into groups based on units of instruction
  unit_num <- cumsum(str_detect(rmd, "^---"))
  
  # Return list of units
  split(rmd, unit_num)
}

get_unit_class <- function(unit) {
  cl <- str_split_fixed(unit[1], "&", 2)[2]
  valid_classes <- c("text",
                     "cmd_question",
                     "mult_question",
                     "video",
                     "figure")
  if(!cl %in% valid_classes) stop("Invalid unit class used!")
  cl
}

collapse_choices <- function(choices) {
  no_num <- gsub("^_?[1-9][.]\\s|_?$", "", choices)
  paste(no_num, collapse = "; ")
}

rmd2df <- function(rmd_path) {
  my_rmd <- readLines(rmd_path, warn=FALSE)
  # Get metadata from yaml - set as lesson attributes below
  meta <- get_yaml(my_rmd)
  cleaned <- clean_me(my_rmd)
  units <- into_units(cleaned)
  classes <- lapply(units, get_unit_class)
  units_with_class <- mapply(`class<-`, units, classes)
  rows <- sapply(units_with_class, make_row)
  
  # Assemble content data frame
  df <- as.data.frame(t(rows), stringsAsFactors=FALSE)
  
  # Return object of class "lesson"
  lesson(df, lesson_name=meta$`Lesson Name`, course_name=meta$`Course Name`,
         author=meta$Author, type=meta$Type, organization=meta$Organization,
         version=meta$Version)
}