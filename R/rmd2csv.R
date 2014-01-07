# This script takes content authored in an Rmd file and puts it in a csv file.
# This is a covenience for instructors who prefer to author content in Rmd format.
# It will be refined over time as we revisit course authoring best practices.
# NOTE: The csv will have the same name as the Rmd file and will be placed in the
# same directory. Must uncomment final write.csv() to use!

clean_rmd <- function(rmd) {
  
  # Remove leading and trailing whitespace
  rmd_clean <- str_trim(rmd)
  
  # Remove empty lines
  rmd_clean[which(rmd_clean != "")]
}

into_units <- function(rmd) {
  
  # Separate rmd into groups based on units of instruction
  unit_num <- cumsum(rmd == "---")
  
  # Return list of units
  split(rmd, unit_num)
}

collapse_choices <- function(choices) {
  no_num <- sub("^[1-9][.]\\s", "", choices)
  paste(no_num, collapse = "; ")
}

parse_unit <- function(unit) {
 
  # Remove dashes if they are there
  dash_ind <- grep("^---$", unit)
  if(length(dash_ind) > 0) unit <- unit[-dash_ind]
  
  # Find code chunk delimeters
  beg_chunk <- grep("```{r", unit, fixed=TRUE)
  end_chunk <- grep("^```$", unit)
  
  if(length(beg_chunk) > 0 && length(end_chunk) > 0) {
    # Run this block only if code chunk exists
    # Define output to be everything before code chunk begins
    cl <- "cmd_question"
    output <- unit[seq(1, beg_chunk - 1)]
    
    # Take correct answer to be everything in between (exclusive)
    correct_ans <- unit[seq(beg_chunk + 1, end_chunk - 1)]
    
  } else {
    # Run this if no code chunk exists
    cl <- "text"
    output <- unit
    correct_ans <- NA
  }
  
  # Find answer choices
  choice_ind <- grep("^[1-9][.]", unit)
  # Set choices to NA
  choices <- NA
  
  # If answer choices exist
  if(length(choice_ind) > 0) {
    cl <- "mult_question"
    output <- unit[-choice_ind]
    choices <- collapse_choices(unit[choice_ind])
  }
  
  # Return output and correct answer
  list(cl = cl, output = output, correct_ans = correct_ans, choices = choices)
}

rmd2csv <- function(rmd_path) {
  # Read Rmd file with output and correct answers only
  my_rmd <- readLines(rmd_path, n = 500)
  
  # Remove title
  my_rmd <- my_rmd[-c(1, 2)]
  
  # Remove whitespace
  my_rmd_clean <- clean_rmd(my_rmd)
  
  # Break rmd into units
  units <- into_units(my_rmd_clean)
  
  # Vectorize parsing over units
  content <- lapply(units, parse_unit)
  
  # Create df columns
  cl <- sapply(content, `[[`, 1)
  output <- sapply(content, `[[`, 2)
  correct_ans <- sapply(content, `[[`, 3)
  choices <- sapply(content, `[[`, 4)
  
  # Organize in data frame
  df <- data.frame(Class = cl, Output = output, CorrectAnswer = correct_ans, 
                   AnswerChoices = choices, AnswerTests = NA, Hint = NA, 
                   Figure = NA, VideoLink = NA, Tag = NA)
  
  # Create path to csv
  csv_path <- sub(".Rmd$", ".csv", rmd_path)

  # Write data frame to csv
  write.csv(df, file = csv_path, row.names = FALSE)
}

#fp <- "./inst/Courses/Intro to R/module1/mod1_new.Rmd"
#rmd2csv(fp)
