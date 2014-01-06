# This script takes content authored in an Rmd file and puts it in a csv file.
# This is a covenience for instructors who prefer to author content in Rmd format.
# It will be refined over time as we revisit course authoring best practices.
# NOTE: The csv will have the same name as the Rmd file and will be placed in the
# same directory.

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
    output <- unit[seq(1, beg_chunk - 1)]
    
    # Take correct answer to be everything in between (exclusive)
    correct_ans <- unit[seq(beg_chunk + 1, end_chunk - 1)]
  } else {
    # Run this if no code chunk exists
    output <- unit
    correct_ans <- NA
  }
  
  # Remove answer choices for multiple choice questions until I figure out what
  # to do with them
  choice_ind <- grep("^[1-9][.]", output)
  if(length(choice_ind) > 0) output <- output[-choice_ind]
  
  # Return output and correct answer
  list(output = output, correct_ans = correct_ans)
}

# Load stringr package
library(stringr)

# Define file path, excluding extension (Rmd/csv)
fp <- "./inst/Courses/Intro to R/module3/mod3_new"

# Read Rmd file with output and correct answers only
my_rmd <- readLines(paste0(fp, ".Rmd"), n = 500)

# Remove title
my_rmd <- my_rmd[-c(1, 2)]

# Remove whitespace
my_rmd_clean <- clean_rmd(my_rmd)

# Break rmd into units
units <- into_units(my_rmd_clean)

# Vectorize parsing over units
content <- lapply(units, parse_unit)

# Create df columns
output <- sapply(content, `[[`, 1)
correct_ans <- sapply(content, `[[`, 2)

# Organize in data frame
df <- data.frame(Output = output, CorrectAnswer = correct_ans)

# Write data frame to csv
write.csv(df, file = paste0(fp, ".csv"), row.names = FALSE)