## Need to put unit class at top of each unit of content as comment so that I know how to parse different blocks differently?? How to deal with multiple choice, where the answer choices are each getting their own place in a vector (ex: output[[24]])

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
  
  
  # Return output and correct answer
  list(output = output, correct_ans = correct_ans)
}

# Load stringr package
library(stringr)

# Read Rmd file with output and correct answers only
my_rmd <- readLines("./inst/Courses/Intro to R/module1/mod1_test.Rmd", n = 500)

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
df <- data.frame(output = output, correct_ans = correct_ans)

# Write data frame to csv
write.csv(df, file = "~/Desktop/mod1_testcsv.csv")