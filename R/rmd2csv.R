####################################
###### Maybe much easier if I write a function to parse one unit of instruction
###### then iterate over the entire doc.
####################################

# Load stringr package
library(stringr)

# Read Rmd file with Output and AnswerTests only
my_rmd <- readLines("./inst/Courses/Intro to R/module1/mod1_test.Rmd", n = 500)

# Remove leading and trailing whitespace
my_rmd <- str_trim(my_rmd)

# Find indexes of ---'s, which separate units of instruction
dash_ind <- grep("^---$", my_rmd)

# Find beginnings and ends of R code chunks
beg_chunk <- grep("```{r", my_rmd, fixed=TRUE)
end_chunk <- grep("^```$", my_rmd)

# Confirm same length
length(beg_chunk) == length(end_chunk)

# Get line indexes of all R code chunks
chunk_ind <- c(mapply(`:`, beg_chunk, end_chunk))

# Distinguish actual code from code chunk delimeters
code_ind <- setdiff(chunk_ind, c(beg_chunk, end_chunk))

# Get indexes of blank lines
blank_ind <- which(my_rmd == "")

# Get indexes of regular text
text_ind <- setdiff(seq(length(my_rmd)), c(dash_ind, chunk_ind, blank_ind))

# Store output
output <- my_rmd[text_ind]

# Store correct answers
correct_ans <- my_rmd[code_ind]
