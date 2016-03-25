# Instruction set for swirl.R's "virtual machine".

# All classes first Output, all in the same way, hence one method
# suffices.
#
present <- function(current.row, e)UseMethod("present")

present.default <- function(current.row, e){
  # Present output to user
  post_exercise(e, current.row)
  # Initialize attempts counter, if necessary
  if(!exists("attempts", e)) e$attempts <- 1
  # Increment pointer
  e$iptr <- 1 + e$iptr
}

# All classes then wait for user response, in different ways, hence
# different methods are required. Text and video are both finished
# at this point.

waitUser <- function(current.row, e)UseMethod("waitUser")

waitUser.default <- function(current.row, e){
  readline("...")
  e$row <- 1 + e$row
  e$iptr <- 1
}

waitUser.text_question <- function(current.row, e){
  e$val <- str_trim(unlist(strsplit(readline("ANSWER: "),",")))
  e$iptr <- 1 + e$iptr
}

waitUser.text_many_question <- function(current.row, e){
  e$val <- str_trim(unlist(strsplit(readline("ANSWER: "),",")))
  e$iptr <- 1 + e$iptr
}

waitUser.text_order_question <- function(current.row, e){
  e$val <- str_trim(unlist(strsplit(readline("ANSWER: "),",")))
  e$iptr <- 1 + e$iptr
}


waitUser.video <- function(current.row, e){
  response <- readline("Yes or No? ")
  if(tolower(response) %in% c("y", "yes")){
    swirl_out(s()%N%"Type nxt() to continue")
    e$prompt <- TRUE
    e$playing <- TRUE
    browseURL(current.row[,"VideoLink"])
  }
  e$row <- 1 + e$row
  e$iptr <- 1
}

waitUser.figure <- function(current.row, e){
  fp <- file.path(e$path, current.row[,"Figure"])
  local({
    source(fp,local=TRUE)
    xfer(environment(), globalenv())
    temp <- as.list(environment())
    e$snapshot <- c(e$snapshot, temp)
  })
  readline("...")
  e$row <- 1 + e$row
  e$iptr <- 1
}


waitUser.mult_question <- function(current.row, e){
  # Use strsplit with split=";" to separate the choices
  # Use select.list to get the user's choice.
  choices <- strsplit(current.row[,"AnswerChoices"],";")
  # Strsplit returns a list but we want only its first element,
  # a vector of choices. Use str_trim (pkg stringr) to remove
  # leading and trailing white space from the choices.
  choices <- str_trim(choices[[1]])
  # Store the choice in e$val for testing
  e$val <- post_mult_question(e, choices)
  
  e$iptr <- 1 + e$iptr
}


waitUser.exact_question <- function(current.row, e){
  # Indicate a return to the prompt is necessary.
  e$prompt <- TRUE
  e$iptr <- 1 + e$iptr
}

waitUser.range_question <- function(current.row, e){
  # Indicate a return to the prompt is necessary.
  e$prompt <- TRUE
  e$iptr <- 1 + e$iptr
}

waitUser.cmd_question <- function(current.row, e){
  # Indicate a return to the prompt is necessary.
  e$prompt <- TRUE
  e$iptr <- 1 + e$iptr
}

#' @importFrom tools file_path_sans_ext
waitUser.script <- function(current.row, e){
  # If this is the first attempt or the user wants to start over, 
  # then create temp files so nothing gets overwritten
  if(e$attempts == 1 || isTRUE(e$reset)) {
    # Get original script name
    orig_script_name <- current.row[,"Script"]
    # Get file path of original script
    orig_script_path <- file.path(e$path, "scripts", orig_script_name)
    # Path temp copy of original script
    e$script_temp_path <- file.path(tempdir(), orig_script_name)
    
    # Original correct script name
    correct_script_name <- paste0(
      tools::file_path_sans_ext(orig_script_name), "-correct.R")
    # Original correct script path
    correct_script_path <- file.path(e$path, "scripts", correct_script_name)
    # Path of temp correct script
    e$correct_script_temp_path <- file.path(tempdir(), correct_script_name)
    
    # Copy original script to temp file
    file.copy(orig_script_path, e$script_temp_path, overwrite = TRUE)
    # Copy original correct to temp correct
    file.copy(correct_script_path, e$correct_script_temp_path, overwrite = TRUE)
    
    # Set reset flag back to FALSE
    e$reset <- FALSE
  }
  # Have user edit the copy. This will reopen the file if 
  # accidentally closed
  file.edit(e$script_temp_path)
  # Give instructions
  # swirl_out("INSTRUCTIONS: Edit the script and experiment in the console as much as you want. When you are ready to move on, SAVE YOUR SCRIPT and type submit() at the prompt. The script will remain open until you close it.",
  #          skip_before = FALSE, skip_after = TRUE)
  # Indicate a return to the prompt is necessary
  e$prompt <- TRUE
  # Enter 'play' mode so that user can mess around in the console
  e$playing <- TRUE
  # Advance lesson
  e$iptr <- 1 + e$iptr
}

# Only the question classes enter a testing loop. Testing is the
# same in both cases. If the response is correct they indicate
# instruction should progress. If incorrect, they publish a hint
# and return to the previous step.
testResponse <- function(current.row, e)UseMethod("testResponse")

testResponse.default <- function(current.row, e){
  if(isTRUE(getOption("swirl_logging"))){
    e$log$question_number <- c(e$log$question_number, e$row)
    e$log$attempt <- c(e$log$attempt, e$attempts)
    e$log$skipped <- c(e$log$skipped, e$skipped)
    e$log$datetime <- c(e$log$datetime, as.numeric(Sys.time()))
  } 
  
  # Increment attempts counter
  e$attempts <- 1 + e$attempts
  # Get answer tests
  tests <- current.row[,"AnswerTests"]
  if(is.na(tests) || tests == ""){
    results <- is(e, "dev")
    if(!results){
      stop(s()%N%"BUG: There are no tests for this question!")
    }
  } else {
    tests <- str_trim(unlist(strsplit(tests,";")))
    results <- lapply(tests, function(keyphrase){testMe(keyphrase,e)})
  }
  correct <- !(FALSE %in% unlist(results))
  if(correct){
    if(isTRUE(getOption("swirl_logging"))){
      e$log$correct <- c(e$log$correct, TRUE)
    }  
    
    mes <- praise()
    post_result(e, passed = correct, feedback = mes, hint = NULL)
    e$iptr <- 1
    e$row <- 1 + e$row
    # Reset attempts counter, since correct
    e$attempts <- 1
  } else {
    if(isTRUE(getOption("swirl_logging"))){
      e$log$correct <- c(e$log$correct, FALSE)
    }
    
    # Restore the previous global environment from the official
    # in case the user has garbled it, e.g., has typed x <- 3*x
    # instead of x <- 2*x by mistake. The hint might say to type
    # x <- 2*x, which would result in 6 times the original value
    # of x unless the original value is restored.
    if(length(e$snapshot)>0)xfer(as.environment(e$snapshot), globalenv())
    mes <- tryAgain()
    if(is(current.row, "cmd_question") && !is(e, "datacamp")) {
      mes <- paste(mes, s()%N%"Or, type info() for more options.")
    }
    hint <- current.row[,"Hint"]
    post_result(e, passed = correct, feedback = mes, hint = if(is.na(hint)) NULL else hint)
    e$iptr <- e$iptr - 1
  }
  # reset skipped info
  e$skipped <- FALSE
}

testMe <- function(keyphrase, e){
  # patch to accommodate old-style tests
  oldcourse <- attr(e$les, "course_name") %in%
    c("Data Analysis", "Mathematical Biostatistics Boot Camp",
      "Open Intro")

  if(oldcourse){
    # Use old test syntax
    # Add a new class attribute to the keyphrase using
    # the substring left of its first "=".
    attr(keyphrase, "class") <- c(class(keyphrase),
                                  strsplit(keyphrase, "=")[[1]][1])
    return(runTest(keyphrase, e))
  } else {
    # Use new test syntax
    return(eval(parse(text=keyphrase)))
  }
}

# CUSTOM TEST SUPPORT. An environment for custom tests is inserted
# "between" function testMe and the swirl namespace. That is,
# an environment, customTests, is created with parent swirl
# and child testMe. Code evaluated within testMe will thus search
# for functions first in customTests, and then in the swirl namespace.
#
# Custom tests must be defined in a file named "customTests.R" in the
# lesson directory. Tests in such files are loaded into environment
# customTests when a lesson is first loaded or progress is restored.
# The environment is cleared between lessons.

# An environment with parent swirl to hold custom tests.
customTests <- new.env(parent=environment(testMe))
# Make customTests the parent of testMe.
environment(testMe) <- customTests

# Function to load custom tests from a source file.
loadCustomTests <- function(lespath){
  customTests$AUTO_DETECT_NEWVAR <- TRUE
  cfile <- file.path(lespath,"customTests.R")
  if(file.exists(cfile)){
    source(cfile, local=customTests)
  }
  return(TRUE) # legacy
}

# Function to remove everything from environment customTests
clearCustomTests <- function(){
  remove(list=ls(customTests), envir=customTests)
}
