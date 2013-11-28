library(stringr)

#' Instruction set and utilities for testMod.R's "virtual machine".
#' 
#' In testMod4Daphne there are 3 Output Types, 'text', 'video', and 
#' 'question', and for questions there are two Answer Types, 'command' 
#' and 'multiple.' We'll use four S3 classes to deal with them,
#' 'text', 'video', 'cmd_question', and 'mult_question'.
#' Assume instructions operate in the persistent environment, e,
#' hence have direct access to variables such as mod and row.
#' 
#' A 'text' row prints Output, waits for <enter> from
#' readline(...), then indicates it is finished.
#' 
#' A 'video' row prints Output (asking about a video,)
#' waits for a y or n from readline("y or n? "), then performs
#' one of two sequences. If 'y' it prints 'type nxt() to resume',
#' indicates it is finished, and causes a return to the prompt.
#' If 'n' it just indicates it is finished.
#' 
#' A 'mult_question' prints Output, parses mod's AnswerChoice
#' column and enters a potentially infinite loop. Within the loop
#' it waits for a response from select.list and tests it. If correct
#' it exits the loop and indicates it is finished. If incorrect 
#' it prints the Hint and remains in the loop.
#' 
#' A 'cmd_question' prints Output and enters a potentially
#' infinite loop. First it exits to the R prompt. Upon return it tests
#' what the user has entered. If correct it exits the loop and indicates
#' it is finished. If incorrect it prints the Hint and remains
#' in the loop.

#' All classes first Output, all in the same way, hence one method
#' suffices.
#' 
present <- function(current.state, e)UseMethod("present")

present.default <- function(current.row, e){
  swirl_out(current.row[,"Output"])
  e$iptr <- 1 + e$iptr
}

#' All classes then wait for user response, in 4 different ways, hence
#' 4 different methods are required. Text and video are both finished
#' at this point.

waitUser <- function(current.row, e)UseMethod("waitUser")

waitUser.text <- function(current.row, e){
  readline("...")
  e$row <- 1 + e$row
  e$iptr <- 1
}

waitUser.video <- function(current.row, e){
  response <- readline("y or n? ")
  if(response %in% c("y", "Y", "yes", "Yes")){
    swirl_out("Type nxt() to continue")
    e$prompt <- TRUE
    browseURL(current.row[,"VideoLink"])
  }
  e$row <- 1 + e$row
  e$iptr <- 1
}

waitUser.mult_question <- function(current.row, e){
  # Use strsplit with split=";" to separate the choices
  # Strsplit returns a list but we want only its first element,
  # a vector of choices.
  # Use str_trim (pkg stringr) to remove white space from the choices.
  # Use select.list to get the user's choice.
  # Return the user's choice.
  choices <- strsplit(current.row[,"AnswerChoices"],";")
  choices <- str_trim(choices[[1]])
  e$val <- select.list(choices)
  e$iptr <- 1 + e$iptr
}

waitUser.cmd_question <- function(current.row, e){
  e$prompt <- TRUE
  e$iptr <- 1 + e$iptr
}

#' Questions of both types enter a loop at point 2. Point 3 is testing. 
#' Point 4 is conditional on testing. Failure prints the Hint (point 5)
#' and returns to point 2. Success exits.
#' 
#' There are up to 5 points. Point 1 is common, Output. Point 2 requires
#' 4 methods, plus a FINISHED flag, and a PROMPT flag. Point 3, testing
#' requires at most 2 methods; possibly just one. Point 4 requires just
#' 1: upon success set FINISHED, upon failure print the Hint and set
#' the instruction pointer to 2 (user response.)
#' 
#' 
