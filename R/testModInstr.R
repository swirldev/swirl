#' Instruction set for testMod.R's "virtual machine".
#' 
#' In testMod4Daphne there are 3 Output Types, 'text', 'video', and 
#' 'question', and for questions there are two Answer Types, 'command' 
#' and 'multiple.' We'll use four S3 classes to deal with them,
#' 'text', 'video', 'cmd_question', and 'mult_question'.
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

library(stringr)
source("R/S3Tests.R")
source("R/phrases.R")

#' All classes first Output, all in the same way, hence one method
#' suffices.
#' 
present <- function(current.row, e)UseMethod("present")

present.default <- function(current.row, e){
  swirl_out(current.row[, "Output"])
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
  response <- readline("Yes or No? ")
  if(tolower(response) %in% c("y", "yes")){
    swirl_out("Type nxt() to continue")
    e$prompt <- TRUE
    browseURL(current.row[,"VideoLink"])
  }
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
  e$val <- select.list(sample(choices), graphics=FALSE)
  e$iptr <- 1 + e$iptr
}

waitUser.cmd_question <- function(current.row, e){
  # Indicate a return to the prompt is necessary.
  e$prompt <- TRUE
  e$iptr <- 1 + e$iptr
}

#' Only the two question classes enter a testing loop. Testing is the
#' same in both cases. If the response is correct they indicate
#' instruction should progress. If incorrect, they publish a hint
#' and return to the previous step.  
testResponse.default <- function(current.row, e){
  tests <- str_trim(unlist(strsplit(current.row[,"AnswerTests"],";")))
  results <- lapply(tests, function(keyphrase){testMe(keyphrase,e)})
  correct <- !(FALSE %in% results)
  if(correct){
    swirl_out(praise())
    e$iptr <- 1
    e$row <- 1 + e$row
  } else {
    swirl_out(paste(tryAgain(), current.row[,"Hint"]))
    e$iptr <- e$iptr -1
  }
}

testMe <- function(keyphrase, e){
  attr(keyphrase, "class") <- strsplit(keyphrase, "=")[[1]][1]
  return(runTest(keyphrase, e))
}


#' The video class requires a nxt() function.
nxt <- function(){invisible()}
