# Instruction set for swirl.R's "virtual machine". 

# All classes first Output, all in the same way, hence one method
# suffices.
# 
present <- function(current.row, e)UseMethod("present")

present.default <- function(current.row, e){
  swirl_out(current.row[, "Output"], skip_after=TRUE)
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
#   e$row <- 1 + e$row
#   e$iptr <- 1
  e$iptr <- 1 + e$iptr
}

waitUser.text_many_question <- function(current.row, e){
  e$val <- str_trim(unlist(strsplit(readline("ANSWER: "),",")))
#   e$row <- 1 + e$row
#   e$iptr <- 1
  e$iptr <- 1 + e$iptr
}

waitUser.text_order_question <- function(current.row, e){
  e$val <- str_trim(unlist(strsplit(readline("ANSWER: "),",")))
#   e$row <- 1 + e$row
#   e$iptr <- 1
  e$iptr <- 1 + e$iptr
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

waitUser.figure <- function(current.row, e){
  file.path <- paste(e$path,current.row[,"Figure"],sep="/")
  source(file=file.path,local=TRUE)
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
  e$val <- select.list(sample(choices), graphics=FALSE)
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

# Only the question classes enter a testing loop. Testing is the
# same in both cases. If the response is correct they indicate
# instruction should progress. If incorrect, they publish a hint
# and return to the previous step. 
testResponse <- function(current.row, e)UseMethod("testResponse")

testResponse.default <- function(current.row, e){
  tests <- current.row[,"AnswerTests"]
  if(is.na(tests) || tests == ""){
    results <- is(e, "dev")
    if(!results){
      stop("BUG: There are no tests for this question!")
    }
  } else {
    tests <- str_trim(unlist(strsplit(tests,";")))
    results <- lapply(tests, function(keyphrase){testMe(keyphrase,e)})
  }
  correct <- !(FALSE %in% unlist(results))
  if(correct){
    swirl_out(praise())
    e$iptr <- 1
    e$row <- 1 + e$row
  } else {
    mes <- tryAgain()
    if(is(current.row, "cmd_question")) {
      mes <- paste(mes, "Or, type info() for more options.")
    }
    swirl_out(mes)
    temp <- current.row[,"Hint"]
    if (!is.na(temp)) swirl_out(current.row[,"Hint"], skip_after=TRUE)
    e$iptr <- e$iptr -1
  }
}

testMe <- function(keyphrase, e){
  # Add a new class attribute to the keyphrase using
  # the substring left of its first "=".
  attr(keyphrase, "class") <- c(class(keyphrase),
                                strsplit(keyphrase, "=")[[1]][1])
  return(runTest(keyphrase, e))
}
