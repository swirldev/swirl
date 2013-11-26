source("R/utils.R")
source("R/modConstructor.R")

resume.testMod <- function(e){
  # This function is entered ONLY when the user has entered a
  # valid expression at the R prompt.
  #
  # We may be entering for the first time, in which case our environment
  # will not be fully initialized. We check for that and initialize if
  # necessary.
  fromhi <- (!exists("mod",e))
  if(fromhi){
    # Load the course module, using Nick's constructor which 
    # adds attributes identifying the course and indicating dependencies.
    e$mod <- module(read.csv("data/testMod4Daphne.csv"),"4Daphne", "test", "Nick")
    # expr, val, ok, and vis should have been set by the callback.
 
    e$row <- 1
    # add new variables here
    e$ignore <- c(ls(e), "ignore")
  }
  
  # At this point we KNOW we have just returned from the R prompt.
  # The only question is whether the user has just entered "hi" or "nxt"
  # or is answering a command question. We know from above whether
  # "hi" was entered so we should just retain that information to use
  # here. We'll have to check for deparse(e$expr) == "nxt()" or something.
  #
  # If we HAVE entered on hi or nxt, we should go on to the next
  # question.
 
  # we need two routines, one for command questions and one for noncommand
  # questions. Note video questions are treated differently from other noncommand
  # questions - they give the user 
  # a prompt and the user has to type nxt() to continue
  
  
  
  # If we've NOT entered on hi or nxt, the user has answered
  # a command question. We should test the answer. If correct, we
  # should praise and go on to the next row of e$mod. Otherwise, we
  # should give the hint and return(TRUE) to go back to the prompt.
  #
  # So there are two circumstances in which we go on to the next row,
  # 1) hi or nxt, and 2) correct command answer. Perhaps we need a
  # nextQuestion() function or method?
  #
  # In any case, going to the next row of e$mod is a matter of 
  # incrementing e$row, presenting the Output, getting a user
  # response of some type, and possibly testing it.
  # In the case of text OutputType, user response comes from readline("...")
  # The case of multiple AnswerType is similar, but uses select.list
  # In the case of video we do readline("y or n: "), and browseURL and
  # return to the prompt in the case of "y".
  # In the case of commands, we return to the prompt.
  #
  # It's not clear to me at the moment how to organize such steps
  # cogently and extensibly. Possibly nextQuestion() and
  # resumeQuestion() functions?
 
  
  #start testing here

 
  return(TRUE)
}