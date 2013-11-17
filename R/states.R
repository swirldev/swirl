library(stringr)
source("R/ansTests.R")

### STATE CLASSES AND METHODS

# Define "signatures" for methods to interface between control
# and content. These tell R that when it sees, for instance, 
# nextState(state) it should actually invoke a function of the
# form nextState.blah(state), where blah is a class of state.
#
doStage <- function(state, expr, val)UseMethod("doStage")
nextState <- function(state)UseMethod("nextState")

# Define the nextState method for the base class, tmod. Since
# nextState should do the same thing for every class we will
# only need this one. The higher classes will "inherit" it
# like this: A class actually consists of a class hierarchy.
# In this case it will be either just the base class, "tmod", or
# a vector of two classes such as c("tmod_video", "tmod").
# If, for instance, R cannot find a function of the form
# nextState.tmod_video(state), it will look for one of
# the form nextState.tmod(state). This method creates a new state of the
# appropriate class from the next row of content.
#
nextState.tmod <- function(state){
  # All we need is the appropriate row of the course module.
  n <- 1 + state$row
  # If we've run out of content, return NULL to signal
  # control that we're done.
  if(n > module$rows)return(NULL)
#   # To support command tests, we want a cumulative record of all variables
#   # which the user has created. TODO: We should also keep track of those
#   # which the user has removed.
#   vars <- state$vars
  # Begin building the class hierarchy for this state
  # starting with a base state called tmod.
  cls <- c("tmod")
  # Get the appropriate row of content from the global variable, "module."
  content <- module$mod[n,]
  # If this row of content is a video, add tmod_video to the
  # class hierarchy.
  if(content$OutputType == "video")cls <- c("tmod_video", cls)
  # 
  # If the row is a question, then add the command's AnswerType
  # to the class hierarchy
  if(content$OutputType == "question"){
    if(content$AnswerType == "command") cls <- c("tmod_cmd", cls)
    if(content$AnswerType == "multiple") cls <- c("tmod_mult", cls)
  }
  # Return the constructed state, which is just a list of useful things
  # with a class attribute. (Function structure() just adds the attribute
  # to the list.)
  return(
    structure(list(content=content, row=n, stage=1), class = cls)
  )
}

# The doStage methods will be different for different classes.
# The base method just prints out the state's Output and gives
# the user a chance to continue or quit.
doStage.tmod <- function(state, expr, val){
  prettyOut(state$content[1,"Output"])
  # Allow the user to suspend instruction
  suspend <- suspendQ()
  # Tell control to suspend or continue
  return(list(finished=TRUE, prompt=suspend, suspend=suspend, state=state))
}

# For video, the doStage method prints the state's output, which asks
# whether the user would like to watch a video. If yes, a system
# call opens the appropriate link in a browser.
doStage.tmod_video <- function(state, expr, val){
  prettyOut(state$content[1,"Output"])
  resp <- readline("ANSWER: ")
  if(resp %in% c("y", "Y", "yes", "Yes")){
    # The user wants to watch the video.
    # Indicate the lesson will be suspended meanwhile.
    prettyOut("Type nxt() to continue.")
    # Direct the default browser to the video.
    browseURL(state$content[,"VideoLink"])
    # Tell control that this state is finished and lessons are suspended
    return(list(finished=TRUE, prompt=TRUE, suspend=TRUE, state=state))
  } else {
    # The user wants to skip the video, so tell control to continue.
    return(list(finished=TRUE, prompt=FALSE, suspend=FALSE, state=state))
  }
}

# For multiple choice, the doStage method must present the choices
# and apply tests to the answers. We're finished if all tests
# return TRUE. Otherwise we type the hint and retry.
doStage.tmod_mult <- function(state, expr, val){
  # If we're at stage 2 or more (retry) give the hint.
  # We could limit the number of tries here as well.
  state$stage <- state$stage + 1
  # Ask the question
  prettyOut(state$content[,"Output"])
  # Form the choices
  choices <- str_trim(unlist(strsplit(state$content[,"AnswerChoices"], ";")))
  # Get the user's answer
  ans <- select.list(choices, graphics=FALSE)
  # Extract the keyphrases from content's AnswerTests row
  keyphrases <- as.list(str_trim(unlist(str_split(state$content$AnswerTests, ";"))))
  # Apply the associated tests to the response.
  results <- lapply(keyphrases, 
                    function(x)testByPhrase(x,state,expr,ans))
  # For now, the user fails unless all tests are passed
  passed <- !(FALSE %in% results) 
  if(passed){
    prettyOut("Correct!")
  } else {
    prettyOut(paste("Nope.", state$content$Hint))
  }
  return(list(finished=passed, prompt=FALSE, suspend=FALSE, state=state))
}

# For questions requiring that the user type a command, we must ask
# the question, return the user to the R prompt, and evaluate his or her
# response at a subsequent stage of this same state.
doStage.tmod_cmd <- function(state, expr, val){
  if(state$stage == 1){
    # We are entering the state for the first time. Ask the question,
    # increment the stage and tell control to return the user 
    # to the prompt.
    prettyOut(state$content[,"Output"])
    state$stage <- 1 + state$stage
    return(list(finished=FALSE, prompt=TRUE, suspend=FALSE, state=state))
  } else {
#     # The user has responded to the question. Control has captured the 
#     # response and passed it along as parameters expr and val. The
#     # response may have resulted in creation of one or more new variables,
#     # however, so we must capture those.
#     new.vars <- findAssignedNames(expr)
#     # Incorporate them in the state
#     state$vars <- union(new.vars, state$vars)
    # Extract the keyphrases from content's AnswerTests row
    keyphrases <- as.list(str_trim(unlist(str_split(state$content$AnswerTests, ";"))))
    # Apply the associated tests to the response.
    results <- lapply(keyphrases, 
                      function(x)testByPhrase(x,state,expr,val))
    # For now, the user fails unless all tests are passed
    passed <- !(FALSE %in% results) 
    if(passed){
      prettyOut("Correct!")
      return(list(finished=TRUE, prompt=FALSE, suspend=FALSE, state=state))
    } else {
      # The user has failed. Present the hint and tell control to
      # return the user to the prompt for another try
      state$state <- 1 + state$stage
      prettyOut(state$content[,"Hint"])
      return(list(finished=FALSE, prompt=TRUE, suspend=FALSE, state=state))
    }
  }
}

### UTILITIES

prettyOut <- function(...) {
  wrapped <- strwrap(str_c(..., sep = " "),
                     width = getOption("width") - 2)
  message(str_c("| ", wrapped, collapse = "\n"))
}

# Allows the user to suspend instruction by entering "play" (w/o quotes)
# or to continue by entering anything else, including nothing.
suspendQ <- function(){
  suspend <- readline("...") == "play"
  if(suspend)prettyOut("Type nxt() to continue.")
  return(suspend)
}

# Finds names of variables assigned in the given expression,
# returning them in a character vector
findAssignedNames <- function(expr){
  # Convert the expression, a tree, to a flat list 
  f.expr <- flatten(expr)
  # Find all names preceded by assignment operators
  # Indices of names in f.expr
  name.idx <- which(sapply(f.expr, function(x)class(x)=="name"))
  # Indices of assigment operators
  assign.idx <- which(sapply(f.expr, function(x)x=='<-'))
  # Indices of names preceded by assignment operators
  idx <- intersect(name.idx, 1+assign.idx)
  return(as.character(f.expr[idx]))
}

# Applies a test specified by a keyphrase
testByPhrase <- function(keyphrase, state, expr, val){
  # Does the given expression contain `<-` ?
  if(keyphrase=="assign")return(testAssign(state, expr, val))
  # Have new variables been created?
  if(keyphrase=="newVar")return(testNewVar(state, expr, val))
  # Has a specificed function been used?
  if(substr(keyphrase,1,8)=="useFunc="){
    func <-substr(keyphrase, 9, nchar(keyphrase))
    passed <- testFunc(state, expr, val, func)
    return(passed)
  }
  # Has the user calculated the correct value?
  if(substr(keyphrase,1,7)=="result="){
    # The correct expression is that appearing after "result="
    correct.expr <- parse(text=substr(keyphrase, 8, nchar(keyphrase)))
    return(testResultEquals(state, exp, val, correct.expr))
  }
  # Has the user given a correct one-word answer?
  if(substr(keyphrase,1,5)=="word="){
    correct.word <- substr(keyphrase,6,nchar(keyphrase))
    return(testVal(state, expr, val, correct.word))
  }
}
