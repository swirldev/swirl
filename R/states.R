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
  # To support tests, make a vector of the names of variables which
  # currently exist in the global environment.
  vars <- ls(globalenv())
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
    structure(list(content=content, vars=vars, row=n, stage=1), class = cls)
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
  if(state$stage > 1) prettyOut(state$content[,"Hint"])
  state$stage <- state$stage + 1
  # Ask the question
  prettyOut(state$content[,"Output"])
  # Form the choices
  choices <- str_trim(unlist(strsplit(state$content[,"AnswerChoices"], ";")))
  # Get the user's answer
  ans <- select.list(choices, graphics=FALSE)
  # Apply the muliple choice test. The correct answer should be
  # specified in the AnswerTests column in the form
  # "result=<correct answer>".
  temp <- state$content$AnswerTests
  correct.ans <- substr(temp, 8, nchar(temp))
  passed <- ans == correct.ans
  if(ans == correct.ans){
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
    # The user has responded to the question. Control has captured the 
    # response and passed it along as parameters expr and val. The
    # response may have resulted in creation of one or more new variables,
    # however, so we must capture those. The state contains a vector of
    # variables which existed when the state was entered initially.
    # Capture the names of variables available globally now
    current.vars <- ls(globalenv())
    # Function setdiff finds the names in the first argument which are
    # not in the second, hence captures names of variables created since
    # the state was entered initially.
    new.vars <- setdiff(current.vars, state$vars)
    # Tests are specified by keyphrases in the AnswerTests column
    keyphrases <- as.list(str_trim(unlist(str_split(state$content$AnswerTests, ";"))))
    # Apply the tests to the response.
    results <- lapply(keyphrases, 
                      function(x)testByPhrase(x,state,expr,val,new.vars))
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

# Applies a test specified by a keyphrase
testByPhrase <- function(keyphrase, state, expr, val, new.vars){
  # Does the given expression contain `<-` ?
  if(keyphrase=="assign")return(testAssign(state, expr, val))
  # Have new variables been created?
  if(keyphrase=="newVar")return(length(new.vars) > 0)
  # Has a specificed function been used?
  if(substr(keyphrase,1,8)=="useFunc="){
    func <-substr(keyphrase, 9, nchar(keyphrase))
    return(testFunc(state, expr, val, func))
  }
  # Has the user calculated the correct value?
  if(substr(keyphrase,1,7)=="result="){
    # This test assumes a new variable should have been created.
    # If not, the test fails.
    if(length(new.vars) == 0)return(FALSE)
    # The correct expression is that appearing after "result="
    correct.expr <- parse(text=substr(keyphrase, 8, nchar(keyphrase)))
    # The correct value is that of the correct expression, but to evaluate
    # the correct expression we need the values of those variables in
    # the global environment whose names appear in new.vars. (It is
    # technically possible for the user to have created more than one.)
    new.var.vals <- lapply(new.vars, function(x)get(x,globalenv()))
    # We'll try to evaluate the correct expression using each of the
    # values of the variables created. 
    possibly.corrrect <- 
      lapply(new.var.vals, function(x)tryEval(correct.expr, x))
    # Some of these tries may have returned try errors. We'll remove
    # them. First find all the entries which are not try errors.
    idx <- sapply(possibly.correct, function(x)class(x)!="try-error")
    # Excise them.
    possibly.correct <- possibly.correct[idx]
    # See if there are any matches between these and the values calculated
    # by the user.
    matches <- intersect(val, possibly.correct)
    # The test succeeds if there is at least one match
    return(length(matches) > 0)
  }
}

# This function tries to evaluate an expression containing newVal.
# If it succeeds it will return the value of the expression. If it
# fails it will return an NA.
tryEval <- function(expr, newVal){
  return(try(eval(expr), silent=TRUE))
}