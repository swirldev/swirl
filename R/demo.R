library(stringr)

# Implement the module content and state as an environment,
# following Hadley. 
module <- new.env(parent = emptyenv())

# Read swirl 1.0 course content into the module environment
module$mod2 <- read.csv("data/mod2.csv", as.is=TRUE)
# As a convenience, store mod2's number of rows there too.
module$rows <- nrow(module$mod2) 

# Read the cars dataset from the openintro package into the global env.
# (It's stored as a csv in case openintro is not installed.)
cars <- read.csv("data/cars.csv", as.is=TRUE, comment.char="#")

hi <- function(){
  # Initialize a "state", consisting of a row number and a hint flag,
  # stored in module.
  module$row.nr <- 1
  module$hint <- FALSE
  # Remove any leftover callbacks from previous runs.
  removeTaskCallback(which(getTaskCallbackNames() == "mod2"))
  # Register function answerCmd() as that to be called
  # upon completion of any "top level" task, i.e., a command the
  # user enters from the R prompt.
  addTaskCallback(answerCmd, name = "mod2")
  runState()
  invisible()
}

nxt <- function(override=FALSE){
  # Increment the state unless the hint flag is TRUE
  # or the override parameter is TRUE.
  if(!module$hint | override){
    module$row.nr <- 1 + module$row.nr
    module$hint <- FALSE
  }
  runState()
  invisible()
}

bye <- function(){
  # Unregister callback
  removeTaskCallback(which(getTaskCallbackNames() == "mod2"))
  message("That's all, folks!")
  invisible()
}

# Hadley's display function
frndlyOut <- function(...) {
  # Format the argument for pretty display.
  # getOption("width") gives screen width in characters.
  wrapped <- strwrap(str_c(..., sep = " "), 
                     width = getOption("width") - 2)
  # Start each line with "| " (str_c is in package stringr)
  # and display the result.
  message(str_c("| ", wrapped, collapse = "\n"))
}

runState <- function(){
  if(module$row.nr <= module$rows){
    content <- module$mod2[module$row.nr,]
    if(content[,1]=="text"){
      runTextState(content)
    } else if(content[,1]=="figure"){
      runFigureState(content)
    } else if(content[,1]=="video"){
      runVideoState(content)
    } else if(content[,1]=="question"){
      runQuestionState(content)
    } else {
      message("Can't do this one; run nxt()")
    }
  } else {
    bye()
  }
}

runTextState <- function(content){
  frndlyOut(content[,"Output"])
}

runFigureState <- function(content){
  frndlyOut(content[,"Output"])
  file.path <- paste("R",content[,"Figure"],sep="/")
  source(file=file.path, local=TRUE)
  if(content[,"Figure.Type"] == "addition"){
    frndlyOut("I'm displaying the previous plot in case you need to refer back to it.")}
}

runVideoState <- function(content){
  frndlyOut(content[,"Output"])
  resp <- readline("ANSWER: ")
  if(resp %in% c("y", "Y", "yes", "Yes")){
    browseURL(content[,"Video"])
  }
}

runQuestionState <- function(content){
  if(module$hint){
    frndlyOut(content[,"Hint"])
  } else {
    frndlyOut(content[,"Output"])
  }
  if(content[,"Answer.Type"] == "text"){
    answerText(content)
  } else if (content[,"Answer.Type"] == "multiple"){
    answerMultiple(content)
  } else if (content[,"Answer.Type"] == "exact"){
    answerExact(content)
  } else if (content[,"Answer.Type"] == "range"){
    answerRange(content)
  } else {
    message("Can't check this one; run nxt()")
  }
}

answerText <-function(content){
  respond(readline("ANSWER: ") == content[,"Correct.Answer"])
}

answerMultiple <- function(content){
  choices <- str_trim(unlist(strsplit(content[,"Choices"], ";")))
  respond(select.list(choices) == content[,"Correct.Answer"])
}

answerExact <- function(content){
  suppressWarnings(
    ans <- try(as.numeric(readline("ANSWER: ")), silent=TRUE)
  )
  respond(is.numeric(ans) && ans == as.numeric(content[,"Correct.Answer"]))
}

answerRange <- function(content){
  suppressWarnings(
    ans <- try(as.numeric(readline("ANSWER: ")), silent=TRUE)
  )
  # assume the correct answer will convert correctly to numeric
  temp <- as.numeric(unlist(str_split(content[,"Correct.Answer"],";")))
  # use is.logical in case the user types a non-digit which converts to NA's
  respond(is.logical(ans >= temp[1] && ans <= temp[2]))
}

respond <- function(correct){
  if(correct){
    module$hint <- FALSE
    frndlyOut("That is correct. Brilliant!")
  } else {
    module$hint <- TRUE
    frndlyOut("Sorry. That's not quite what I need. Type nxt() for a hint and another try.")
  }
}

answerCmd <- function(...){
  # The callback
  return(TRUE)
}