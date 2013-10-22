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
  # Register function checkCmd() as that to be called
  # upon completion of any "top level" task, i.e., a command the
  # user enters from the R prompt.
  addTaskCallback(checkCmd, name = "mod2")
  runState()
}

nxt <- function(override=FALSE){
  # Increment the state unless the hint flag is TRUE
  # or the override parameter is TRUE.
  if(!module$hint | override){
    module$row.nr <- 1 + module$row.nr
    module$hint <- FALSE
  }
  runState()
}

bye <- function(){
  # Unregister callback
  removeTaskCallback(which(getTaskCallbackNames() == "mod2"))
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
    } else {
      message("Can't do this one; run nxt()")
    }
  } else {
    bye()
    message("That's all, folks!")
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

checkCmd <- function(...){
  # The callback
}