library(stringr)
library(openintro)

mod2.env <- new.env(parent = emptyenv())

# Read swirl 1.0 course content into the mod2.env environment
assign("mod2",
       read.csv("data/mod2.csv", as.is=TRUE),
       env=mod2.env)


hi <- function(){
  # Initialize a "state", consisting of a row number, in mod2.env
  assign("row.nr", 1, env=mod2.env)
  # We'll need the cars dataset. For now, just hard code it.
  data(cars)
  runState()
}

runState <- function(){
  row.nr <- get("row.nr", mod2.env)
  n.rows <- nrow(get("mod2", mod2.env))
  if(row.nr <= n.rows){
  content <- get("mod2", mod2.env)[row.nr,]
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
    message("That's all, folks!")
  }
}

frndlyOut <- function(...) {
  # Format the argument for pretty display.
  # getOption("width") gives screen width in characters.
  wrapped <- strwrap(str_c(..., sep = " "), 
                     width = getOption("width") - 2)
  # Start each line with "| " (str_c is in package stringr)
  # and display the result.
  message(str_c("| ", wrapped, collapse = "\n"))
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

nxt <- function(){
  row.nr <- get("row.nr", mod2.env)
  assign("row.nr", row.nr+1, envir=mod2.env)
  runState()
}
