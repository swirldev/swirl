source("R/utils.R")

resume.testMod <- function(e){
  if(!exists("mod", e)){
    e$mod <- read.csv("data/testMod4Daphne.csv")
    e$expr <- NULL
    e$val <- NULL
    e$ok <- NULL
    e$vis <- NULL
    e$row <- 1
    e$prompt <- FALSE
    # add new variables here
    e$ignore <- c(ls(e), "ignore")
  }
  
  if (!e$prompt){
    present(e$mod[e$row,])
    resp <- rdln(e$mod[e$row,],"y or n? ")
    choice <- selection(e$mod[e$row,])
    e$prompt <- needPrompt(e$mod[e$row,])
  }
  else{
    e$prompt <- FALSE
  }
  
  
  #start testing here

 
  return(TRUE)
}