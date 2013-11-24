
##This function is simply going to call swirl_out on the Output column of a row of mod.

present <- function(x){
  swirl_out(x[,"Output"])
}

rdln <- function(x,s){
  if(x[,"OutputType"]=="video"){
    return( readline(s))
  }else{
    return(NA)
  }
}

needPrompt <- function(x,s){
  return(x[,"AnswerType"]=="command")
}

#Utilities

library(stringr)

swirl_out <- function(...) {
  wrapped <- strwrap(str_c(..., sep = " "),
                     width = getOption("width") - 2)
  message(str_c("| ", wrapped, collapse = "\n"))
}