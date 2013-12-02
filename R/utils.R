library(stringr)

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

choice <- function(x){
  # Unless x has AnswerType multiple, return NULL
  # Use strsplit with split=";" to separate the choices
  # Strsplit returns a list but we want only its first element,
  # a vector of choices.
  # Use str_trim (pkg stringr) to remove white space from the choices.
  # Use select.list to get the user's choice.
  # Return the user's choice.
}

needPrompt <- function(x){
  return(x[,"AnswerType"]=="command")
}

#Utilities

swirl_out <- function(...) {
  wrapped <- strwrap(str_c(..., sep = " "),
                     width = getOption("width") - 2)
  message(str_c("| ", wrapped, collapse = "\n"))
}