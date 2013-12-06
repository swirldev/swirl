#' One approach to extensible testing
#' 
#' If tests are to be identified by keyphrases, then keyphrases must somehow be
#' converted (i.e., parsed) to function calls. It is reasonable to anticipate
#' that new tests will arise with broad deployment and new course material. 
#' Thus it would be convenient if new tests and keyphrases could be added 
#' without the need to change core swirl source code. 
#' 
#' Tests themselves would be new functions or methods, hence are additional code
#' by nature. The problem is to extensibly parse keyphrases to function calls.
#' One possibility, illustrated below, is to give new tests themselves
#' primary responsibility for parsing their own keyphrases.
#' 
#' This would leave the problem of having core swirl code invoke a new test
#' based on the presence of a new keyphrase in a course Module. In our test
#' module, the unique keyphrases are:
#'  "assign"  "newVar"  "word=20" "useFunc=c" "useFunc=mean" 
#'  "result=mean(newVar)" "word=mean()"
#' The tests themselves are identified by the substrings before the "=".
#' Substrings after "=" are essentially arguments. To illustrate a possiblity
#' we'll have core code base its function call on the string prior to "=",
#' and leave the rest to tests themselves. It is doubtful this scheme would
#' be flexible enough in general.
#' 
#' There are various ways to do it, but we'll use S3 methods because we're
#' using them for other things as well. We'll give the keyphrase a class
#' attribute corresponding to the substring prior to "=", and use the keyphrase
#' as first argument to the method.

runTest <- function(...)UseMethod("runTest")

#' Returns TRUE if e$expr is an assignment 
#' 
runTest.assign <- function(keyphrase, e) {
  identical(class(e$expr), "<-")
}

#' Returns TRUE if the function to the right of = in the keyphrase has
#' been used in e$expr
#'  
runTest.useFunc <- function(keyphrase, e) {
  func <- strsplit(keyphrase,"=")[[1]][2]
  (is.call(e$expr) || is.expression(e$expr)) &&
    func %in% flatten(e$expr)
}

#' Returns TRUE if as.character(e$val) matches the string to the right
#' of "=" in keyphase
#' This is for single word answers
runTest.word <- function(keyphrase, e) {
  correctVal <- strsplit(keyphrase,"=")[[1]][2]
  identical(as.character(e$val), correctVal)
}
#' Returns TRUE if as.character(e$val) matches the string to the right
#' of "=" in keyphase
#' This is for multi-word answers for which order matters
runTest.word_order <- function(keyphrase, e) {
  correctVal <- strsplit(keyphrase,"=")[[1]][2]
  identical(as.character(e$val), correctVal)
}
#' Returns TRUE if as.character(e$val) matches the string to the right
#' of "=" in keyphase
#' This is for multi-word answers for which order doesn't matter
runTest.word_many <- function(keyphrase,e){
  correct_ans <- strsplit(keyphrase,"=")[[1]][2]
  correct_list <- str_trim(unlist(strsplit(correct_ans,",")))
  identical(sort(correct_list), sort(e$val))
}

#' Tests if the user has just created one new variable. If so, assigns 
#' e$newVar its value and returns TRUE.
runTest.newVar <- function(keyphrase, e){
  eval(e$expr)
  newVars <- setdiff(ls(),c("keyphrase", "e"))
  if (length(newVars)==1){
    eval(e$expr,e)
    e$newVar <- e$val
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

#' Tests if the user has just created one new variable of correct name. If so, 
#' returns TRUE.
runTest.correctName <- function(keyphrase, e){
  correctName <- strsplit(keyphrase,"=")[[1]][2]
  eval(e$expr)
  newVars <- setdiff(ls(),c("keyphrase", "e"))
  if ((length(newVars)==1) && (identical(newVars,correctName))) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
 }

#' Tests the result of a computation such as mean(newVar) applied
#' to a specific variable created in a previous question. 
runTest.result <- function(keyphrase, e){
  correct.expr <- parse(text=strsplit(keyphrase,"=")[[1]][2])
  newVar <- e$newVar
  return(identical(e$val, eval(correct.expr)))
}

runTest.exact <- function(keyphrase,e){
  is.correct <- FALSE
  if(is.numeric(e$val)){
    correct.ans <- eval(parse(text=strsplit(keyphrase,"=")[[1]][2]))
    epsilon <- 0.01*abs(correct.ans)
    is.correct <- abs(e$val-correct.ans) <= epsilon
  }
  return(is.correct)
}

runTest.range <- function(keyphrase,e){
  is.correct <- FALSE
  correct.ans <- eval(parse(text=strsplit(keyphrase,"=")[[1]][2]))
  if (is.numeric(e$val)){
     temp <- as.numeric(unlist(str_split(correct.ans,";")))
     # use is.logical in case the user types a non-digit which converts to NA's
     is.correct <- is.logical(ans >= temp[1] && ans <= temp[2])
  }
  return(is.correct)
}

runTest.swirl1cmd <- function(keyphrase,e){
  correct.expr <- eval(parse(text=strsplit(keyphrase,"=")[[1]][2]))
 
}




### HELPER FUNCTIONS

flatten <- function(expr){
  if(is.leaff(expr)){
    return(expr)
  } else {
    return(unlist(lapply(expr, flatten)))
  }
}

is.leaff <- function(x)!(is.call(x) || is.expression(x))

