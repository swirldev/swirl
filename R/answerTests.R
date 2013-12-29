#' Extensible testing
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

#' Always returns FALSE. If the default test in invoked, something is wrong.
runTest.default <- function(...)return(FALSE)

#' Returns TRUE if e$expr is an assignment 
#' 
runTest.assign <- function(keyphrase, e) {
  identical(class(e$expr), "<-")
}

#' Returns TRUE if the function to the right of = in the keyphrase has
#' been used in e$expr
#'  
runTest.useFunc <- function(keyphrase, e) {
  func <- rightside(keyphrase)
  (is.call(e$expr) || is.expression(e$expr)) &&
    func %in% flatten(e$expr)
}

#' Returns TRUE if as.character(e$val) matches the string to the right
#' of "=" in keyphase
#' This is for single word answers
runTest.word <- function(keyphrase, e) {
  correctVal <- str_trim(rightside(keyphrase))
  identical(str_trim(as.character(e$val)), 
            str_trim(as.character(correctVal)))
}
#' Returns TRUE if as.character(e$val) matches the string to the right
#' of "=" in keyphase
#' This is for multi-word answers for which order matters
runTest.word_order <- function(keyphrase, e) {
  correctVal <- str_trim(rightside(keyphrase))
  correct_list <- str_trim(unlist(strsplit(correctVal,",")))
  userAns <- str_trim(unlist(strsplit(as.character(e$val),",")))
  identical(userAns, correct_list)
}
#' Returns TRUE if as.character(e$val) matches the string to the right
#' of "=" in keyphase
#' This is for multi-word answers for which order doesn't matter
runTest.word_many <- function(keyphrase,e){
  correct_ans <- rightside(keyphrase)
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
  correctName <- rightside(keyphrase)
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
  correct.expr <- parse(text=rightside(keyphrase))
  newVar <- e$newVar
  ans <- all.equal(e$val, eval(correct.expr))
  # all.equal may return a diagnostic string
  return(ifelse(is.logical(ans), ans, FALSE))
}

runTest.exact <- function(keyphrase,e){
  is.correct <- FALSE
  if(is.numeric(e$val)){
    correct.ans <- eval(parse(text=rightside(keyphrase)))
    epsilon <- 0.01*abs(correct.ans)
    is.correct <- abs(e$val-correct.ans) <= epsilon
  }
  return(is.correct)
}

runTest.range <- function(keyphrase,e){
  is.correct <- FALSE
  correct.ans <-parse(text=rightside(keyphrase))
  if (is.numeric(e$val)){
     correct.ans <- as.character(correct.ans)
     temp <- str_split(correct.ans,"-")
     temp <- as.numeric(unlist(str_split(correct.ans,"-")))
     # use is.logical in case the user types a non-digit which converts to NA's
     is.correct <- (e$val >= temp[1] && e$val <= temp[2])
  }
  return(is.correct)
}


runTest.swirl1cmd <- function(keyphrase,e){
  correct.expr <- parse(text=rightside(keyphrase))
  correct.ans  <- eval(correct.expr)
  ans.is.correct <- isTRUE(all.equal(correct.ans, e$val))
  call.is.correct <- identical(as.expression(e$expr)[[1]], as.expression(correct.expr)[[1]])
  if(ans.is.correct && call.is.correct){
    return(TRUE)
  } else  
    if (ans.is.correct && !call.is.correct){
      assign("uexpr",e$expr,globalenv())
      assign("cexpr",correct.expr,globalenv())
      swirl_out("That's not the expression I expected but it works.")
      #todo
      #following line is temporary fix to create correct vars for future ques if needed
      eval(correct.expr,globalenv())
      return(TRUE)
    }
  else
    return(FALSE)
}

runTest.trick <- function(keyphrase,e){
 if (exists("trick",e,inherits=FALSE)){
   rm("trick",envir=e,inherits=FALSE)
   return(TRUE)
 }
 else{
   e$trick <- 1
   return(FALSE)
 } 
}



### HELPER FUNCTIONS

rightside <- function(keyphrase){
  n <- str_locate(keyphrase,"=")[1]
  return(substr(keyphrase,n+1,nchar(keyphrase)))
}


flatten <- function(expr){
  if(is.leaff(expr)){
    return(expr)
  } else {
    return(unlist(lapply(expr, flatten)))
  }
}

is.leaff <- function(x)!(is.call(x) || is.expression(x))

