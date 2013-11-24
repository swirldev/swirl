runTest <- function(...)UseMethod("runTest")



#' Takes as arguments the current state, expression (as passed to callback function), 
#' value of the expression, and the correct or exprected value. Returns TRUE if the
#' value of the expression matches the correct value and FALSE otherwise.
runTest.testVal <- function(e, correctVal) {
  identical(as.character(e$val), correctVal)
}

#' Takes as arguments the current state, expression (as passed to callback function), 
#' and value of the expression. Returns TRUE if the expression is an assignment 
#' 
runTest.testAssign <- function(e, val) {
  identical(class(e$expr), "<-")
}

#' Takes as arguments the current state, expression (as passed to callback function), 
#' value of the expression, and a function name (as a string). Returns TRUE if the
#' function name provided is matched in the expression.
#' 
runTest.testFunc <- function(e, func) {
  (is.call(e$expr) || is.expression(e$expr)) &&
  func %in% flatten(e$expr)
}

#' Tests whether one new variable has been created.
runTest.testNewVar <- function(e){
  eval(e$expr)
  newVars <- setdiff(ls(),c("e"))
  if (length(newVars)==1){
    eval(e$expr,e)
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

#' Tests the result of a computation such as mean(newVar) applied
#' to a variable created in a previous question. 
runTest.testResultEquals <- function(e, correct.expr){
  # Get the variables created all previous questions.
  vars <- setdiff(ls(e), e$ignore)
  # The test fails if there were none.
  if(length(vars) == 0)return(FALSE)
  # Evaluate the correct expression on all of them
  possibly.correct <- lapply(vars, function(newVar)tryEval(correct.expr, newVar))
  # Test succeeds if the correct value matches that which the user
  # computed in this question.
  return(val %in% possibly.correct)
}


### HELPER FUNCTIONS

#' This function tries to evaluate an expression containing newVal.
#' If it succeeds it will return the value of the expression. If it
#' fails it will return an NA.
tryEval <- function(expr, newVar){
  return(try(eval(expr), silent=TRUE))
}

flatten <- function(expr){
  if(is.leaff(expr)){
    return(expr)
  } else {
    return(unlist(lapply(expr, flatten)))
  }
}

is.leaff <- function(x)!(is.call(x) || is.expression(x))

