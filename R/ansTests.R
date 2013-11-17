#' Takes as arguments the current state, expression (as passed to callback function), 
#' value of the expression, and the correct or exprected value. Returns TRUE if the
#' value of the expression matches the correct value and FALSE otherwise.
#' 
testVal <- function(state, expr, val, correctVal) {
  identical(val, correctVal)
}

#' Takes as arguments the current state, expression (as passed to callback function), 
#' and value of the expression. Returns TRUE if the expression is an assignment 
#' 
testAssign <- function(state, expr, val) {
  identical(class(expr), "<-")
}

#' Takes as arguments the current state, expression (as passed to callback function), 
#' value of the expression, and a function name (as a string). Returns TRUE if the
#' function name provided is matched in the expression.
#' 
testFunc <- function(state, expr, val, func) {
  (is.call(expr) || is.expression(expr)) &&
  func %in% flatten(expr)
}

#' Tests whether one new variable has been created.
testNewVar <- function(state, expr, val, ...){
  current.vars <- setdiff(ls(module), module$ignore)
  past <- module$mirror[[1]]
  new.vars <- setdiff(current.vars, names(past))
  if(length(new.vars) == 1)return(TRUE)
  changes <- sapply(names(past), function(x)past[[x]]!=get(x,module))
}

#' Tests the result of a computation such as mean(newVar) applied
#' to a variable created in a previous question. 
testResultEquals <- function(state, expr, val, correct.expr){
  # Get the variables created all previous questions.
  vars <- module$mirror[[1]]
  # The test fails if there were none.
  if(length(vars) == 0)return(FALSE)
  # Evaluate the correct expression on all of them
  possibly.correct <- lapply(vars, function(newVar)tryEval(correct.expr, newVar))
  print(possibly.correct)
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


### EXAMPLES

# e <- quote(y <- mean(c(1,2,3)))
# flatten(e)
# testVal(NULL, e, eval(e), mean(c(1,2,3)))  # TRUE
# testVal(NULL, e, eval(e), 2)  # TRUE
# testVal(NULL, e, eval(e), 100)  # FALSE
# testAssign(NULL, e, eval(e))  # TRUE
# testFunc(NULL, e, eval(e), "mean")  # TRUE
# testFunc(NULL, e, eval(e), "apply")  # FALSE
# 
# g <- quote(median(sample(1:20, 5)))
# flatten(g)
# testFunc(NULL, g, eval(e), "sample")  # TRUE
# testFunc(NULL, g, eval(e), "sum")  # FALSE
# testFunc(NULL, g, eval(e), "sample")  # TRUE
# testFunc(NULL, g, eval(e), "sum")  # FALSE
# 
# h <- quote(assign("x", 10))
# flatten(h)
# testVal(NULL, h, eval(h), 10) # TRUE
# testAssign(NULL, h, eval(e))  # FALSE - Using assign() makes the expr class "call", not "<-"
# testFunc(NULL, h, eval(h), "assign")  # TRUE
# testFunc(NULL, h, eval(h), "<-")  # FALSE