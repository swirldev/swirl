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
  func %in% flatten(expr)
}

testResultEquals <- function(state, expr, val, correct.expr){
  # This test assumes a new variable should have been created.
  # sometime during the lesson. If not, the test fails.
  if(length(state$vars) == 0)return(FALSE)
  # The correct value is that of the correct expression, but to evaluate
  # the correct expression we need the values of those variables in
  # the global environment whose names appear in state$vars.
  var.vals <- lapply(state$vars, function(x)get(x,globalenv()))
  # We'll try to evaluate the correct expression using each of the
  # values of the variables created.
  possibly.correct <- 
    lapply(var.vals, function(x)tryEval(correct.expr, x))
  # Some of these tries may have returned try errors. We'll remove
  # them. First find all the entries which are not try errors.
  idx <- sapply(possibly.correct, function(x)class(x)!="try-error")
  # Excise them.
  possibly.correct <- possibly.correct[idx]
  # See if there are any matches between these and the values calculated
  # by the user.
  matches <- intersect(val, possibly.correct)
  # The test succeeds if there is at least one match
  return(length(matches) > 0)
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