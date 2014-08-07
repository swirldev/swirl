# Extensible testing
# 
# If tests are to be identified by keyphrases, then keyphrases must somehow be
# converted (i.e., parsed) to function calls. It is reasonable to anticipate
# that new tests will arise with broad deployment and new course material. 
# Thus it would be convenient if new tests and keyphrases could be added 
# without the need to change core swirl source code. 
# 
# Tests themselves would be new functions or methods, hence are additional code
# by nature. The problem is to extensibly parse keyphrases to function calls.
# One possibility, illustrated below, is to give new tests themselves
# primary responsibility for parsing their own keyphrases.
# 
# The tests themselves are identified by the substrings before the "=".
# Substrings after "=" are essentially arguments. To illustrate a possiblity
# we'll have core code base its function call on the string prior to "=",
# and leave the rest to tests themselves. It is doubtful this scheme would
# be flexible enough in general.
# 
# There are various ways to do it, but we'll use S3 methods because we're
# using them for other things as well. We'll give the keyphrase a class
# attribute corresponding to the substring prior to "=", and use the keyphrase
# as first argument to the method.


runTest <- function(...)UseMethod("runTest")

# Always returns FALSE. If the default test in invoked, something is wrong.
runTest.default <- function(...)return(FALSE)

# Always returns TRUE, for development purposes.
runTest.true <- function(...)return(TRUE)

# Returns TRUE if e$expr is an assignment 
# 
runTest.assign <- function(keyphrase, e) {
  identical(class(e$expr), "<-")
}

# Returns TRUE if the function to the right of = in the keyphrase has
# been used in e$expr
#  
runTest.useFunc <- function(keyphrase, e) {
  func <- rightside(keyphrase)
  (is.call(e$expr) || is.expression(e$expr)) &&
    func %in% flatten(e$expr)
}

# Returns TRUE if as.character(e$val) matches the string to the right
# of "=" in keyphase
# This is for single word answers
runTest.word <- function(keyphrase, e) {
  correctVal <- str_trim(rightside(keyphrase))
  identical(str_trim(as.character(e$val)), 
            str_trim(as.character(correctVal)))
}
# Returns TRUE if as.character(e$val) matches the string to the right
# of "=" in keyphase
# This is for multi-word answers for which order matters
runTest.word_order <- function(keyphrase, e) {
  correctVal <- str_trim(rightside(keyphrase))
  correct_list <- str_trim(unlist(strsplit(correctVal,",")))
  userAns <- str_trim(unlist(strsplit(as.character(e$val),",")))
  identical(userAns, correct_list)
}
# Returns TRUE if as.character(e$val) matches the string to the right
# of "=" in keyphase
# This is for multi-word answers for which order doesn't matter
runTest.word_many <- function(keyphrase,e){
  correct_ans <- rightside(keyphrase)
  correct_list <- str_trim(unlist(strsplit(correct_ans,",")))
  identical(sort(correct_list), sort(e$val))
}

# Tests if the user has just created one new variable. If so, assigns 
# e$newVar its value and returns TRUE.
runTest.newVar <- function(keyphrase, e){
  # TODO: Eventually make auto-detection of new variables an option.
  # Currently it can be set in customTests.R
  delta <- if(!customTests$AUTO_DETECT_NEWVAR){
    safeEval(e$expr, e)
  } else {
    e$delta
  }
  if (length(delta)==1){
    e$newVar <- delta[[1]]
    e$newVarName <- names(delta)[1]
    e$delta <- mergeLists(delta, e$delta)
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

# Tests if the user has just created one new variable of correct name. If so, 
# returns TRUE.
# keyphrase: correctName=<correct name>
runTest.correctName <- function(keyphrase, e){
  # TODO: Eventually make auto-detection of new variables an option.
  # Currently it can be set in customTests.R
  delta <- if(!customTests$AUTO_DETECT_NEWVAR){
    safeEval(e$expr, e)
  } else {
    e$delta
  }
  correctName <- rightside(keyphrase)
  if ((length(delta)==1) && (identical(names(delta)[1],correctName))) {
    e$newVar <- delta[[1]]
    e$newVarName <- names(delta)[1]
    e$delta <- mergeLists(delta, e$delta)
    return(TRUE)
  }
  else {
    return(FALSE)
  }
 }

# Tests the result of a computation such as mean(newVar) applied
# to a specific variable created in a previous question. 
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

runTest.newcmd <- function(keyphrase,e){
  correct.expr <- parse(text=rightside(keyphrase))[[1]]
  correct.ans  <- eval(correct.expr)
  ansResults <- expectThat(e$val,
                               equals(correct.ans,label=correct.ans),
                               label=e$val)
  callResults <- expectThat(as.expression(e$expr)[[1]],
                                is_identical_to(correct.expr,label=deparse(correct.expr)),
                                label=deparse(e$expr))
    
 #   identical(as.expression(e$expr)[[1]], as.expression(correct.expr)[[1]])
  if(ansResults$passed && callResults$passed){
    return(TRUE)
  } else  
    if (ansResults$passed && !callResults$passed){
      swirl_out("That's not the expression I expected but it works.")
      swirl_out(callResults$message)
      #todo
      #following line is temporary fix to create correct vars for future ques if needed
      eval(correct.expr,globalenv())
      return(TRUE)
    }
  else
    return(FALSE)
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
     swirl_out("That's not the expression I expected but it works.")
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

## TESTS AND KEYPHRASES BASED ON PACKAGE TESTTHAT
# These tests will print diagnostics in "dev" mode
# but not in user (default) mode.

# Returns TRUE if e$var or (if it exists) the given 
# global variable is of the given class
# keyphrase: is_a=class or is_a=class,variable
runTest.is_a <- function(keyphrase, e) {
  temp <- strsplit(rightside(keyphrase),",")[[1]]
  class <-  str_trim(temp[1])
  variable <- str_trim(temp[2])
  if(!is.na(variable) && exists(variable, globalenv())){
    val <- get(variable, globalenv())
  } else {
    val <- e$val
  }
  label <- val
  results <- expectThat(val, is_a(class), label=label)
  if(is(e,"dev") && !results$passed)swirl_out(results$message)
  return(results$passed)
}

# Returns TRUE if the function to the right of = in the keyphrase has
# been used in e$expr
# keyphrase: uses_func=functionName
runTest.uses_func <- function(keyphrase, e) {
  func <- rightside(keyphrase)
  results <- expectThat(e$expr,
                        uses_func(func, label=func), 
                        label=deparse(e$expr))
  if(is(e,"dev") && !results$passed)swirl_out(results$message)
  return(results$passed)
}

# Returns TRUE if as.character(e$val) matches the string to the right
# of "=" in keyphase
# keyphrase: matches=regularExpresion
runTest.matches <- function(keyphrase, e) {
  correctVal <- tolower(str_trim(rightside(keyphrase)))
  userVal <- str_trim(as.character(e$val))
  results <- expectThat(tolower(userVal), 
                        matches(correctVal), 
                        label=userVal)
  if(is(e,"dev") && !results$passed)swirl_out(results$message)
  return(results$passed)
}

# Tests if the user has just created one new variable (of correct name
# if given.) If so, returns TRUE.
# keyphrase: creates_var or creates_var=correctName
runTest.creates_var <- function(keyphrase, e){
  # TODO: Eventually make auto-detection of new variables an option.
  # Currently it can be set in customTests.R
  delta <- if(!customTests$AUTO_DETECT_NEWVAR){
    safeEval(e$expr, e)
  } else {
    e$delta
  }
  correctName <- rightside(keyphrase)
  if(is.na(correctName)){
    results <- expectThat(length(delta), equals(1), 
                          label=paste(deparse(e$expr), 
                                      "does not create a variable."))
  } else {
    results <- expectThat(names(delta), 
                          is_equivalent_to(correctName, label=correctName), 
                          label=paste(deparse(e$expr),
                                      "does not create a variable named",
                                      correctName))
  }
  if(results$passed){
    e$newVar <- e$val
    e$newVarName <- names(delta)[1]
    e$delta <- mergeLists(delta, e$delta)
  } else if(is(e,"dev")){
    swirl_out(results$message)
  }
  return(results$passed)
}

# Tests the result of a computation such as mean(newVar) applied
# to a specific variable created in a previous question.
# keyphrase: equals=correctExpression,variable 
runTest.equals <- function(keyphrase, e){
  temp <- strsplit(rightside(keyphrase),",")[[1]]
  correctExprLabel <- temp[1]
  variable <- str_trim(temp[2])
  correctExpr <- gsub(variable, paste0("e$",variable), correctExprLabel)
  correctAns <- safeEval(parse(text=correctExpr))
  if(length(correctAns) != 1)return(FALSE)
  results <- expectThat(e$var, 
                        equals(correctAns[[1]], 
                               label=correctExprLabel), 
                        label=deparse(e$expr))
  if(is(e, "dev") && !results$passed)swirl_out(results$message)
  return(results$passed)
}

# Returns TRUE if as.expression
# (e$expr) matches the expression indicated to the right
# of "=" in keyphrase
# keyphrase:equivalent=expression
runTest.equivalent <- function(keyphrase,e) {
  correctExpr <- as.list(parse(text=rightside(keyphrase)))
  userExpr <- as.list(as.expression(e$expr))
  results <- expectThat(userExpr,
                        is_equivalent_to(correctExpr,deparse(correctExpr)),
                        label=deparse(userExpr))
                        
  if(is(e,"dev") && !results$passed)swirl_out(results$message)
  return(results$passed)
}



# Tests that a value just entered at the R prompt is within
# the given range
# keyphrase: in_range=a,b
runTest.in_range <- function(keyphrase, e){
  range <- try(eval(parse(text=paste0("c(", rightside(keyphrase), ")"))),
               silent=TRUE)
  if(!is.numeric(range)){
    swirl_out(paste("The given range", rightside(keyphrase), "is not numeric."))
    return(FALSE)
  }
  results <- expectThat(e$var, 
                        in_range(range, 
                                 label=range), 
                        label=e$var)
  if(is(e, "dev") && !results$passed)swirl_out(results$message)
  return(results$passed)
}

# Test that the user has entered an expression identical to that
# given in the keyphrase.
# keyphrase: "expr_identical=<correct expression>"
runTest.expr_identical <- function(keyphrase, e){
  correct <- parse(text=rightside(keyphrase))[[1]]
  expr <- e$expr
  if(is.expression(expr))expr <- expr[[1]]
  results <- expectThat(expr, 
                        is_identical_to(correct, label=rightside(keyphrase)),
                        label=deparse(expr))
  if( is(e, "dev") && !results$passed)swirl_out(results$message) 
  return(results$passed)
}

# Test the the length of e$val is that given in the keyphrase
# keyphrase: "val_length=<integer>"
runTest.val_length <- function(keyphrase, e){
  try(n <- as.integer(rightside(keyphrase)), silent=TRUE)
  if(is.na(n)){
    stop(message=paste("BUG: right side of", keyphrase,
                                 "is not an integer."))
  }
  results <- expectThat(length(e$val), equals(n, label=n), 
                        label=paste0("length(c(", toString(e$val), "))"))                                                   
  if( is(e, "dev") && !results$passed)swirl_out(results$message) 
  return(results$passed)
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


### TESTTHAT FUNCTIONS CUSTOMIZED FOR ANSWERTESTS

findExpr <- function(name, env = parent.frame()){
  subs <- do.call("substitute", list(as.name(name), env))
  str_c(deparse(subs, width.cutoff = 500), collapse = "\n")
}

expectThat <- function(object, condition, info=NULL, label=NULL){
  if (is.null(label)) {
    label <- findExpr("object")
  }
  results <- swirlExpectation(condition(object))
  results$message <- str_c(label, " ", results$message)
  if (!is.null(info)) {
    results$message <- str_c(results$message, "\n", info)
  }
  return(results)
}

# Patch for slight incompatibility of testthat versions
swirlExpectation <- function(testthat_expectation){
  passed <- testthat_expectation$passed
  error <- testthat_expectation$error
  if(exists("failure_msg", testthat_expectation)){
    message <- failure_msg <- testthat_expectation$failure_msg
    success_msg <- testthat_expectation$success_msg
  } else {
    failure_msg <- message <- testthat_expectation$message
    success_msg <- "unknown"
  }
  structure(
    list(
      passed = passed, error = error, message = message,
      failure_msg = failure_msg, success_msg = success_msg
    ),
    class = c("swirl_expectation", "expectation")
  )
}


## CUSTOM EXPECTATIONS FOR ANSWER TESTS 

uses_func <- function(expected, label = NULL, ...){
  if(is.null(label)){
    label <- findExpr("expected")
  }else if (!is.character(label) || length(label) != 1) {
    label <- deparse(label)
  }
  function(expr){
    uses <- (is.call(expr) || is.expression(expr)) && 
      expected %in% flatten(expr)
    expectation(identical(uses, TRUE),
                str_c("does not use ", label))
  }
}

in_range <- function(range, label=NULL){
  range <- sort(range)
  function(number){
    isOK <- is.numeric(number) && 
      isTRUE(number >= range[1]) && 
      isTRUE(number <= range[2])
    expectation(identical(isOK, TRUE), 
                str_c("is not between ", range[1], " and ", range[2]))
  }
}





