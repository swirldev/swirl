#' Answer Tests
#'
#' Answer tests are how swirl determines whether a user has answered
#' a question correctly or not. Each question has one or more answer
#' tests associated with it, all of which must be satisfied in order for
#' a user's response to be considered correct. As the instructor, you 
#' can specify any combination of our standard answer tests or create your
#' own custom answer tests to suit your specific needs. This document will
#' explain your options.
#' 
#' @details
#' For each question that you author as part of a swirl lesson, you 
#' must specify exactly one \emph{correct answer}. This is separate and
#' distinct from the answer tests. This does not have to be
#' the only correct answer, but it must answer the question correctly. 
#' If a user \code{\link{skip}}s your question, this is the answer that will be
#' entered on his or her behalf.
#' 
#' If you're using the \href{https://github.com/swirldev/swirlify}{swirlify}
#' authoring tool, the correct answer will
#' be automatically translated into the appropriate answer test for most
#' question types. Questions that require the user to enter a valid
#' command at the R prompt (which we'll refer to as \emph{command questions})
#' are the only exception. Since there are often many possible ways to
#' answer a command question, you must determine how you'd like swirl to
#' assess the correctness of a user's response. This is where answer
#' tests come in.
#' 
#' You can specify any number of answer tests. If you use more than one, you
#' must separate them with semicolons. If you do not specify any answer tests
#' for a command question, then the default test will be used. The default
#' test is \code{omnitest(correctExpr='<correct_answer_here>')} and will simply
#' check that the user's expression matches the expression that you provided 
#' as a correct answer.
#'
#' @name AnswerTests
#' 
#' @section Standard Answer Tests:
#' Each of the standard answer tests listed below has
#' its own help file, for which you'll find a link at the bottom 
#' of this page.
#' 
#' \code{\link{expr_creates_var}}: Test that a new variable has been created.
#' 
#' \code{\link{expr_identical_to}}: Test that the user has entered a particular expression.
#' 
#' \code{\link{expr_is_a}}: Test that the expression itself is of a specific \code{\link{class}}.
#' 
#' \code{\link{expr_uses_func}}: Test that a particular function has been used.
#' 
#' \code{\link{func_of_newvar_equals}}: Test the result of a computation such as \code{mean(newVar)} applied to a specific (user-named) variable created in a previous question.
#' 
#' \code{\link{omnitest}}: Test for a correct expression, a correct value, or both.
#' 
#' \code{\link{val_has_length}}: Test that the value of the expression has a particular \code{\link{length}}.
#' 
#' \code{\link{val_matches}}: Test that the user's expression matches a regular expression (\code{\link{regex}}).
#' 
#' \code{\link{var_is_a}}: Test that the \emph{value} of the expression is of a specific \code{\link{class}}.
#' 
#' @section Custom Answer Tests:
#' [INFO ON CUSTOM TESTS GOES HERE...]
#' @family AnswerTests
NULL

 
#' Test for a correct expression, a correct value, or both.
#' 
#' Omnitest can test for a correct expression, a correct value,
#' or both. In the case of values it is limited to testing for 
#' character or numerical vectors of length 1. For course authors
#' only, omnitest is not exported.
#' @param correctExpr the correct or expected expression as a string
#' @param correctVal the correct value (numeric or character)
#' @param strict a logical value indicating that the expression should be as expected even if the value is correct. If FALSE (the default) a correct value will pass the test even if the expression is not as expected, but a notification will be issued.
#' @examples
#' \dontrun{
#' 
#'   # Test that a user has chosen a correct menu item
#'   #
#'   omnitest(correctVal='Men in a college dorm.')
#'    
#'   # Test that a user has entered a correct number at the
#'   # command line
#'   #
#'   omnitest(correctVal=19)
#'    
#'   # Test that a user has entered a particular command
#'   #
#'   omnitest('myVar <- c(3, 5, 7)')
#'    
#'   # Test that a user has entered a command which computes
#'   # a specific value but perhaps in a different manner 
#'   # than anticipated
#'   #
#'   omnitest('sd(x)^2', 5.95)
#'   #
#'   # If the user enters sd(x)*sd(x), rather than sd(x)^2, a notification
#'   # will be issued, but the test will not fail.
#'   
#'   # Test that a user has entered a command which computes
#'   # a specific value in a particular way
#'   #
#'   omnitest('sd(x)^2', 5.95, strict=TRUE)
#'   #
#'   # In this case, if the user enters sd(x)*sd(x) the test will fail.
#'   
#'   }
#'   @family AnswerTests
omnitest <- function(correctExpr=NULL, correctVal=NULL, strict=FALSE){
  e <- get("e", parent.frame())
  # Trivial case
  if(is.null(correctExpr) && is.null(correctVal))return(TRUE)
  # Testing for correct expression only
  if(!is.null(correctExpr) && is.null(correctVal)){
    return(expr_identical_to(correctExpr))
  }
  # Testing for both correct expression and correct value
  # Value must be character or single number
  valGood <- NULL
  if(!is.null(correctVal)){
    if(is.character(e$val)){
      valResults <- expectThat(e$val,
                               is_equivalent_to(correctVal, label=correctVal),
                               label=(e$val))
      if(is(e, "dev") && !valResults$passed)swirl_out(valResults$message)
      valGood <- valResults$passed
      # valGood <- val_matches(correctVal)
    } else if(!is.na(e$val) && is.numeric(e$val) && length(e$val) == 1){
      cval <- try(as.numeric(correctVal), silent=TRUE)
      valResults <- expectThat(e$val, 
                            equals(cval, label=correctVal),
                            label=toString(e$val))
      if(is(e, "dev") && !valResults$passed)swirl_out(valResults$message)
      valGood <- valResults$passed
    }
  }
  exprGood <- ifelse(is.null(correctExpr), TRUE, expr_identical_to(correctExpr))
  if(valGood && exprGood){
    return(TRUE)
  } else if (valGood && !exprGood && !strict){
      swirl_out("That's not the expression I expected but it works.")
      swirl_out("I've executed the correct expression in case the result is needed in an upcoming question.")
      eval(parse(text=correctExpr),globalenv())
      return(TRUE)
    } else {
      return(FALSE)
    }
}

#' Test that the user has entered an expression identical to that
#' given as the first argument.
#' @param correct_expression the correct or expected expression as a string
#' @return TRUE or FALSE
#' @examples
#' \dontrun{
#'   # Test that a user has entered a particular command
#'   #
#'   expr_identical_to('myVar <- c(3, 5, 7)')
#' }
#' @family AnswerTests
expr_identical_to <- function(correct_expression){
  e <- get("e", parent.frame())
  expr <- e$expr
  if(is.expression(expr))expr <- expr[[1]]
  correct <- parse(text=correct_expression)[[1]]
  results <- expectThat(expr, 
                        is_identical_to(correct, label=correct_expression),
                        label=deparse(expr))
  if( is(e, "dev") && !results$passed)swirl_out(results$message) 
  return(results$passed)
}

#' Returns TRUE if as.character(e$val) matches the regular
#' expression given as the first argument.
#' @param regular_expression a regular expression which user value should match
#' @return TRUE or FALSE
#' @examples
#' \dontrun{
#'   # Test that a user has entered a value matching
#'   # '[Cc]ollege [Ss]tudents' or has selected it 
#'   # in a multiple choice question.
#'   #
#'   val_matches('[Cc]ollege [Ss]tudents')
#' }
#' @family AnswerTests
val_matches <- function(regular_expression) {
  e <- get("e", parent.frame())
  userVal <- str_trim(as.character(e$val))
  results <- expectThat(userVal, 
                        matches(regular_expression), 
                        label=userVal)
  if(is(e,"dev") && !results$passed)swirl_out(results$message)
  return(results$passed)
}


#' Returns TRUE if a variable of the given name exists
#' in the global environment and is of the given class.
#' @param class expected class which the given variable
#' @param var_name name of the variable
#' @return TRUE or FALSE
#' @examples
#' \dontrun{
#' # Test that a variable named "x" in the global environmentis numeric.
#' var_is_a('numeric', 'x')
#' }
#' @family AnswerTests
var_is_a <- function(class, var_name) {
  e <- get("e", parent.frame())
  class <-  str_trim(class)
  var_name <- str_trim(var_name)
  if(exists(var_name, globalenv())){
    val <- get(var_name, globalenv())
    label <- val
    results <- expectThat(val, is_a(class), label=label)
    if(is(e,"dev") && !results$passed)swirl_out(results$message)
    return(results$passed)
  } else {
    if(is(e,"dev"))swirl_out(paste(var_name, "does not exist."))
    return(FALSE)
  }
}

#' Returns TRUE if e$expr is of the given class
#' @param class expected class of the given expression
#' @return TRUE or FALSE
#' @examples
#' \dontrun{
#' # Test if the expression entered by a user is an assignment
#' #
#' expr_is_a('<-')
#' }
#' @family AnswerTests
expr_is_a <- function(class) {
  e <- get("e", parent.frame())
  class <-  str_trim(class)
  expr <- e$expr
  label <- deparse(e$expr)
  results <- expectThat(expr, is_a(class), label=label)
  if(is(e,"dev") && !results$passed)swirl_out(results$message)
  return(results$passed)
}

#' Returns TRUE if the e$expr uses the function whose
#' name is given as the first argument.
#' @param func name of the function expected to be used
#' @return TRUE or FALSE
#' @examples
#' \dontrun{
#' # Test that the user has entered an expression using sd()
#' #
#' expr_uses_func('sd')
#' }
#' @family AnswerTests
expr_uses_func <- function(func) {
  e <- get("e", parent.frame())
  func <- str_trim(func)
  results <- expectThat(e$expr,
                        uses_func(func, label=func), 
                        label=deparse(e$expr))
  if(is(e,"dev") && !results$passed)swirl_out(results$message)
  return(results$passed)
}

#' Tests if the e$expr creates one new variable (of correct name
#' if given.) If so, returns TRUE.
#' @param correctName expected name of the new variable or NULL
#' @return TRUE or FALSE
#' @examples
#' \dontrun{
#' # Test if the user has entered an expression which creates
#' # a new variable of any name.
#' expr_creates_var()
#' #
#' # Test if the user has entered an expression which creates
#' # a variable named 'myNum'
#' #
#' expr_creates_var('myNum')
#' }
#' @family AnswerTests
expr_creates_var <- function(correctName=NULL){
  e <- get("e", parent.frame())
  # TODO: Eventually make auto-detection of new variables an option.
  # Currently it can be set in customTests.R
  delta <- if(!customTests$AUTO_DETECT_NEWVAR){
    safeEval(e$expr, e)
  } else {
    e$delta
  }
  if(is.null(correctName)){
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

#' Test the the length of e$val is that given by the first argument
#' @param len expected length of the variable created by a user
#' @return TRUE or FALSE
#' @examples
#' \dontrun{
#' # Test that the user has created a varible of length 10
#' #
#' val_has_length(10)
#' }
#' @family AnswerTests
val_has_length <- function(len){
  e <- get("e", parent.frame())
  try(n <- as.integer(len), silent=TRUE)
  if(is.na(n)){
    stop(message=paste("BUG: specified length", len,
                                 "is not an integer."))
  }
  results <- expectThat(length(e$val), equals(n, label=n), 
                        label=paste0("length(c(", toString(e$val), "))"))                                                   
  if( is(e, "dev") && !results$passed)swirl_out(results$message) 
  return(results$passed)
}

#' Tests the result of a computation such as mean(newVar) applied
#' to a specific variable created in a previous question and
#' saved as e$newVar.
#' @param correct_expression expression expected to be applied
#' @return TRUE or FALSE
#' @examples
#' \dontrun{
#' # Test if user has taken the mean of a variable created
#' # in an earlier question.
#' #
#' func_of_newvar_equals('mean(newVar)')
#' }
#' @family AnswerTests
func_of_newvar_equals <- function(correct_expression){
  e <- get("e", parent.frame())
  e1 <- cleanEnv(e$snapshot)
  assign(e$newVarName, e$newVar, e1)
  correctExpr <- gsub("newVar", e$newVarName, correct_expression)
  ans <- eval(parse(text=correctExpr), e1)
  results <- expectThat(e$val, 
                        equals(ans, 
                               label=correctExpr), 
                        label=deparse(e$expr))
  if(is(e, "dev") && !results$passed)swirl_out(results$message)
  return(results$passed)
}




