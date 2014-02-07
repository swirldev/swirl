# The following tests probably won't survive in their current form,
# as we are still trying to work out a syntax which is both brief
# and comprehensive. These tests apply to all courses except
# Data Analysis, Mathematical Biostatistics Boot Camp, and Open Intro.
# 
# The first two tests, expr_identical_to and val_matches cover
# all questions in Intro to R except one.
# 
# To cover the exception, four additional tests were necessary: 
# var_is_a, expr_uses, expr_creates_var, and val_has_length.
# 
# One additional test, expr_is_a, was needed for the test modules.
#
# Omnitest is an aggregate of more basic tests and is meant to cover
# many of the questions which have appeared in lessons so far.
# CAVEAT: Omnitest is completely untested. We will have to write
# a small module for the purpose.
 
# Omnitest can test for a correct expression, a correct value,
# or both. In the case of values it is limited to testing for 
# character or numerical vectors of length 1.
# 
# Examples:
# 
#   1. To test that a user has chosen a correct menu item
#    omnitest(correctVal='Men in a college dorm.')
#   2. To test that a user has entered a correct number at the
#   command line
#    omnitest(correctVal=19)
#   3. To test that a user has entered a particular command
#    omnitest('myVar <- c(3, 5, 7)')
#   4. To test that a user has entered a command which computes
#   a specific value but perhaps in a different manner than anticipated
#    omnitest('sd(x)^2', 5.95)
#   If the user enters sd(x)*sd(x), rather than sd(x)^2, a notification
#   will be issued, but the test will not fail.
#   5. To test that a user has entered a command which computes
#   a specific value in a particular way
#    omnitest('sd(x)^2', 5.95, strict=TRUE)
#   In this case, if the user enters sd(x)*sd(x) the test will fail.
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
  valResults <- NULL
  if(!is.null(correctVal)){
    if(is.character(e$val)){
      valResults <- val_matches(correctVal)
    } else if(!is.na(e$val) && is.numeric(e$val) && length(e$val) == 1){
      cval <- try(as.numeric(correctVal), silent=TRUE)
      valResults <- expectThat(e$val, 
                            equals(cval, label=correctVal),
                            label=toString(e$val))
    }
    if(is(e, "dev") && !valResults$passed)swirl_out(valResults$message)
  }
  valGood <- valResults$passed
  exprGood <- expr_identical_to(correctExpr)
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

# Test that the user has entered an expression identical to that
# given as the first argument.
expr_identical_to <- function(correct_expression){
  e <- get("e", parent.frame())
  expr <- e$expr
  if(is.expression(expr))expr <- expr[[1]]
  results <- expectThat(expr, 
                        is_identical_to(correct, label=rightside(keyphrase)),
                        label=deparse(expr))
  if( is(e, "dev") && !results$passed)swirl_out(results$message) 
  return(results$passed)
}

# Returns TRUE if as.character(e$val) matches the regular
# expression given as the first argument.
val_matches <- function(regular_expression) {
  e <- get("e", parent.frame())
  userVal <- str_trim(as.character(e$val))
  results <- expectThat(userVal, 
                        matches(regular_expression), 
                        label=userVal)
  if(is(e,"dev") && !results$passed)swirl_out(results$message)
  return(results$passed)
}


# Returns TRUE if a variable of the given name exists
# in the global environment and is of the given class.
var_is_a <- function(class, var_name) {
  e <- get("e", parent.frame())
  class <-  str_trim(class)
  var_name <- str_trim(var_name)
  if(exists(var_name, globalenv())){
    val <- get(variable, globalenv())
    label <- val
    results <- expectThat(val, is_a(class), label=label)
    if(is(e,"dev") && !results$passed)swirl_out(results$message)
    return(results$passed)
  } else {
    if(is(e,"dev"))swirl_out(paste(var_name, "does not exist."))
    return(FALSE)
  }
}

# Returns TRUE if e$expr is of the given class
expr_is_a <- function(class) {
  e <- get("e", parent.frame())
  class <-  str_trim(class)
  expr <- e$expr
  label <- deparse(e$expr)
  results <- expectThat(expr, is_a(class), label=label)
  if(is(e,"dev") && !results$passed)swirl_out(results$message)
  return(results$passed)
}

# Returns TRUE if the e$expr uses the function whose
# name is given as the first argument.
expr_uses_func <- function(func) {
  e <- get("e", parent.frame())
  func <- str_trim(func)
  results <- expectThat(e$expr,
                        uses_func(func, label=func), 
                        label=deparse(e$expr))
  if(is(e,"dev") && !results$passed)swirl_out(results$message)
  return(results$passed)
}

# Tests if the e$expr creates one new variable (of correct name
# if given.) If so, returns TRUE.
expr_creates_var <- function(correctName=NULL){
  e <- get("e", parent.frame())
  if(is.null(correctName)){
    results <- expectThat(e$expr, creates_var(), label=deparse(e$expr))
  } else {
    results <- expectThat(e$expr, 
                          creates_var(correctName, label=correctName), 
                          label=deparse(e$expr))
  }
  if(results$passed){
    e$newVar <- e$val
  } else if(is(e,"dev")){
    swirl_out(results$message)
  }
  return(results$passed)
}

# Test the the length of e$val is that given by the first argument
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

# Tests the result of a computation such as mean(newVar) applied
# to a specific variable created in a previous question and
# saved as e$newVar.
func_of_newvar_equals <- function(correct_expression){
  e <- get("e", parent.frame())
  correctExpr <- gsub("newVar", paste0("e$","newVar"), correct_expression)
  results <- expectThat(e$val, 
                        equals(eval(parse(text=correctExpr)), 
                               label=correct_expression), 
                        label=deparse(e$expr))
  if(is(e, "dev") && !results$passed)swirl_out(results$message)
  return(results$passed)
}




