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
# There are at least two syntactical issues. The first is that test
# specifications involve commas, hence must be surrounded by quotes
# in a csv file. The second is that e must be specified.
# E.g., "expr_creates_var(e, myVar); expr_uses_func(e,'c')"
# 

# Test that the user has entered an expression identical to that
# given as the first argument.
expr_identical_to <- function(e, correct_expression){
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
val_matches <- function(e, regular_expression) {
  userVal <- str_trim(as.character(e$val))
  results <- expectThat(userVal, 
                        matches(regular_expression), 
                        label=userVal)
  if(is(e,"dev") && !results$passed)swirl_out(results$message)
  return(results$passed)
}


# Returns TRUE if a variable of the given name exists
# in the global environment and is of the given class.
var_is_a <- function(e, class, var_name) {
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
expr_is_a <- function(e, class) {
  class <-  str_trim(class)
  expr <- e$expr
  label <- deparse(e$expr)
  results <- expectThat(expr, is_a(class), label=label)
  if(is(e,"dev") && !results$passed)swirl_out(results$message)
  return(results$passed)
}

# Returns TRUE if the e$expr uses the function whose
# name is given as the first argument.
expr_uses_func <- function(e, func) {
  func <- str_trim(func)
  results <- expectThat(e$expr,
                        uses_func(func, label=func), 
                        label=deparse(e$expr))
  if(is(e,"dev") && !results$passed)swirl_out(results$message)
  return(results$passed)
}

# Tests if the e$expr creates one new variable (of correct name
# if given.) If so, returns TRUE.
expr_creates_var <- function(e, correctName=NULL){
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
val_has_length <- function(e, len){
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
func_of_newvar_equals <- function(e, correct_expression){
  correctExpr <- gsub("newVar", paste0("e$","newVar"), correct_expression)
  results <- expectThat(e$val, 
                        equals(eval(parse(text=correctExpr)), 
                               label=correct_expression), 
                        label=deparse(e$expr))
  if(is(e, "dev") && !results$passed)swirl_out(results$message)
  return(results$passed)
}




