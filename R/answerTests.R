#' A kludge illustrating the use of testthat to check user responses.
#' Prototype course material specifies tests using a keyphrase syntax which,
#' for purposes of this illustration, must be converted to testthat syntax.
#' Hence, some of the awkwardness below. However, we find testthat syntax
#' very intuitive and powerful, facilitating the job of writing tests.
#' 
#' We found it expedient to adapt testthat's expect_that and find_expr 
#' functions which were designed for unit testing. The adaptations are
#' expectThat and findExpr, below. We also found it useful to write three
#' new expectations, uses_function, creates_var, and in_range.
#' 
#' Extensible testing using keyphrases.
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
  results <- expectThat(e$expr, is_a("<-"), label=deparse(e$expr))
  if(!results$passed)swirl_out(results$message)
  return(results$passed)
}

#' Returns TRUE if the function to the right of = in the keyphrase has
#' been used in e$expr
#'  
runTest.useFunc <- function(keyphrase, e) {
  func <- rightside(keyphrase)
  results <- expectThat(e$expr,
                        uses_func(func, label=func), 
                        label=deparse(e$expr))
  if(!results$passed)swirl_out(results$message)
  return(results$passed)
}

#' Returns TRUE if as.character(e$val) matches the string to the right
#' of "=" in keyphase
#' This is for single word answers
runTest.word <- function(keyphrase, e) {
  correctVal <- tolower(str_trim(rightside(keyphrase)))
  userVal <- str_trim(as.character(e$val))
  results <- expectThat(tolower(userVal), 
                        matches(correctVal), 
                        label=userVal)
  if(!results$passed)swirl_out(results$message)
  return(results$passed)
}
#' Returns TRUE if as.character(e$val) matches the string to the right
#' of "=" in keyphase
#' This is for multi-word answers for which order matters
runTest.word_order <- function(keyphrase, e) {
  correctVal <- rightside(keyphrase)
  temp <- tolower(str_trim(unlist(strsplit(correctVal,","))))
  correctVal <- paste(temp, collapse=",")
  temp <- str_trim(unlist(strsplit(as.character(e$val),",")))
  userAns <- paste(temp, collapse=",")
  results <- expectThat(tolower(userAns), 
                        is_identical_to(correctVal, label=correctVal), 
                        label=userAns)
  if(!results$passed)swirl_out(results$message)
  return(results$passed)
}
#' Returns TRUE if as.character(e$val) matches the string to the right
#' of "=" in keyphase
#' This is for multi-word answers for which order doesn't matter
runTest.word_many <- function(keyphrase,e){
  correctVal <- rightside(keyphrase)
  temp <- sort(tolower(str_trim(unlist(strsplit(correctVal,",")))))
  correctVal <- paste(temp, collapse=",")
  temp <- sort(str_trim(unlist(strsplit(as.character(e$val),","))))
  userAns <- paste(temp, collapse=",")
  results <- expectThat(tolower(userAns), 
                        is_identical_to(correctVal, label=correctVal), 
                        label=userAns)
  if(!results$passed)swirl_out(results$message)
  return(results$passed)
}

#' Tests if the user has just created one new variable. If so, assigns 
#' e$newVar its value and returns TRUE.
runTest.newVar <- function(keyphrase, e){
  results <- expectThat(e$expr, creates_var(), label=deparse(e$expr))
  if(results$passed){
    e$newVar <- e$val
  } else {
    swirl_out(results$message)
  }
  return(results$passed)
}

#' Tests if the user has just created one new variable of correct name. If so, 
#' returns TRUE.
runTest.correctName <- function(keyphrase, e){
  correctName <- rightside(keyphrase)
  results <- expectThat(e$expr, 
                        creates_var(correctName, label=correctName), 
                        label=deparse(e$expr))
  if(results$passed){
    e$newVar <- e$val
  } else {
    swirl_out(results$message)
  }
  return(results$passed)
 }

#' Tests the result of a computation such as mean(newVar) applied
#' to a specific variable created in a previous question. 
runTest.result <- function(keyphrase, e){
  correct.expr <- parse(text=rightside(keyphrase))
  newVar <- e$newVar
  results <- expectThat(e$val, 
                        equals(eval(correct.expr), label=rightside(keyphrase)), 
                        label=deparse(e$expr))
  if(!results$passed)swirl_out(results$message)
  return(results$passed)
}

runTest.exact <- function(keyphrase,e){
  if(is.numeric(e$val)){
    correct.ans <- eval(parse(text=rightside(keyphrase)))
    results <- expectThat(e$val, 
                          equals(correct.ans, label=correct.ans), 
                          label=e$val)
    if(!results$passed)swirl_out(results$message)
    return(results$passed)
  }
  return(FALSE)
}

runTest.range <- function(keyphrase,e){
  correct.ans <-parse(text=rightside(keyphrase))
  if (is.numeric(e$val)){
     correct.ans <- as.character(correct.ans)
     temp <- str_split(correct.ans,"-")
     temp <- as.numeric(unlist(str_split(correct.ans,"-")))
     results <- expectThat(e$val,
                           in_range(temp, label=temp), label=e$val)
     if(!results$passed)swirl_out(results$message)
     return(results$passed)
  }
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

### TESTTHAT FUNCTIONS CUSTOMIZED FOR ANSWERTESTS

findExpr <- function(name, env = parent.frame()){
  subs <- do.call("substitute", list(as.name(name), env))
  str_c(deparse(subs, width.cutoff = 500), collapse = "\n")
}

expectThat <- function(object, condition, info=NULL, label=NULL){
  if (is.null(label)) {
    label <- findExpr("object")
  }
  results <- condition(object)
  results$message <- str_c(label, " ", results$message)
  if (!is.null(info)) {
    results$message <- str_c(results$message, "\n", info)
  }
  return(results)
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

creates_var <- function(expected=NULL, label = NULL){
  function(expr){
    eval(expr)
    newVars <- setdiff(ls(),"expr")
    creates <- length(newVars) == 1
   asNamed <- is.null(expected) || identical(expected, newVars[1])
    message <- str_c("does not create a variable ")
    if(!is.null(expected)) message <- str_c(message, "named ", expected)
    expectation(identical(creates&&asNamed, TRUE), message)
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

