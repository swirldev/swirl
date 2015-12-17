# Reference: Creating a More Robust Version of Omnitest, https://github.com/swirldev/swirl/issues/196

#' Recursively expand both the correct expression and the user's expression and
#' test for a match. CAUTION: May raise errors, as in rmatch_calls.
#' 
#' @export
#' @param expr1 expression
#' @param expr2 expression
#' @param eval_for_class TRUE or FALSE. If TRUE, evaluate the first argument of an S3 method to determine its class. Default=FALSE.
#' @param eval_env parent environment for evaluations to determine class. Ignored if eval_for_class=FALSE
#' @return TRUE or FALSE according to whether expanded expressions match.
#' @examples
#' \dontrun{
#'   
#'   expr1 <- quote(print(paste("my_name_is", "mud")))
#'   expr2 <- quote(print(paste("my_name_is", "mud", sep=" ")))
#'   err <- try(ans <- is_robust_match(expr1, expr2, eval_for_class=TRUE), silent=TRUE)
#'   if(is(ans, "try-error")){
#'     ans <- isTRUE(all.equal())
#'   }
#' }
is_robust_match <- function(expr1, expr2, eval_for_class, eval_env=NULL){
  expr1 <- rmatch_calls(expr1, eval_for_class, eval_env)
  expr2 <- rmatch_calls(expr2, eval_for_class, eval_env)
  isTRUE(all.equal(expr1, expr2))
}

#' Recursively expand match calls in an expression from the bottom up.
#' 
#' Given an expression, expr, traverse the syntax tree from the
#' bottom up, expanding the call to include default values of
#' named formals as appropriate, and applying match.call to the result.
#' Functionality is limited to expressions containing ordinary functions
#' or S3 methods. If parameter eval_for_class has its default value of FALSE,
#' an error will be raised for any S3 method whose first argument (as an expression) 
#' is not atomic. If eval_for_class is TRUE, the first argument will be evaluated
#' to determine its class. Evaluation will take place in the environment given by
#' parameter eval_env. 
#' CAUTION: eval_for_class=TRUE is likely to result in multiple evaluations of the same code.
#' Expressions containing S4 or reference class methods will also raise errors.
#' @export
#' @param expr an R expression (a.k.a. abstract syntax tree)
#' @param eval_for_class TRUE or FALSE. If TRUE, evaluate the first argument of an S3 method to determine its class. Default=FALSE.
#' @param eval_env environment in which to evaluate for class. Ignored if eval_for_class=FALSE
#' @return an equivalent R expression with function or method calls in canonical form.
#' @examples
#' \dontrun{
#' 
#' # Function
#' rmatch_calls(quote(help("print")))
#' help(topic = "print", package = NULL, lib.loc = NULL, verbose = getOption("verbose"), 
#' try.all.packages = getOption("help.try.all.packages"), help_type = getOption("help_type"))
#' 
#' # S3 method with atomic first argument
#' rmatch_calls(quote(seq(0, 1, by=.5)))
#' seq(from = 0, to = 1, by = 0.5, length.out = NULL, along.with = NULL)
#' 
#' # S3 method with non-atomic first argument, eval_for_class = FALSE (default)
#' rmatch_calls(quote(seq(as.Date("2014-02-01"), as.Date("2014-03-01"))))
#' #Error in rmatch_calls(quote(seq(as.Date("2014-02-01"), as.Date("2014-03-01")))) : 
#' #  Illegal expression, seq(as.Date(x = "2014-02-01"), as.Date(x = "2014-03-01")): 
#' #  The first argument, as.Date(x = "2014-02-01"), to S3 method 'seq', is a call, 
#' #  which (as an expression) is not atomic, hence its class can't be determined in an 
#' #  abstract syntax tree without additional information.
#'  
#' # S3 method with non-atomic first argument, eval_for_class = TRUE
#' rmatch_calls(quote(seq(as.Date("2014-02-01"), as.Date("2014-03-01"))), eval_for_class=TRUE)
#' seq(from = as.Date(x = "2014-02-01"), to = as.Date(x = "2014-03-01"), 
#'     length.out = NULL, along.with = NULL)  
#' }
rmatch_calls <- function(expr, eval_for_class=FALSE, eval_env=NULL){
  # If expr is not a call, just return it.
  if(!is.call(expr))return(expr)
  # Replace expr's components with matched versions.
  for(n in 1:length(expr)){
    expr[[n]] <- rmatch_calls(expr[[n]],eval_for_class)
  }
  # If match.fun(expr[[1]]) raises an exception here, the code which follows
  # would be likely to give a misleading result. Catch the error merely to
  # produce a better diagnostic.
  tryCatch(fct <- match.fun(expr[[1]]),
           error=function(e)stop(paste0("Illegal expression ", dprs(expr), 
                                        ": ", dprs(expr[[1]]), " is not a function.\n")))
  # If fct is a special function such as `$`, or builtin such as `+`, return expr.
  if(is.primitive(fct)){
    return(expr)
  }
  # If fct is an (S4) standardGeneric, match.call is likely to give a misleading result,
  # so raise an exception. (Note that builtins were handled earlier.)
  if(is(fct, "standardGeneric")){
    stop(paste0("Illegal expression, ", dprs(expr), ": ", dprs(expr[[1]]), " is a standardGeneric.\n"))
  }
  # At this point, fct should be an ordinary function or an S3 method.
  if(isS3(fct)){
    # If the S3 method's first argument, expr[[2]], is anything but atomic 
    # its class can't be determined here without evaluation.
    if(!is.atomic(expr[[2]]) & !eval_for_class){
      stop(paste0("Illegal expression, ", dprs(expr),": The first argument, ", dprs(expr[[2]]), 
                  ", to S3 method '", dprs(expr[[1]]), 
                  "', is a ", class(expr[[2]]) , ", which (as an expression) is not atomic,",
                  " hence its class can't be determined in an abstract",
                  " syntax tree without additional information.\n"))
    }
    # Otherwise, attempt to find the appropriate method.
    if(is.null(eval_env)){
      eval_env <- new.env()
    } else {
      eval_env <- new.env(parent=eval_env)
    }
    temp <- eval(expr[[2]], envir = eval_env)
    classes <- try(class(temp), silent=TRUE)
    for(cls in classes){
      err <- try(fct <- getS3method(as.character(expr[[1]]), cls), silent=TRUE)
      if(!is(err, "try-error"))break
    }
    # If there was no matching method, attempt to find the default method. If that fails,
    # raise an error
    if(is(err, "try-error")){
      tryCatch(fct <- getS3method(as.character(expr[[1]]), "default"),
               error = function(e)stop(paste0("Illegal expression ", dprs(expr), ": ",
                                              "There is no matching S3 method or default for object, ",
                                              dprs(expr[[2]]), ", of class, ", cls,".\n")))
    }
  }
  # Form preliminary match. If match.call raises an error here, the remaining code is
  # likely to give a misleading result. Catch the error merely to give a better diagnostic.
  tryCatch(expr <- match.call(fct, expr),
           error = function(e)stop(paste0("Illegal expression ", dprs(expr), ": ", 
                                          dprs(expr[[1]]), " is not a function.\n")))
  # Append named formals with default values which are not included
  # in the preliminary match
  fmls <- formals(fct)
  for(n in names(fmls)){
    if(!isTRUE(fmls[[n]] == quote(expr=)) && !(n %in% names(expr[-1]))){
      expr[n] <- fmls[n]
    }
  }
  # match call again, for order
  expr <- match.call(fct, expr)
  return(expr)
}
# Helpers
isS3 <- function(fct)isTRUE(grep("UseMethod", body(fct)) > 0)
dprs <- function(expr)deparse(expr, width.cutoff=500)
