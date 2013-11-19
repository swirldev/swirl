source("R/states.R")

hi <- function(){
  # Clean any leftover tmod callback or modules
  removeTaskCallback(id="tmod")
  if(exists("module"))rm(module, envir=globalenv())
  # Create a new instance of module in the global environment.
  assign("module", new.env(), envir=globalenv() )
  # Read the test module into it
  module$mod <- read.csv("data/testMod4Daphne.csv", as.is=TRUE)
  # Store the test module's number of rows there too.
  module$rows <- nrow(module$mod)
  # And a vector of names to ignore while tracking user progress.
  module$ignore <- c("mod", "rows", "state", "ignore", "suspended",
                     "mirror")
  # And the initial state,
  module$state <- nextState(structure(list(row=0), class="tmod"))
  # an indication that the lesson is in progress
  module$suspended <- FALSE
  # And a list of length 2 to act as mirror
  module$mirror <- lapply(1:2, function(x)module$mirror[[x]]<-list())
  # Register the function cback(), below, as a callback. This
  # means that R will automatically invoke it whenever the user
  # enters a successful expression at the R prompt.
  addTaskCallback(cback, name="tmod")
  # return invisibly; i.e., don't display a result
  invisible()
}

#' Resumes instruction
nxt <- function(){
  invisible()
}

#' Cleans up
bye <- function(){
  prettyOut("Thanks for stopping by!")
  removeTaskCallback(id="tmod")
  if(exists("module"))rm(module, envir=globalenv())
  invisible()
}

#' Once registered by function hi(), this is invoked whenever the
#' user enters a successful command at the R prompt. The first
#' argument, expr, is a parsed expression representing what the
#' user has entered. The second argument, val, is the value
#' returned by that expression. The third and fourth parameters
#' are technically required but useless here.
cback <- function(expr, val, ok, vis){
  # If the user has asked R for help, do nothing in the course.
  if(askingHelp(expr))return(TRUE)
  # If the user has asked to start or resume, do so.
  if(identical(expr, parse(text="nxt()")[[1]]) ||
       identical(expr, parse(text="hi()")[[1]])){
    module$suspended <- FALSE
  }
  # If instruction is suspended, do nothing.
  if(module$suspended)return(TRUE) 
  # Otherwise, process the current state.
  updateMirror(expr, ok)
  n <- 1 # to avoid any infinte loop
  while(n < 20){
    n <- n+1
    # Get the current state from the global variable, "module".
    state <- module$state
    # If the current state is NULL, we are done. Return FALSE
    # to remove this callback from the task list.
    if(is.null(state)) {
      prettyOut("Thanks for stopping by!")
      return(FALSE)
    }    
      # Tell the current state to perform its next stage of operation
    response <- doStage(state, expr, val)
    # During that process, the user may have asked to suspend instruction.
    # Store the associated variable in the global variable, "module"
    module$suspended <- response$suspend
    if(response$finished){
      # If the current state is finished, make the next state current
      # by storing it in the global variable, "module."
      module$state <- nextState(response$state)
    } else {
      # Otherwise, continue with the current state
      module$state <- response$state
    }
    
    if(response$prompt)break
  } 
  return(TRUE)
}

### UTILITIES

#' Returns TRUE if the user has asked for help at the R prompt.
#' Take my word for it. OK; maybe not.
askingHelp <- function(expr){
  if(is.expression(expr))expr <- expr[[1]]
  if(!is.call(expr))return(FALSE)
  return(expr[[1]] == "?" || expr[[1]] == "help")
}

#' The idea of the mirror is to track what the user has done
#' by reproducing it in a "protected" environment and keeping
#' a history some number of steps back, currently two.
updateMirror <- function(expr, ok){
  # Do not update for hi() or nxt()
  if(identical(expr, parse(text="nxt()")[[1]]) ||
       identical(expr, parse(text="hi()")[[1]])){
    return()
  }
  # Update only if the expression succeeded. (Currently, only
  # expressions which succeed will make it to this point, but
  # that may change in future versions of R.)
  if(ok){
    # Move current snapshots to the rear
    n <- length(module$mirror)
    while(n > 1){
      module$mirror[[n]] <- module$mirror[[n-1]]
      n <- n - 1
    }
    # Form a snapshot of the current mirror:
    # Get all existing variable names, except those ignored
    vars <- setdiff(ls(module), module$ignore)
    # Get the associated values
    module$mirror[[1]] <- lapply(vars, function(x)get(x,module))
    # Associate the values with their names
    names(module$mirror[[1]]) <- vars
    # Eval expr in the protected environment to mirror
    # what the user has done in the global environment
    eval(expr, module)
   }
}