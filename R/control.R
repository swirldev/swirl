source("R/states.R")

hi <- function(){
  removeTaskCallback(id="tmod")
  if(exists("module"))rm(module, envir=globalenv())
  # Create a new container for global data and state of user's progress
  temp <- new.env(parent = emptyenv())
  # Read the test module into it
  temp$mod <- read.csv("data/testMod.csv", as.is=TRUE)
  # Store mod's number of rows there too.
  temp$rows <- nrow(temp$mod)
  # Assign temp, which is only known inside this function,
  # to a variable named "module" in the global environment.
  assign("module", temp, envir=globalenv() )
  # Store the initial state in the global variable, "module".
  module$state <- nextState(structure(list(row=0), class="tmod"))
  # Register the function cback(), below, as a callback. This
  # means that R will automatically invoke it whenever the user
  # enters a successful expression at the R prompt.
  addTaskCallback(cback, name="tmod")
  # return invisibly; i.e., don't display a result
  invisible()
}

# Resumes instruction
nxt <- function(){
  invisible()
}

# Cleans up
bye <- function(){
  removeTaskCallback(id="tmod")
  rm(module, envir=globalenv())
  invisible()
}

# Once registered by function hi(), this is invoked whenever the
# user enters a successful command at the R prompt. The first
# argument, expr, is a parsed expression representing what the
# user has entered. The second argument, val, is the value
# returned by that expression. The third and fourth parameters
# are technically required but useless here.
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
  n <- 1 # to avoid any infinte loop
  while(n < 20){
    n <- n+1
    # Get the current state from the global variable, "module".
    state <- module$state
    # If the current state is NULL, we are done. Return FALSE
    # to remove this callback from the task list.
    if(is.null(state))return(FALSE)
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

askingHelp <- function(expr){
  # Returns TRUE if the user has asked for help at the R prompt.
  # Take my word for it.
  if(class(expr)=="expression")expr <- expr[[1]]
  return(expr[[1]] == "?" || expr[[1]] == "help")
}