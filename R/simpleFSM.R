usrVars <- new.env(parent=globalenv())

#' Callback as FSM (finite state machine,) take 1.
#' 
#' Trivial demo of a callback which can remember its state while in
#' operation, or save and restore it on command. Its state consists
#' of an integer and it counts from 1 to 5.
#' 
#' Source this file and invoke hi() at the prompt. It will print 1.
#' If you type nxt() it will print 2. If you type brk(), it will save
#' its state, n, in the binary file "data/saved.RData," and quit.
#' If you type hi(TRUE) it will restore its state from the file
#' and resume where it left off, typing 3, 4, and 5 after every nxt(). 

#' Defines a function within a function, thus making the environment
#' of the enclosing function the parent of the enclosed. The parent
#' environment persists through multiple invocations of the enclosed
#' function (whose environments disappear.) The persistent state, n, 
#' lives in the parent.
makeFSM <- function(loadfile=FALSE){
  # This function is called only once and its environment persists.
  # The state, n, lives here and persists along with it.
  if(loadfile){
    # restore the saved state
    load("data/saved.RData", envir=globalenv())  ############### LOADS ALL INTO GLOBAL
  } else {
    # else reset n
    n <- 1
  }
  # This function is returned by makeFSM and is invoked multiple
  # times as callback. Each invocation results in a new environment,
  # so this function cannot "remember" things between invocations
  # on its own. The persistent state, n, is passed from the
  # parent environment.
  function(expr, val, ok, vis, data=n){
    # If user makes an assignment then evaluate expr in usrVars environment
    if(identical(class(expr), "<-")) eval(expr, envir=usrVars)
    print(ls(envir=usrVars))
    # Save user progress after every top-level task
    save(list=c("n", ls(envir=usrVars)), file="data/saved.RData" )  ##### REPLACES USRVARS EVERY RUN
    # If the user has typed brk(), then suspend the callback
    if(is.call(expr)){
      if(expr[[1]] == "brk") return(FALSE)
    }
    # Note: the parent environment can be accessed directly
    # from here using parent.env(environment()). Thus the child has
    # complete control over the persistent parent.
    print(paste("n =", n))
    # Set n of the parent environment (<<-) to n+1.
    n <<- n+1
    # Continue as callback (return TRUE) unless n > 10.
    return(n <= 10)
  }
}

nxt <- function(){invisible()}

brk <- function(){invisible()}
  
hi <- function(loadfile=FALSE){
  # Remove all active callbacks
  while(length(getTaskCallbackNames()) > 0) removeTaskCallback(1)
  if(!loadfile && file.exists("data/saved.RData")) {
    yn <- readline("Are you sure you want to overwrite your saved progress? ")
    if(tolower(substring(yn, 1, 1)) == "n") {
      readline("Okay. I'll start you were you left off. Press <enter> to continue...")
      return(hi(TRUE))
    } else {
      readline("You're the boss! I'll start you from the beginning. Press <enter> to continue...")
    }
  }
  # Create and register a callback with persistent parent.
  addTaskCallback(makeFSM(loadfile), name="simpleFSM")
  invisible()
}