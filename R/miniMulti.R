#' Demo of minimal code which multitasks between the R prompt and
#' a swirl tutorial, using a persistent environment to store data
#' between invocations of the tutorial.
#' 
#' Hadley Wickham, Advanced R Programming: "Environments have 
#' reference semantics: R's usual copy on modify rules do not
#' apply. Whenever you modify an environment, you modify every copy."
#' http://adv-r.had.co.nz/Environments.html#environment-basics
#' 
#' Invoke hi() for the default illustration and hi("alt") for an
#' alternate.

#' Creates an environment, e, defines a function, cb, and registers
#' cb as a callback with data argument, e. The callback retains a
#' reference to the environment in which it was created, environment(cb),
#' hence that environment, which also contains e, persists as long
#' as cb remains registered. Thus e can be used to store infomation
#' between invocations of cb. 
hi <- function(resume.class="default"){
  # e lives here, in the environment created when hi() is run
  e <- new.env(globalenv())
  # The callback also lives in the environment created when hi()
  # is run and retains a reference to it. Because of this reference,
  # the environment which contains both e and cb() persists as
  # long as cb() remains registered.
  cb <- function(expr, val, ok, vis, data=e){
    # The following will modify the persistent e, as per Hadley's
    # remark, above.
    e$expr <- expr
    e$val <- val
    e$ok <- ok
    e$vis <- vis
    # This dummy object of class resume.class "tricks" the S3 system
    # into calling the proper resume method.
    return(resume(structure(e,class=resume.class )))
  }
  bye()
  addTaskCallback(cb, name="mini")
  invisible()
}

bye <- function(){
  removeTaskCallback("mini")
  invisible()
}

#' ILLUSTRATIVE RESUME METHODS
#' The top level code for running a swirl module would reside in
#' an appropriate resume method. The default and alternate given
#' below might give the idea. A serious resume method would support
#' user registration, choice of module, testing answers, saving
#' user progress, etc.  

resume <- function(...)UseMethod("resume")

resume.default <- function(e){
  if(!exists("cmd.history", e)){
    swirl_out("I'm the default resume method. I merely keep, and print, a cumulative list of exressions entered by the user at the R prompt. When the list reaches 5 items, I return FALSE causing the callback to exit.")
    e$cmd.history <- list()
  }
  e$cmd.history <- c(e$expr, e$cmd.history)
  lapply(e$cmd.history, print)
  return(length(e$cmd.history) < 5)
  invisible()
}

resume.alt <- function(e){
  if(!exists("n", e)){
    swirl_out("I'm an alternate resume method. I ask questions which require 3 types of user input: readline, command, and select.list. Then I quit.")
    e$n <- 1
    e$ans.history <- list()
  }
  if(e$n == 1){
    e$name <- readline("What is your name? ")
    e$n <- e$n+1
    swirl_out("Please enter a valid command at the R prompt:")
    e$n <- e$n+1
    return(TRUE)
  } else if(e$n==3){
    e$cmd <- deparse(e$expr)
    swirl_out("Which of the following is your least favorite?")
    e$fav <- select.list(list("C++", "R", "Python"), graphics=FALSE)
    swirl_out(paste("Your name is", e$name))
    swirl_out(paste("You entered the command", e$cmd))
    swirl_out(paste("Your least favorite language is", e$fav))
    return(FALSE)
  }
}


#' UTILITIES

library(stringr)

swirl_out <- function(...) {
  wrapped <- strwrap(str_c(..., sep = " "),
                     width = getOption("width") - 2)
  message(str_c("| ", wrapped, collapse = "\n"))
}