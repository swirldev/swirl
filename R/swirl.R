#' An interactive learning environment for R and statistics
#' 
#' This function presents a choice of course modules and interactively
#' tutors a user through them. A user may be asked to watch a video, to
#' answer a multiple-choice or fill-in-the-blanks question, or to
#' enter a command in the R console precisely as if he or she were 
#' using R in practice. Emphasis is on the last, interacting with the
#' R console. User responses are tested for correctness and hints are
#' given if appropriate. Progress is automatically saved so that a user
#' may quit at any time and later resume without losing work.
#' 
#' The usual way to exit swirl is to type bye() in the R console.
#' A user may return to the R console at any time by pressing the Esc
#' key. The Esc key alone, however, does not cause swirl to exit. A
#' user must type bye() in addition. Swirl will print a goodbye message
#' whenever it exits to signal that it is no longer in operation. 
#' 
#' While swirl is in operation, it may be controlled by entering special
#' commands in the R console, using the Esc key to return to the R console
#' if necessary. One of the special commands is bye() as discussed above.
#' Others are play(), nxt(), skip(), and info(). The parentheses are
#' important.
#' 
#' Sometimes a user will want to play around in the R console without
#' interference or commentary from swirl. This can be accomplished by
#' using the special command, play(). Swirl will remain in operation,
#' silently, until the special command nxt() is entered. In general,
#' nxt() can always be used to display the upcoming question.
#' 
#' The special command, skip(), can be used to skip a question if 
#' necessary. Swirl will enter the correct answer and notify the
#' user of the names of any new variables which it may have created
#' in doing so. These may be needed for subsequent questions.
#' 
#' Finally, info() may be used to display a list of the special commands
#' themselves with brief explanations of what they do.
#' @param resume.class for development only; please accept the default.
#' @export
swirl <- function(resume.class="default"){
  # Creates an environment, e, defines a function, cb, and registers
  # cb as a callback with data argument, e. The callback retains a
  # reference to the environment in which it was created, environment(cb),
  # hence that environment, which also contains e, persists as long
  # as cb remains registered. Thus e can be used to store infomation
  # between invocations of cb.
  removeTaskCallback("mini")
  # e lives here, in the environment created when swirl() is run
  e <- new.env(globalenv())
  # The callback also lives in the environment created when swirl()
  # is run and retains a reference to it. Because of this reference,
  # the environment which contains both e and cb() persists as
  # long as cb() remains registered.
  cb <- function(expr, val, ok, vis, data=e){
    # The following will modify the persistent e
    e$expr <- expr
    e$val <- val
    e$ok <- ok
    e$vis <- vis
    # This dummy object of class resume.class "tricks" the S3 system
    # into calling the proper resume method.
    return(resume(structure(e,class=resume.class )))
  }
  addTaskCallback(cb, name="mini")
  invisible()
}

## SPECIAL COMMANDS

#' Exit swirl.
#' 
#' Swirl operates by installing a callback function which responds
#' to commands entered in the R console. This is how it captures
#' and tests answers given by the user in the R console. Swirl will
#' remain in operation until this callback is removed, which is
#' what bye() does.
#' @export
bye <- function(){
  removeTaskCallback("mini")
  swirl_out("Leaving swirl now...")
  invisible()
}

#' Begin the upcoming question or unit of instruction.
#' 
#' This is the way to regain swirl's attention after viewing
#' a video or play()'ing around in the console. 
#' @export
nxt <- function(){invisible()}

#' Skip the current unit of instruction
#' 
#' Swirl will enter the correct answer and notify the
#' user of the names of any new variables which it may have created
#' in doing so. These may be needed for subsequent questions.
#' @export
skip <- function(){invisible()}

#' Tell swirl to ignore console input for a while
#' 
#' It is somethimes useful to play around in the R console out of
#' curiosity or to solidify a concept. This command will cause
#' swirl to remain idle, allowing the user to experiment at will,
#' until the command nxt() is entered. 
#' @export
play <- function(){invisible()}

#' Display a list of special commands.
#' 
#' Display a list of the special commands, bye(), play(), nxt(),
#' skip(), and info().
#' @export
info <- function(){
  swirl_out()
  swirl_out(" When you are in the R console:")
  swirl_out()
  swirl_out("-- Typing skip() allows you to skip the current question.")
  swirl_out()
  swirl_out("-- Typing play() lets you experiment with R on your own; swirl will ignore what you do...")
  swirl_out("-- UNTIL you type nxt() which will regain swirl's attention.")
  swirl_out()
  swirl_out("-- Typing bye() causes swirl to exit. Your progress will be saved.")
  swirl_out()
  swirl_out("-- Typing info() displays these options again.")  

  swirl_out()
  invisible()
}

## RESUME METHOD

resume <- function(...)UseMethod("resume")

# Default method resume implements a finite state (or virtual) machine. 
# It runs a fixed "program" consisting of three "instructions" which in 
# turn present information, capture a user's response, and test and retry 
# if necessary. The three instructions are themselves S3 methods which 
# depend on the class of the active row of the course module. The 
# instruction set is thus extensible. It can be found in R/instructionSet.R. 
# 
resume.default <- function(e){
  # Trap special functions
  if(uses_func("info")(e$expr)[[1]]){
    return(TRUE)
  }
  if(uses_func("nxt")(e$expr)[[1]]){
    e$playing <- FALSE
    e$iptr <- 1
  }
  if(uses_func("play")(e$expr)[[1]]){
    e$playing <- TRUE
  }
  # If the user is playing, ignore console input,
  # but remain in operation.
  if(exists("playing", envir=e, inherits=FALSE) && e$playing)return(TRUE)
  # If the user wants to skip the current question, do the bookkeeping.
  if(uses_func("skip")(e$expr)[[1]]){
    # Increment a skip count kept in e.
    if(!exists("skips", e))e$skips <- 0
    e$skips <- 1 + e$skips
    # Enter the correct answer for the user.
    correctAns <- e$current.row[,"CorrectAnswer"]
    e$expr <- parse(text=correctAns)[[1]]
    e$val <- eval(e$expr)
    # Evaluate it in the global environment
    eval(e$expr, globalenv())
    # Inform the user, but don't expose the actual answer.
    swirl_out()
    swirl_out("I've entered the correct answer for you.")
    temp <- new.env()
    eval(e$expr, temp)
    temp <- ls(temp)
    if(length(temp) > 0){
      swirl_out(paste0("In doing so, I've created the variable(s) ", 
                       temp, ", which you may need later."))
    }
    swirl_out()
  }
  # Method menu initializes or reinitializes e if necessary.
  temp <- mainMenu(e)
  # If menu returns FALSE, the user wants to exit.
  if(is.logical(temp) && !isTRUE(temp)){
    swirl_out("Leaving swirl now...")
    return(FALSE)
  }
  # Execute instructions until a return to the prompt is necessary
  while(!e$prompt){
    # If the module is complete, save progress, remove the current
    # module from e, and invoke the top level menu method.
    if(e$row > nrow(e$mod)){
      saveProgress(e)
      # form a new path for the progress file
      # which indicates completion and doesn't
      # fit the regex pattern "[.]rda$" i.e.
      # doesn't end in .rda, hence won't be
      # recognized as an active progress file.
      new_path <- paste(e$progress,".done", sep="")
      # rename the progress file to indicate completion
      if(!file.exists(new_path))file.rename(e$progress, new_path)
      rm(mod, envir=e)
      # let the user select another course module
      temp <- mainMenu(e)
      # if menu returns FALSE, user wants to quit.
      if(is.logical(temp) && !isTRUE(temp)){
        swirl_out("Leaving swirl now...")
        return(FALSE)
    }
    }
    # If we are ready for a new row, prepare it
    if(e$iptr == 1){
      saveProgress(e)
      e$current.row <- e$mod[e$row,]
      # Prepend the row's swirl class to its class attribute
      attr(e$current.row,"class") <- c(e$current.row[,"Class"], 
                                       attr(e$current.row,"class"))
    }
    # Execute the current instruction
    e$instr[[e$iptr]](e$current.row, e)
  }
  e$prompt <- FALSE
  return(TRUE)
}

# DEPRECATED method resume.depr implements a finite state (or virtual) machine. 
# It runs a fixed "program" consisting of three "instructions" which in 
# turn present information, capture a user's response, and test and retry 
# if necessary. The three instructions are themselves S3 methods which 
# depend on the class of the active row of the course module. The 
# instruction set is thus extensible. It can be found in R/instructionSet.R. 
# 
resume.depr <- function(e){
  # This function is entered ONLY when the user has entered a
  # valid expression at the R prompt.
  #
  # We may be entering for the first time, in which case our environment
  # will not be fully initialized. We check for that and initialize if
  # necessary. We delegate to a method in order to ease integration of
  # additional functionality as the code base develops.
  fromhi <- (!exists("mod",e,inherits = FALSE))
  if(fromhi)initSwirl(e)
  # Execute instructions until a return to the prompt is necessary
  while(!e$prompt){
    # If the module is complete, return FALSE to remove callback
    if(e$row > nrow(e$mod)){
      saveProgress(e)
      # form a new path for the progress file
      # which indicates completion and doesn't
      # fit the regex pattern "[.]rda$" i.e.
      # doesn't end in .rda, hence won't be
      # recognized as an active progress file.
      new_path <- paste(e$progress,".done", sep="")
      # rename the progress file to indicate completion
      if(!file.exists(new_path))file.rename(e$progress, new_path)
      return(FALSE)
    }
    # If we are ready for a new row, prepare it
    if(e$iptr == 1){
      saveProgress(e)
      e$current.row <- e$mod[e$row,]
      # Prepend the row's swirl class to its class attribute
      attr(e$current.row,"class") <- c(e$current.row[,"Class"], 
                                       attr(e$current.row,"class"))
    }
    # Execute the current instruction
    e$instr[[e$iptr]](e$current.row, e)
  }
  e$prompt <- FALSE
  return(TRUE)
}