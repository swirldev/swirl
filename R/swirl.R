#' An interactive learning environment for R and statistics.
#' 
#' This function presents a choice of course lessons and interactively
#' tutors a user through them. A user may be asked to watch a video, to
#' answer a multiple-choice or fill-in-the-blanks question, or to
#' enter a command in the R console precisely as if he or she were 
#' using R in practice. Emphasis is on the last, interacting with the
#' R console. User responses are tested for correctness and hints are
#' given if appropriate. Progress is automatically saved so that a user
#' may quit at any time and later resume without losing work.
#' 
#' There are several ways to exit swirl: by typing \code{bye()} while in the R
#' console, by hitting the Esc key while not in the R console, or by
#' entering 0 from the swirl course menu. swirl will print a goodbye 
#' message whenever it exits. 
#' 
#' While swirl is in operation, it may be controlled by entering special
#' commands in the R console. One of the special commands is \code{bye()} 
#' as discussed above. Others are \code{play()}, \code{nxt()}, \code{skip()},
#' and \code{info()}. The parentheses are important.
#' 
#' Sometimes a user will want to play around in the R console without
#' interference or commentary from swirl. This can be accomplished by
#' using the special command \code{play()}. swirl will remain in operation,
#' silently, until the special command \code{nxt()} is entered.
#' 
#' The special command \code{skip()} can be used to skip a question if 
#' necessary. swirl will enter the correct answer and notify the
#' user of the names of any new variables which it may have created
#' in doing so. These may be needed for subsequent questions.
#' 
#' Finally, \code{info()} may be used to display a list of the special commands
#' themselves with brief explanations of what they do.
#' @param resume.class for development only; please accept the default.
#' @export
#' @importFrom stringr str_c str_trim str_split str_length 
#' @importFrom stringr str_detect str_locate fixed str_split_fixed
#' @importFrom testthat expectation equals is_equivalent_to 
#' @importFrom testthat is_identical_to is_a matches
#' @importFrom pwr pwr.norm.test pwr.t.test
#' @importFrom plotrix draw.circle
#' @import openintro
#' @examples
#' \dontrun{
#' 
#' swirl()
#' }
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
#' swirl operates by installing a callback function which responds
#' to commands entered in the R console. This is how it captures
#' and tests answers given by the user in the R console. swirl will
#' remain in operation until this callback is removed, which is
#' what \code{bye()} does.
#' @export
#' @examples
#' \dontrun{
#' 
#' | Create a new variable called `x` that contains the number 3.
#' 
#' > bye()
#' 
#' | Leaving swirl now. Type swirl() to resume.
#' }
bye <- function(){
  removeTaskCallback("mini")
  swirl_out("Leaving swirl now. Type swirl() to resume.", skip_after=TRUE)
  invisible()
}

#' Begin the upcoming question or unit of instruction.
#' 
#' This is the way to regain swirl's attention after viewing
#' a video or \code{play()}'ing around in the console. 
#' @export
#' @examples
#' \dontrun{
#' 
#' | Create a new variable called `y` that contains the number 8.
#' 
#' > play()
#' 
#' | Entering play mode. Experiment as you please, then type nxt()
#' | when you ready to resume the lesson.
#' 
#' > 10/14
#' > [1] 0.7142857
#' > zz <- 99
#' > zz
#' > [1] 99
#' > nxt()
#' 
#' | Resuming lesson...
#' }
nxt <- function(){invisible()}

#' Skip the current unit of instruction.
#' 
#' swirl will enter the correct answer and notify the
#' user of the names of any new variables which it may have created
#' in doing so. These may be needed for subsequent questions.
#' @export
#' @examples
#' \dontrun{
#' 
#' | Create a new variable called `y` that contains the number 8.
#' 
#' > skip()
#' 
#' | I've entered the correct answer for you.
#' 
#' | In doing so, I've created the variable(s) y, which you may need later.
#' }
skip <- function(){invisible()}

#' Tell swirl to ignore console input for a while.
#' 
#' It is sometimes useful to play around in the R console out of
#' curiosity or to solidify a concept. This command will cause
#' swirl to remain idle, allowing the user to experiment at will,
#' until the command \code{nxt()} is entered. 
#' @export
#' @examples
#' \dontrun{
#' 
#' | Create a new variable called `y` that contains the number 8.
#' 
#' > play()
#' 
#' | Entering play mode. Experiment as you please, then type nxt()
#' | when you ready to resume the lesson.
#' 
#' > 10/14
#' > [1] 0.7142857
#' > zz <- 99
#' > zz
#' > [1] 99
#' > nxt()
#' 
#' | Resuming lesson...
#' }
play <- function(){invisible()}

#' Display a list of special commands.
#' 
#' Display a list of the special commands, \code{bye()}, \code{play()}, 
#' \code{nxt()}, \code{skip()}, and \code{info()}.
#' @export
#' @examples
#' \dontrun{
#' 
#' | Create a new variable called `z` that contains the number 11.
#' 
#' > info()
#' 
#' | When you are at the R prompt (>):
#' | -- Typing skip() allows you to skip the current question.
#' | -- Typing play() lets you experiment with R on your own; swirl will ignore what
#' | you do...
#' | -- UNTIL you type nxt() which will regain swirl's attention.
#' | -- Typing bye() causes swirl to exit. Your progress will be saved.
#' | -- Typing info() displays these options again.
#' 
#' > bye()
#' 
#' | Leaving swirl now. Type swirl() to resume.
#' }
info <- function(){
  swirl_out("When you are at the R prompt (>):")
  
  swirl_out("-- Typing skip() allows you to skip the current question.", skip_before=FALSE)
  
  swirl_out("-- Typing play() lets you experiment with R on your own; swirl will ignore what you do...", skip_before=FALSE)
  swirl_out("-- UNTIL you type nxt() which will regain swirl's attention.", skip_before=FALSE)
  
  swirl_out("-- Typing bye() causes swirl to exit. Your progress will be saved.", skip_before=FALSE)
  
  swirl_out("-- Typing info() displays these options again.", skip_before=FALSE, skip_after=TRUE)  

  
  invisible()
}

## RESUME METHOD

resume <- function(...)UseMethod("resume")

# Default method resume implements a finite state (or virtual) machine. 
# It runs a fixed "program" consisting of three "instructions" which in 
# turn present information, capture a user's response, and test and retry 
# if necessary. The three instructions are themselves S3 methods which 
# depend on the class of the active row of the course lesson. The 
# instruction set is thus extensible. It can be found in R/instructionSet.R. 
# 
resume.default <- function(e){
  esc_flag <- TRUE
  on.exit(if(esc_flag)swirl_out("Leaving swirl now. Type swirl() to resume.", skip_after=TRUE))
  # Trap special functions
  if(uses_func("info")(e$expr)[[1]]){
    esc_flag <- FALSE
    return(TRUE)
  }
  if(uses_func("nxt")(e$expr)[[1]]){    
    ## Using the stored list of "official" swirl variables and values,
    #  assign variables of the same names in the global environment
    #  their "official" values, in case the user has changed them
    #  while playing.
    xfer(as.environment(e$official), globalenv())
    swirl_out("Resuming lesson...")
    e$playing <- FALSE
    e$iptr <- 1
  }
  if(uses_func("play")(e$expr)[[1]]){
    swirl_out("Entering play mode. Experiment as you please, then type nxt() when you are ready to resume the lesson.", skip_after=TRUE)
    e$playing <- TRUE
  }
  # If the user is playing, ignore console input,
  # but remain in operation.
  if(exists("playing", envir=e, inherits=FALSE) && e$playing){
    esc_flag <- FALSE
    return(TRUE)
  }
  # If the user wants to skip the current question, do the bookkeeping.
  if(uses_func("skip")(e$expr)[[1]]){
    # Increment a skip count kept in e.
    if(!exists("skips", e))e$skips <- 0
    e$skips <- 1 + e$skips
    # Enter the correct answer for the user
    # by simulating what the user should have done
    #
    correctAns <- e$current.row[,"CorrectAnswer"]
    # In case correctAns refers to newVar, add it
    # to the snapshot AND the global environment
    if(exists("newVarName",e)){
      correctAns <- gsub("newVar", e$newVarName, correctAns)
    }
    e$expr <- parse(text=correctAns)[[1]]
    ce <- cleanEnv(e$snapshot)
    e$val <- eval(e$expr, ce)
    xfer(ce, globalenv())
    ce <- as.list(ce)
    # Inform the user, but don't expose the actual answer.    
    swirl_out("I've entered the correct answer for you.")
    if(length(names(ce)) > 0){
      swirl_out(paste0("In doing so, I've created the variable(s) ", 
                       names(ce), ", which you may need later."))
    }  
  }
  # Method menu initializes or reinitializes e if necessary.
  temp <- mainMenu(e)
  # If menu returns FALSE, the user wants to exit.
  if(is.logical(temp) && !isTRUE(temp)){
    swirl_out("Leaving swirl now...")
    esc_flag <- FALSE # To supress double notification
    return(FALSE)
  }
  
  # if e$expr is NOT swirl() or nxt(), the user has just responded to
  # a question at the command line. Simulate evaluation of the
  # user's expression and save any variables changed or created
  # in e$delta. 
  # TODO: Eventually make auto-detection of new variables an option.
  # AUTO_DETECT_NEWVAR is currently hardcoded TRUE. (See utilities.R.)
  if(!uses_func("swirl")(e$expr)[[1]] && 
       !uses_func("nxt")(e$expr)[[1]] &&
       AUTO_DETECT_NEWVAR){
    e$delta <- safeEval(e$expr, e)
  }
  
  # Execute instructions until a return to the prompt is necessary
  while(!e$prompt){
    # If the lesson is complete, save progress, remove the current
    # lesson from e, and invoke the top level menu method.
    if(e$row > nrow(e$les)){
      saveProgress(e)
      # form a new path for the progress file
      # which indicates completion and doesn't
      # fit the regex pattern "[.]rda$" i.e.
      # doesn't end in .rda, hence won't be
      # recognized as an active progress file.
      new_path <- paste(e$progress,".done", sep="")
      # rename the progress file to indicate completion
      if(file.exists(new_path))file.remove(new_path)
      file.rename(e$progress, new_path)
      # remove the current lesson and any custom tests
      rm("les", envir=e)
      clearCustomTests()
      # Let user know lesson is complete
      swirl_out("You've reached the end of this lesson! Returning to the main menu...")
      # let the user select another course lesson
      temp <- mainMenu(e)
      # if menu returns FALSE, user wants to quit.
      if(is.logical(temp) && !isTRUE(temp)){
        swirl_out("Leaving swirl now...")
        esc_flag <- FALSE # to supress double notification
        return(FALSE)
    }
    }
    # If we are ready for a new row, prepare it
    if(e$iptr == 1){
      
      #  Any variables changed or created during the previous
      #  question must have been correct or we would not be about
      #  to advance to a new row. Incorporate these in the list
      #  of swirl's "official" names and values.
      #  It should be safe to remove the current snapshot here.
      if (!is.null(e$delta)){
        e$official <- mergeLists(e$delta,e$official)
      }
      e$delta <- list()
      saveProgress(e)
      e$current.row <- e$les[e$row,]
      # Prepend the row's swirl class to its class attribute
      class(e$current.row) <- c(e$current.row[,"Class"], 
                                       class(e$current.row))
    }
    # Execute the current instruction
    e$instr[[e$iptr]](e$current.row, e)
  }
  
  # Take a snapshot of the global environment here for
  #  comparison after the user has responded to a question at
  #  the command line. Store it in e.
  e$snapshot <- as.list(globalenv())
  e$prompt <- FALSE
  esc_flag <- FALSE
  return(TRUE)
}
