#' Creates an environment, e, defines a function, cb, and registers
#' cb as a callback with data argument, e. The callback retains a
#' reference to the environment in which it was created, environment(cb),
#' hence that environment, which also contains e, persists as long
#' as cb remains registered. Thus e can be used to store infomation
#' between invocations of cb. 
swirl <- function(resume.class="default"){
  bye()
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

bye <- function(){
  removeTaskCallback("mini")
  invisible()
}

nxt <- function(){invisible()}

skip <- function(){invisible()}

play <- function(){invisible()}

## RESUME

resume <- function(...)UseMethod("resume")

#' Default method resume implements a finite state (or virtual) machine. 
#' It runs a fixed "program" consisting of three "instructions" which in 
#' turn present information, capture a user's response, and test and retry 
#' if necessary. The three instructions are themselves S3 methods which 
#' depend on the class of the active row of the course module. The 
#' instruction set is thus extensible. It can be found in R/instructionSet.R. 
#' 
resume.default <- function(e){
  # Trap special functions
  if(uses_func("nxt")(e$expr)[[1]]){
    e$playing <- FALSE
    e$iptr <- 1
  }
  if(uses_func("skip")(e$expr)[[1]]){
    # Stub
    swirl_out("skip() does nothing just yet.")
    return(TRUE)
  }
  if(uses_func("play")(e$expr)[[1]]){
    e$playing <- TRUE
  }
  # If the user is playing, ignore console input,
  # but remain in operation.
  if(exists("playing", envir=e, inherits=FALSE) && e$playing)return(TRUE)
  # Method menu initializes or reinitializes e if necessary.
  temp <- menu(e)
  # If menu returns FALSE, the user wants to exit.
  if(is.logical(temp) && !isTRUE(temp))return(FALSE)
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
      temp <- menu(e)
      # if menu returns FALSE, user wants to quit.
      if(is.logical(temp) && !isTRUE(temp))return(FALSE)
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

#' DEPRECATED method resume.depr implements a finite state (or virtual) machine. 
#' It runs a fixed "program" consisting of three "instructions" which in 
#' turn present information, capture a user's response, and test and retry 
#' if necessary. The three instructions are themselves S3 methods which 
#' depend on the class of the active row of the course module. The 
#' instruction set is thus extensible. It can be found in R/instructionSet.R. 
#' 
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