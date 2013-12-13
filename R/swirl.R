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
  addTaskCallback(cb, name="mini")
  invisible()
}

bye <- function(){
  removeTaskCallback("mini")
  invisible()
}

resume <- function(...)UseMethod("resume")


#' Method resume.default implements a finite state (or virtual) machine. 
#' It runs a fixed "program" consisting of three "instructions" which in 
#' turn present information, capture a user's response, and test and retry 
#' if necessary. The three instructions are themselves S3 methods which 
#' depend on the class of the active row of the course module. The 
#' instruction set is thus extensible. It can be found in R/instructionSet.R. 
#' 
resume.default <- function(e){
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


initSwirl <- function(e)UseMethod("initSwirl")
saveProgress <- function(e)UseMethod("saveProgress")

initSwirl.default <- function(e){
  
  e$usr <- getUser()
  udat <- file.path(find.package("swirlfancy"), "user_data", e$usr)
  if(!file.exists(udat))dir.create(udat, recursive=TRUE)
  # Check for the existence of progress files
  pfiles <- dir(udat)[grep("[.]rda$", dir(udat))]
  # Would the user care to continue with any of these?
  selection <- "No thanks"
  if(length(pfiles) > 0){
    swirl_out("Would you like to continue with one of these modules?")
    selection <- select.list(c(pfiles, selection))
  }
  if(selection != "No thanks"){
    # continue with a previous module
    # restore progress from selected file
    temp <- readRDS(file.path(udat, selection))
    xfer(temp, e)
    # eval retrieved user expr's in global env, but don't include hi
    if(length(e$usrexpr) > 1){
      for(n in 2:length(e$usrexpr)){
        expr <- e$usrexpr[[n]]
        eval(expr, globalenv())
      }
    }
  } else {

    # begin a new module
    #todo this code will change when rda files become available
    modPath <- getModPath()
    base <- basename(modPath)
    len <- str_length(base)
    courseName <- basename(dirname(modPath))
    shortname <- paste0(substr(base,1,3),substr(base,len,len),"_new.csv",collapse=NULL)
    dataName <- paste(modPath,shortname,sep="/")
    
    #initialize course module, assigning module-specific variables
    initFile <- paste(modPath,"initModule.R",sep="/")
    if (file.exists(initFile)){
      source(initFile)
    }
    # Load the course module, using Nick's constructor which 
    # adds attributes identifying the course and indicating dependencies.
    e$mod <- module(read.csv(dataName, as.is=TRUE),base, courseName, "Nick")
    # expr, val, ok, and vis should have been set by the callback.
    # The module's current row
    e$row <- 1
    # The current row's instruction pointer
    e$iptr <- 1
    # A flag indicating we should return to the prompt
    e$prompt <- FALSE
    # A fixed list of instructions for this "virtual machine"
    e$instr <- list(present, waitUser, testResponse.default)
    # An identifier for the active row
    e$current.row <- NULL
    e$path <- modPath
    # the following is from userProgress.R
    # make file path from module info
    fname <- paste(attr(e$mod,"courseName"), attr(e$mod,"modName"), ".rda", sep="")
    # path to file 
    e$progress <- file.path(udat, fname)
    # list to hold expressions entered by the user
    e$usrexpr <- list()
    # create the file
    saveRDS(e, e$progress)
  }
}

saveProgress.default <- function(e){
  n <- length(e$usrexpr)
  expr <- e$expr
  if(n==0 || !identical(e$usrexpr[[n]], expr)){
    e$usrexpr <- c(e$usrexpr, expr)
  }
  # save progress
  saveRDS(e, e$progress)
  
 } 

#' Default for determining the user
getUser <- function()UseMethod("getUser")
getUser.default <- function()"swirladmin"

#' UTILITIES

swirl_out <- function(...) {
  wrapped <- strwrap(str_c(..., sep = " "),
                     width = getOption("width") - 2)
  message(str_c("| ", wrapped, collapse = "\n"))
}
xfer <- function(env1, env2){
  lapply(ls(env1), function(var)getAssign(var, env1, env2))
}

getAssign <- function(var, env1, env2){
  assign(var, get(var, env1, inherits=FALSE), envir=env2)
}

cleanAdmin <- function(){
  udat <- file.path(find.package("swirl"), "user_data", "swirladmin")
  file.remove(dir(udat, pattern="*[.]rda", full.names=TRUE))
  invisible()
}