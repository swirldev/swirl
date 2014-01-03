#' Resume method which accommodates customized menus. To test, use swirl("dev").
#' REMARKS:
#' -allows continuation after completion of a module,
#' -should replace resume.default soon,
#' -decouples presentation of menus from internal technicalities,
#' -accommodates different course locations and formats, currently just csv
#' files in the project directory.

## Method declarations

menu <- function(e, ...)UseMethod("menu")
welcome <- function(e, ...)UseMethod("welcome")
housekeeping <- function(e, ...)UseMethod("housekeeping")
inProgressMenu <- function(e, choices, ...)UseMethod("inProgressMenu")
courseMenu <- function(e, courses)UseMethod("courseMenu")
courseDir <- function(e)UseMethod("courseDir")
moduleMenu <- function(e, choices)UseMethod("moduleMenu")
restoreUserProgress <- function(e, selection)UseMethod("restoreUserProgress")
loadModule <- function(e, ...)UseMethod("loadModule")


resume.default <- function(e){
  # We may be entering for the first time, in which case our environment
  # will not be fully initialized. Method menu checks for this
  menu(e)
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
      menu(e)
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

#' Default course and module navigation logic
#' 
#' This method implements default course and module navigation logic, 
#' decoupling menu presentation from internal processing of user
#' selections. It relies on several methods for menu presentation,
#' namely welcome(e), newUserBlurb(e), inProgressMenu(e, modules),
#' courseMenu(e, courses), and moduleMenu(e, modules). Defaults are provided.
#' 
#' @param e persistent environment accessible to the callback
menu.default <- function(e){
  # Welcome the user if necessary and set up progress tracking
  if(!exists("usr",e,inherits = FALSE)){
    e$usr <- welcome(e)
    udat <- file.path(find.package("swirlfancy"), "user_data", e$usr)
    if(!file.exists(udat)){
      housekeeping(e)
      dir.create(udat, recursive=TRUE)
    }
    e$udat <- udat
  }
  # If there is no active module, obtain one.
  if(!exists("mod",e,inherits = FALSE)){
    # First, allow user to continue unfinished modules
    # if there are any
    pfiles <- inProgress(e)
    response <- character()
    if(length(pfiles) > 0){
      response <- inProgressMenu(e, pfiles)
    }
    if(length(response) > 0 ){
      # If the user has chosen to continue, restore progress
      response <- gsub(" ", "_", response)
      response <- paste0(response,"_.rda")
      restoreUserProgress(e, response)
    } else {
      # Else load a new module.
      # Let user choose the course.
      coursesU <- dir(courseDir(e))
      # path cosmetics
      coursesR <- gsub("_", " ", coursesU)
      course <- courseMenu(e, coursesR)
      # reverse path cosmetics
      courseU <- coursesU[course == coursesR]
      modules <- dir(file.path(courseDir(e), courseU))
      # Let user choose the module.
      module <- moduleMenu(e, modules)
      # Load the module and intialize everything
      e$mod <- loadModule(e, courseU, module)
      # expr, val, ok, and vis should have been set by the callback.
      # The module's current row
      e$row <- 1
      # The current row's instruction pointer
      e$iptr <- 1
      # A flag indicating we should return to the prompt
      e$prompt <- FALSE
      # The job of loading instructions for this "virtual machine"
      # is relegated to an S3 method to allow for different "programs."
      e$instr <- list(present, waitUser, testResponse.default)
      # An identifier for the active row
      e$current.row <- NULL
      # For sourcing files which construct figures etc
      e$path <- file.path(courseDir(e), courseU, module)
      # Set up paths and files to save user progress
      # Make file path from module info
      fname <- progressName(attr(e$mod,"courseName"), attr(e$mod,"modName"))
      # path to file 
      e$progress <- file.path(e$udat, fname)
      # list to hold expressions entered by the user
      e$usrexpr <- list()
      # create the file
      saveRDS(e, e$progress)
    }
  }
}

#' Development version.
welcome.dev <- function(e, ...){
  "swirladmin"
}

#' Default version.
welcome.default <- function(e, ...){
  swirl_out("Welcome! My name is Swirl and I'll be your host today! Please sign in. If you've been here before please use the same name as you did then. If you are new, call yourself something unique.")
  return(readline("What shall I call you? "))
}

#' Presents preliminary information to a new user
#' 
#' @param e persistent environment used here only for its class attribute
#' 
housekeeping.default <- function(e){
  swirl_out("Let's cover a couple of quick housekeeping items before we begin our first lesson. \n\nFirst off, you should know that when you see '...', that means you should press Enter when you are done reading and ready to continue. Also, as you've probably figured out, when you see 'ANSWER:', that means it's your turn to enter a response, then press Enter to continue.\n\nRemember you can stop at any time by pressing the Esc key and typing bye(). Your progress will be saved. Let's get started!")
  readline("\n...  <-- That's your cue to press Enter to continue")
}

#' Development version; does nothing
housekeeping.dev <- function(e){}

#' A stub. Eventually this should be a full menu
inProgressMenu.default <- function(e, choices){
  nada <- "No. Let me start something new."
  swirl_out("Would you like to continue with one of these modules?")
  selection <- select.list(c(choices, nada))
  # return an empty character array if the user rejects all choices
  if(identical(selection, nada))selection <- character()
  return(selection)
}

#' A stub. Eventually this should be a full menu
courseMenu.default <- function(e, choices){
  swirl_out("Please choose a course.")
  return(select.list(choices))
}

#' A stub. Eventually this should be a full menu
moduleMenu.default <- function(e, choices){
  swirl_out("Please choose a module.")
  return(select.list(choices))
}

loadModule.default <- function(e, courseU, module){
  # Load the content file
  modPath <- file.path(courseDir(e), courseU, module)
  len <- str_length(module)
  shortname <- paste0(substr(module,1,3),substr(module,len,len),"_new.csv",collapse=NULL)
  dataName <- file.path(modPath,shortname)     
  #initialize course module, assigning module-specific variables
  initFile <- file.path(modPath,"initModule.R")
  if (file.exists(initFile)){
    source(initFile)
  }
  # Return the course module, using Nick's constructor which 
  # adds attributes identifying the course and indicating dependencies.
  return(module(read.csv(dataName, as.is=TRUE),module, courseU, "Nick"))
}

restoreUserProgress.default <- function(e, selection){
  # read the progress file
  temp <- readRDS(file.path(e$udat, selection))
  # transfer its contents to e
  xfer(temp, e)
  # source the initModule.R file if it exists (fixes swirlfancy #28)
  initf <- file.path(e$path, "initModule.R")
  if(file.exists(initf))source(initf)
  # eval retrieved user expr's in global env, but don't include
  # call to swirl (the first entry)
  if(length(e$usrexpr) > 1){
    for(n in 2:length(e$usrexpr)){
      expr <- e$usrexpr[[n]]
      eval(expr, globalenv())
    }
  }
}


# UTILITIES

progressName <- function(courseName, modName){
  paste(courseName, modName, ".rda", sep="_")
}

inProgress <- function(e){
  pfiles <- dir(e$udat)[grep("[.]rda$", dir(e$udat))]
  pfiles <- gsub("[.]rda", "", pfiles)
  pfiles <- str_trim(gsub("_", " ", pfiles))
  return(pfiles)
}

completed <- function(e){
  pfiles <- dir(e$udat)[grep("[.]done$", dir(e$udat))]
  pfiles <- gsub("[.]done", "", pfiles)
  pfiles <- gsub("[.]rda", "", pfiles)
  pfiles <- str_trim(gsub("_", " ", pfiles))
  return(pfiles)
}

courseDir.default <- function(e){
  # e's only role is to determine the method used
  file.path("inst", "Courses")
}
