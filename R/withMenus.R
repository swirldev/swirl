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
inProgressMenu <- function(e, choices, ...)UseMethod("inProgressMenu")
courseMenu <- function(e, courses)UseMethod("courseMenu")
courseDir <- function(e)UseMethod("courseDir")
moduleMenu <- function(e, choices)UseMethod("moduleMenu")
restoreUserProgress <- function(e, selection)UseMethod("restoreUserProgress")
loadModule <- function(e, ...)UseMethod("loadModule")

resume.dev <- function(e){
  # We may be entering for the first time, in which case our environment
  # will not be fully initialized. Method menu checks for this
  menu(e)
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

menu.default <- function(e){
  # Welcome the user if necessary and set up progress tracking
  if(!exists("usr",e,inherits = FALSE)){
    e$usr <- welcome(e)
    udat <- file.path(find.package("swirlfancy"), "user_data", e$usr)
    if(!file.exists(udat))dir.create(udat, recursive=TRUE)
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

#' Development version. Default should be a full welcome menu
welcome.dev <- function(e, ...){
  "swirladmin"
}

#' A stub. Eventually this should be a full menu
inProgressMenu.default <- function(e, choices){
  nada <- "No. Let me start something new."
  print("Would you like to continue with one of these modules?")
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
  file.path("data", "Courses")
}
