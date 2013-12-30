#' Resume method which accommodates menus. To test, use swirl("withMenus").
#' REMARKS:
#' -a work in progress
#' -allows continuation after completion of a module unlike resume.default.
#' -should eventually replace resume.default
#' -should eventually decouple presentation of menus from 
#'  internal technicalities

resume.withMenus <- function(e){
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


menu <- function(e, ...)UseMethod("menu")
welcome <- function(e, ...)UseMethod("welcome")
loadModule <- function(e, ...)UseMethod("loadModule")

menu.default <- function(e){
  # Welcome the user if necessary and set up progress tracking
  if(!exists("usr",e,inherits = FALSE)){
    welcome(e)
    udat <- file.path(find.package("swirlfancy"), "user_data", e$usr)
    if(!file.exists(udat))dir.create(udat, recursive=TRUE)
    e$udat <- udat
  }
  if(!exists("mod",e,inherits = FALSE))loadModule(e)
}

#' A stub. Eventually this should be a full welcome menu
welcome.default <- function(e, ...){
  e$usr <- "swirladmin"
}

#' Almost the same as initSwirl.default at the moment,
#' eventually to be broken up into smaller units separating
#' presentation of menus from internal technicalities.
loadModule.default <- function(e, ...){
  # Check for the existence of progress files
  pfiles <- dir(e$udat)[grep("[.]rda$", dir(e$udat))]
  # Would the user care to continue with any of these?
  selection <- "No thanks"
  if(length(pfiles) > 0){
    swirl_out("Would you like to continue with one of these modules?")
    selection <- select.list(c(pfiles, selection))
  }
  if(selection != "No thanks"){
    # continue with a previous module
    # restore progress from selected file
    temp <- readRDS(file.path(e$udat, selection))
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
    # The job of loading instructions for this "virtual machine"
    # is relegated to an S3 method to allow for different "programs."
    e$instr <- list(present, waitUser, testResponse.default)
    # An identifier for the active row
    e$current.row <- NULL
    e$path <- modPath
    # the following is from userProgress.R
    # make file path from module info
    fname <- paste(attr(e$mod,"courseName"), attr(e$mod,"modName"), ".rda", sep="")
    # path to file 
    e$progress <- file.path(e$udat, fname)
    # list to hold expressions entered by the user
    e$usrexpr <- list()
    # create the file
    saveRDS(e, e$progress)
  }
}