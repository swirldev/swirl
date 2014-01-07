## Method declarations

mainMenu <- function(e, ...)UseMethod("mainMenu")
welcome <- function(e, ...)UseMethod("welcome")
housekeeping <- function(e, ...)UseMethod("housekeeping")
inProgressMenu <- function(e, choices, ...)UseMethod("inProgressMenu")
courseMenu <- function(e, courses)UseMethod("courseMenu")
courseDir <- function(e)UseMethod("courseDir")
moduleMenu <- function(e, choices)UseMethod("moduleMenu")
restoreUserProgress <- function(e, selection)UseMethod("restoreUserProgress")
loadModule <- function(e, ...)UseMethod("loadModule")
loadInstructions <- function(e, ...)UseMethod("loadInstructions")

# Default course and module navigation logic
# 
# This method implements default course and module navigation logic, 
# decoupling menu presentation from internal processing of user
# selections. It relies on several methods for menu presentation,
# namely welcome(e), housekeeping(e), inProgressMenu(e, modules),
# courseMenu(e, courses), and moduleMenu(e, modules). Defaults 
# are provided.
# 
# @param e persistent environment accessible to the callback
mainMenu.default <- function(e){
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
    response <- ""
    if(length(pfiles) > 0){
      response <- inProgressMenu(e, pfiles)
    }
    if(response != "" ){
      # If the user has chosen to continue, restore progress
      response <- gsub(" ", "_", response)
      response <- paste0(response,"_.rda")
      restoreUserProgress(e, response)
    } else {
      # Else load a new module.
      # Let user choose the course.
      coursesU <- dir(courseDir(e))
      # Eliminate empty directories
      idx <- sapply(coursesU, 
                    function(x)length(dir(file.path(courseDir(e),x)))>0)
      coursesU <- coursesU[idx]
      # If no courses are available, exit
      if(length(coursesU)==0){
        swirl_out("No courses are available. Try again using swirl() with no parameter.")
        return(FALSE)
      }
      # path cosmetics
      coursesR <- gsub("_", " ", coursesU)
      module <- ""
      while(module == ""){
        course <- courseMenu(e, coursesR)
        if(course=="")return(FALSE)
        # reverse path cosmetics
        courseU <- coursesU[course == coursesR]
        modules <- dir(file.path(courseDir(e), courseU), pattern="module")
        # Let user choose the module.
        module <- moduleMenu(e, modules)
      }
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
      loadInstructions(e)
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
      # indicator that swirl is not reacting to console input
      e$playing <- FALSE
      # create the file
      saveRDS(e, e$progress)
    }
  }
  return(TRUE)
}

# Development version.
welcome.dev <- function(e, ...){
  "swirladmin"
}

# Default version.
welcome.default <- function(e, ...){
  swirl_out()
  swirl_out("Welcome! My name is Swirl and I'll be your host today! Please sign in. If you've been here before please use the same name as you did then. If you are new, call yourself something unique.")
  swirl_out()
  return(readline("What shall I call you? "))
}

# Presents preliminary information to a new user
# 
# @param e persistent environment used here only for its class attribute
# 
housekeeping.default <- function(e){
  swirl_out()
  swirl_out(paste0("Thanks, ", e$usr,". Let's cover a couple of quick housekeeping items before we begin our first lesson. First off, you should know that when you see '...', that means you should press Enter when you are done reading and ready to continue."))
  readline("\n...  <-- That's your cue to press Enter to continue")
  swirl_out()
  swirl_out("Also, as you've probably figured out, when you see 'ANSWER:', or when you see the R prompt (>), or when you are asked to select from a list, that means it's your turn to enter a response, then press Enter to continue.")
  swirl_out()
  select.list(c("Continue.", "Proceed.", "Let's get going!"),
              title="Select 1, 2, or 3 and press Enter", graphics=FALSE)
  swirl_out()
  swirl_out("You can return to the R console (>) at any time by pressing the Esc key; this doesn't exit so swirl remains in operation.")
  info()
  swirl_out("Let's get started!")
  readline("\n...")
}

# Development version; does nothing
housekeeping.dev <- function(e){}

# A stub. Eventually this should be a full menu
inProgressMenu.default <- function(e, choices){
  nada <- "No. Let me start something new."
  swirl_out("Would you like to continue with one of these modules?")
  selection <- select.list(c(choices, nada), graphics=FALSE)
  # return a blank if the user rejects all choices
  if(identical(selection, nada))selection <- ""
  return(selection)
}

# A stub. Eventually this should be a full menu
courseMenu.default <- function(e, choices){
  swirl_out("Please choose a course, or type 0 to exit swirl.")
  return(select.list(choices, graphics=FALSE))
}

# A stub. Eventually this should be a full menu
moduleMenu.default <- function(e, choices){
  swirl_out("Please choose a module, or type 0 to return to course menu.")
  return(select.list(choices, graphics=FALSE))
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
  instructor <- courseU # default
  instructorFile <- file.path(modPath,"instructor.txt")
  if(file.exists(instructorFile)){
    instructor <- readLines(instructorFile)[1]
  }
  # Return the course module, using Nick's constructor which 
  # adds attributes identifying the course and indicating dependencies.
  return(module(read.csv(dataName, as.is=TRUE),module, courseU, instructor))
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

loadInstructions.default <- function(e){
  e$instr <- list(present, waitUser, testResponse)
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
  file.path(find.package("swirlfancy"), "Courses")
}

courseDir.dev <- function(e){
  # e's only role is to determine the method used
  file.path("inst", "Courses")
}


# Provided the path to a directory, chooseFile() presents a "pretty" list of all
# files and directories in that directory. The user selects their desired
# file or directory and the function returns the full path to it.

chooseFile <- function(path2Dir) {
  allFiles <- list.files(path2Dir)
  cleanNames <- sub(".csv", "", gsub("_", " ", allFiles))
  
  chosenFile <- select.list(cleanNames, graphics=FALSE)
  path2Choice <- file.path(path2Dir, allFiles[which(cleanNames==chosenFile)])
}

# Gets desired course and module from user, then returns full path to module.
getModPath <- function() {
  swirl_out("Please select a course: ")
  courseDir <- chooseFile(file.path("inst", "Courses"))
  
  swirl_out("Please select a module: ")
  modulePath <- chooseFile(courseDir)
}

# Default for determining the user
getUser <- function()UseMethod("getUser")
getUser.default <- function()"swirladmin"