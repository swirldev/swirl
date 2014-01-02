initSwirl <- function(e)UseMethod("initSwirl")

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
    # source the initModule.R file if it exists (fixes swirlfancy #28)
    initf <- file.path(e$path, "initModule.R")
    if(file.exists(initf))source(initf)
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
    fname <- paste(attr(e$mod,"courseName"), attr(e$mod,"modName"), ".rda", sep="_")
    # path to file 
    e$progress <- file.path(udat, fname)
    # list to hold expressions entered by the user
    e$usrexpr <- list()
    # create the file
    saveRDS(e, e$progress)
  }
}

loadInstructions <- function(e, ...)UseMethod("loadInstructions")

loadInstructions.default <- function(e){
  e$instr <- list(present, waitUser, testResponse)
}