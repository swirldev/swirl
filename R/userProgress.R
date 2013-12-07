#' Prototype feature to save and restore user progress.
#' Source and type hi("userProgress").
#' End session with <esc>, then bye()

source("R/testMod.R")

#' This overrides and wraps resume.testMod for the simple
#' purpose of detecting when a module is complete and
#' taking appropriate action on the progress file.
resume.userProgress <- function(e){
  rtn <- resume.testMod(e)
  # independently of testMod, check for end of module
  if(e$row > nrow(e$mod)){
    saveProgress.userProgress(e)
    # form a new path for the progress file
    # which indicates completion and doesn't
    # fit the regex pattern "[.]rda$" i.e.
    # doesn't end in .rda, hence won't be
    # recognized as an active progress file.
    new_path <- paste(e$progress,".done", sep="")
    # rename the progress file to indicate completion
    if(!file.exists(new_path))file.rename(e$progress, new_path)
  }
  return(rtn)
}

initSwirl.userProgress <- function(e){
  e$usr <- getUser()
  udat <- file.path(find.package("swirl"), "user_data", e$usr)
  if(!file.exists(udat))dir.create(udat, recursive=TRUE)
  # Check for the existence of progress files
  pfiles <- dir(udat)[grep("[.]rda$", dir(udat))]
  # Would the user care to continue with any of these?
  selection <- "No thanks"
  if(length(pfiles) > 0){
    swirl_out("Would you like to continue with one of these modules?")
    selection <- select.list(c(pfiles, selection))
  }
  if(selection == "No thanks"){
    # init with new module
    initSwirl.default(e)
    # make file path from module info
    fname <- paste(attr(e$mod,"courseName"), attr(e$mod,"modName"), ".rda", sep="")
    # path to file 
    e$progress <- file.path(udat, fname)
    # list to hold expressions entered by the user
    e$usrexpr <- list()
    # create the file
    saveRDS(e, e$progress)
  } else {
    # restore contents of e from selected file
    temp <- readRDS(file.path(udat, selection))
    xfer(temp, e)
    # eval retrieved user expr's in global env, but don't include hi
    if(length(e$usrexpr) > 1){
      for(n in 2:length(e$usrexpr)){
        expr <- e$usrexpr[[n]]
        eval(expr, globalenv())
      }
    }
  }
}

#' This function is invoked prior to processing
#' a new row, which means the last row was
#' successfully completed. Thus the current
#' value of e$expr should be saved unless seen
#' the last time.
saveProgress.userProgress <-  function(e){
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

# utils

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