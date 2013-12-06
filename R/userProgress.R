#' Prototype feature to save and restore user progress.
#' Source and type hi(c("userProgress", "testMod")).
#' End session with <esc>
#' 
#' TODO: somehow mark finished modules by renaming or something

source("R/testMod.R")

initSwirl.userProgress <- function(e){
  e$usr <- "swirladmin"   # stub for sign in or sign up
  # Find or create progress storage location
  udat <- file.path(find.package("swirl"), "user_data", e$usr)
  if(!file.exists(udat))dir.create(udat)
  # Check for the existence of progress files
  pfiles <- dir(udat)[grep("[.]rda", dir(udat))]
  # Would the user care to continue with any of these?
  selection <- "No thanks"
  swirl_out("Would you like to continue with one of these modules?")
  if(length(pfiles) > 0){
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