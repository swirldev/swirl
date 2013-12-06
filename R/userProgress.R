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
  if(length(pfiles) > 0){
    selection <- select.list("Would you like to continue with one of these modules?", c(pfiles, selection))
  }
  if(selection == "No thanks"){
    initSwirl.default(e) 
    e$usrexpr <- list()
  } else {
    # restore selection
  }
}

#' This function is invoked prior to processing
#' a new row, which means the last row was
#' successfully completed. Thus the current
#' value of e$expr should be saved unless seen
#' the last time.
saveProgress.userProgress <-  function(e){
  if(length(e$usrexp)==0 || !identical(e$usrexpr[[1]], e$expr)){
    e$usrexpr <- c(e$expr, e$usrexpr)
  }
  # save e to disk
}