source("R/testMod.R")

initSwirl.userProgress <- function(e){
  # Sign up?
  #   => capture username
  #   create directory
  # Else sign in
  # e$usr <- username
  # find stored progress
  # if previous, continue previous?
  #   => restore progress including relevant parts of e
  # else choose new module
  #   => partially populate e
  initSwirl.default(e) # temporary
  #      Create a list for expressions entered by user
  e$usrexpr <- list()
}

#' This function is invoked prior to processing
#' a new row, which means the last row was
#' successfully completed. Thus the current
#' value of e$expr should be saved unless seen
#' the last time.
saveProgress.userProgress <-  function(e){
  if(!identical(e$usrexpr[[1]], e$expr)){
    e$usrexpr <- c(e$expr, e$usrexpr)
  }
  # save e to disk
}