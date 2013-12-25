saveProgress <- function(e)UseMethod("saveProgress")

saveProgress.default <- function(e){
  n <- length(e$usrexpr)
  expr <- e$expr
  if(n==0 || !identical(e$usrexpr[[n]], expr)){
    e$usrexpr <- c(e$usrexpr, expr)
  }
  # save progress
  saveRDS(e, e$progress)
  
}