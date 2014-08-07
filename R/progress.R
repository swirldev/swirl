saveProgress <- function(e)UseMethod("saveProgress")

saveProgress.default <- function(e){
  # save progress
  suppressMessages(suppressWarnings(saveRDS(e, e$progress)))
}
