saveProgress <- function(e)UseMethod("saveProgress")

saveProgress.default <- function(e){
  # save progress
  saveRDS(e, e$progress)
}