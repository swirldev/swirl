saveLog <- function(e)UseMethod("saveLog")

saveLog.default <- function(e){
  # save log
  suppressMessages(suppressWarnings(
    saveRDS(e$log, file.path(e$udat, paste0(as.integer(Sys.time()), ".swlog")))))
}