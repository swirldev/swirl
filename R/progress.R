saveProgress <- function(e)UseMethod("saveProgress")

saveProgress.default <- function(e){
  # save progress
  suppressMessages(suppressWarnings(saveRDS(e, e$progress)))
}

# Provide a user name
deleteProgress <- function(user){
  # Find path to user data
  path <- system.file("user_data", user, package = "swirl")
  
  # Delete all files within a user folder
  if(file.exists(path)){
    invisible(file.remove(list.files(path, full.names = TRUE), recursive = TRUE))
  }
}