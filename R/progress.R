saveProgress <- function(e)UseMethod("saveProgress")

saveProgress.default <- function(e){
  # save progress
  suppressMessages(suppressWarnings(saveRDS(e, e$progress)))
}

#' Delete a user's progress
#' 
#' @param user The user name whose progress will be deleted.
#' @param path If specified, the directory where the user_data can be found
#' @export
#' @examples
#' \dontrun{
#' 
#' delete_progress("bill")
#' }
delete_progress <- function(user, path = NULL){
  # Make sure user entered a user name
  if(nchar(user) < 1){
    stop("Please enter a valid username.")
  }

  # Find path to user data
  if(is.null(path)) {
    path <- system.file("user_data", user, package = "swirl")  
  }
  
  # Delete all files within a user folder
  if(file.exists(path)){
    invisible(file.remove(list.files(path, full.names = TRUE), recursive = TRUE))
    message(paste0(s()%N%"Deleted progress for user: ", user))
  } else {
    message(paste0(s()%N%"Could not find account for user: ", user))
  }
}