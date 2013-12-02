#' Provided the path to a directory, chooseFile() presents a "pretty" list of all
#' files and directories in that directory. The user selects their desired
#' file or directory and the function returns the full path to it.

chooseFile <- function(path2Dir) {
  allFiles <- list.files(path2Dir)
  cleanNames <- sub(".csv", "", gsub("_", " ", allFiles))
  
  chosenFile <- select.list(cleanNames, graphics=FALSE)
  path2Choice <- file.path(path2Dir, allFiles[which(cleanNames==chosenFile)])
}

#' Gets desired course and module from user, then returns full path to module.
getModPath <- function() {
  swirl_out("Please select a course: ")
  courseDir <- chooseFile(file.path("data", "Courses"))
  
  swirl_out("Please select a module: ")
  modulePath <- chooseFile(courseDir)
}