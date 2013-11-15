#' Take advantage of the optional data argument that gets passed to the callbalk
#' function. The data argument can be a list beginning with the username, course, and module,
#' then containing any other information we wish to save (e.g. user defined variables, user's
#' current progress, etc.)

saveSession <- function(..., data) {
  # Set up directory in a native platform-compliant manner
  dir <- file.path(path.package("swirl"), "userData")
  # Assuming first three elements of data are username, course, and module...
  fileName <- paste(data[[1]], data[[2]], data[[3]], sep="_")
  # Specify final file path
  filePath <- file.path(dir, fileName)
  
  # Save user data as .Rdata file
  save(data, file=filePath)
}

#' Initiate callback that will cause session data (contained entirely in data 
#' argument) to be saved automatically following every top-level task.
addTaskCallBack(saveSession, data=data, name="autoSave")