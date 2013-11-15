#' Take advantage of the optional data argument that gets passed to the callbalk
#' function. The data argument can be a list beginning with the username, course, and module,
#' then containing any other information we wish to save (e.g. user defined variables, user's
#' current progress, etc.)

saveSession <- function(expr, val, ok, vis, data) {
  # Set up directory in a native platform-compliant manner
  fileDir <- file.path(path.package("swirl"), "userData")
  # Assuming first three elements of data are username, course, and module...
  fileName <- paste0(paste(data[[1]], data[[2]], data[[3]], sep="_"), ".Rdata")
  # Specify final file path
  filePath <- file.path(fileDir, fileName)
  
  # Save user data as .Rdata file
  save(data, file=filePath)
}

######## TEST ########

# Load swirl library so it can find path.package("swirl")
library(swirl)

# Define data variable, which will contain all important session data
data <- list("ncarchedi", "data_analysis", "module2", 
             list(a="current state", b="user-defined variables", c="other useful info"))

#' Initiate callback that will cause session data (contained entirely in data 
#' argument) to be saved automatically following every top-level task.
addTaskCallback(saveSession, data=data, name="autoSave")