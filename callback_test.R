library(stringr)

out <- function(...) {
  # Format the argument for pretty display.
  # getOption("width") gives screen width in characters.
  wrapped <- strwrap(str_c(..., sep = " "), 
                     width = getOption("width") - 2)
  # Start each line with "| " (str_c is in package stringr)
  # and display the result.
  message(str_c("| ", wrapped, collapse = "\n"))
}

# Prints (with explanation) the 4 callback function arguments
printCallbackArgs <- function(...) {
  message(rep("=", getOption("width")))
  message("CALLBACK IS ACTIVE")
  message(rep("=", getOption("width")))
  message("Expression for the top-level task (as list):")
  print(as.list(..1))
  message("Result of the top-level task (as list):")
  print(as.list(..2))
  message(paste("Completed successfully?", ..3))
  message(paste("Result printed?", ..4))
  message(rep("=", getOption("width")))
}

# Prints goodbye message and removes active callbacks
bye <- function() {
  out("Thanks for stopping by!")
  rmcb()
}

# Removes active callbacks
rmcb <- function() {
  removeTaskCallback(which(getTaskCallbackNames() == "test"))
  invisible()
}

# Creates a new callback that invokes primary callback function
runTest <- function() {
  addTaskCallback(cbfun, name="test")
  invisible()
}

# Primary callback function (callback quarterback). Invoked each time a top-level task (command line entry) is successfully completed. 
cbfun <- function(...) {
  # Print callback arguments
  printCallbackArgs(...)
  
  ### DO SOME OTHER STUFF HERE
  
  # Then return TRUE to keep callback alive
  TRUE
}