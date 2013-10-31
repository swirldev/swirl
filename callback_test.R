library(stringr)

frndly_out <- function(...) {
  # Format the argument for pretty display.
  # getOption("width") gives screen width in characters.
  wrapped <- strwrap(str_c(..., sep = " "), 
                     width = getOption("width") - 2)
  # Start each line with "| " (str_c is in package stringr)
  # and display the result.
  message(str_c("| ", wrapped, collapse = "\n"))
}

# Function that is to be invoked each time a top-level task (command line entry) is successfully completed. Prints (with explanation) the 4 arguments with which it is called.
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
  
  if (assignTest()) {
    message("Great job!")
    bye()
  } else {
    frndly_out("Assign the value 999 to a variable called 'dog'.")
    return(TRUE)
  }
}

assignTest <- function() {
  exists("dog", globalenv()) && get("dog", globalenv()) == 999
}

bye <- function() {
  frndly_out("Thanks for stopping by!")
  rmcb()
}

# Removes leftover callbacks
rmcb <- function() {
  removeTaskCallback(which(getTaskCallbackNames() == "test"))
}

# Creates a new callback that invokes printCallbackArgs function above
runTest <- function() {
  addTaskCallback(printCallbackArgs, name="test")
  invisible()
}