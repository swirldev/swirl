source("R/miniMulti.R")
source("R/modConstructor.R")
source("R/testModInstr.R")

resume.testMod <- function(e){
  # This function is entered ONLY when the user has entered a
  # valid expression at the R prompt.
  #
  # We may be entering for the first time, in which case our environment
  # will not be fully initialized. We check for that and initialize if
  # necessary.
  fromhi <- (!exists("mod",e))
  if(fromhi){
    # Load the course module, using Nick's constructor which 
    # adds attributes identifying the course and indicating dependencies.
    e$mod <- module(read.csv("data/testMod4Daphne.csv", as.is=TRUE),"4Daphne", "test", "Nick")
    # expr, val, ok, and vis should have been set by the callback.
    # The module's current row
    e$row <- 1
    # The current row's instruction pointer
    e$iptr <- 1
    # A flag indicating we should return to the prompt
    e$prompt <- FALSE
    # A fixed list of instructions for this "virtual machine"
    e$instr <- list(present, waitUser, testResponse.default)
    # An identifier for the active row
    e$current.row <- NULL
  }
  # Execute instructions until a return to the prompt is necessary
  while(!e$prompt){
    # If the module is complete, return FALSE to remove callback
    if(e$row > nrow(e$mod))return(FALSE)
    # If we are ready for a new row, prepare it
    if(e$iptr == 1){
      e$current.row <- e$mod[e$row,]
      # Prepend the row's swirl class to its class attribute
      attr(e$current.row,"class") <- c(classifyRow(e$current.row), 
                                       attr(e$current.row,"class"))
    }
    # Execute the current instruction
    e$instr[[e$iptr]](e$current.row, e)
  }
  e$prompt <- FALSE
  return(TRUE)
}

# Determines the class of a row
classifyRow <- function(current.row){
  if(current.row[,"OutputType"] == "text")return("text")
  if(current.row[,"OutputType"] == "video")return("video")
  if(current.row[,"AnswerType"] == "multiple")return("mult_question")
  return("cmd_question")
  
}