# Source: https://gist.github.com/hadley/6734404
# A tutorial navigates through a series of states, either manually, with
# nxt(), prv(), jmp() etc, or automatically, by using addTaskCallback() and
# checking that preconditions are met.
#   
# Each state has:
#  * a message
#  * a next state (a function name)  
#    (eventually may have multiple next named states)
#  * an auto test 
#
# Questions:
#
#  * how could you make it more interactive like a pick-a-path book?
#    * would need to have multiple "next" tests
#  * would it make sense to have a special file format? (json based?)
#  * if running on web, how would you modify stuff outside of the cli?
#  * how can you save state so you can start again later?
#  * states should have setup functions that they can use to ensure that
#    the environment is in the right state
#  * should have the ability to run code and show output
#  * need to be able to load/run "modules"
#  * build simple web interface?
#     * v. similar to command line, but sends json result
#     * two components: script output, new instructions

library(stringr)

# Define frndly to be a new environment (a container for 
# variables and functions.) It is global in scope, and its
# name is hard-coded in the functions below. Its states are
# also global variables with special names such as start_state.
frndly <- new.env(parent = emptyenv())

frndly_out <- function(...) {
  # Format the argument for pretty display.
  # getOption("width") gives screen width in characters.
  wrapped <- strwrap(str_c(..., sep = " "), 
                     width = getOption("width") - 2)
  # Start each line with "| " (str_c is in package stringr)
  # and display the result.
  message(str_c("| ", wrapped, collapse = "\n"))
}

hi <- function() {
  # Register function auto_advance() as that to be called
  # upon completion of any "top level" task, i.e., a command the
  # user enters from the R prompt.
  addTaskCallback(auto_advance, name = "frndly")
  # Set the tutorial state "start" (see jmp below)
  jmp("start")
  # Return NULL but don't print it
  invisible()
}

bye <- function() {
  frndly_out("All done! Use jmp('start') to restart.")
  # Unregister function auto_advance()
  removeTaskCallback(which(getTaskCallbackNames() == "frndly"))
  # Return NULL but don't print it
  invisible()
}

jmp <- function(state) {
  # Jumping to a NULL state means quiting frndly 
  if (is.null(state)) return(bye())
  # Get the global variable whose name is the argument with "_state" 
  # appended. This global variable is a list constructed by new_state()
  # below.
  state <- get(str_c(state, "_state"))
  # Set the next state
  frndly$next_state <- state$next_state
  # States, such as assignment_state defined below, may have a test
  # which determines whether the user has successfully completed a task 
  # which is associated with the state. If there is such a test, and it
  # indicates failure, (possibly because the task has not yet been given,)
  # make the state's test the active one. 
  if (!is.null(state$test) && !state$test()) {
    frndly$test <- state$test    
  }
  # Display the state's message:
  frndly_out(state$msg)
}

nxt <- function() {
  jmp(frndly$next_state)
}
# Store the following message as the "source" attribute of the function 
# nxt().
attr(nxt, "source") <- "| Hey you! To evaluate a function in R, you need to put () on the end."

auto_advance <- function(...) {
  # Since it was registered without a "data" argument, auto_advance will be
  # called with 4 arguments.
  # The first of these, ..1, is the top-level command which the
  # user just entered. (Actually, it is the associated "call". See ?call.)
  # The second, ..2, is the result of evaluating that command.
  # The third and fourth are logical, indicating whether or not the
  # command was successful, and whether or not the output was visible,
  # respectively.
  # Code to indicate how the callback works
  print("auto_advancing")
  print(paste("  ", class(..1), ": ", as.expression(..1), sep=""))
  if(class(..1) == "<-"){
    # Command was an assignment. For a simple assignment, the name of the
    # variable assigned is the second element of as.list(..1).
    print(paste("  variable assigned:", as.expression(..1[[2]])))
    # For an assignment such as cars$speed <- 10, the second element of
    # ..1 is a call invoking $.
  }
  try(print(paste("  value computed:", ..2)), silent=TRUE)
  #
  # Hadley's original code
  if (is.null(frndly$test)) return(TRUE)
  # Auto-advance using nxt() if test returns TRUE
  if (frndly$test()) {
    nxt()    
  }
  return(TRUE)
}

new_state <- function(msg, next_state = NULL, test = NULL) {
  structure(list(msg = msg, next_state = next_state, test = test), 
            class = "state")
}

is.state <- function(x) inherits(x, "state")

start_state <- new_state("Hi! I'm frndly, your friendly introduction to R. I'm going to guide you through a quick introduction to R. You'll know it's me talking whenever you see output that starts with |. Otherwise you'll be interacting with R in exactly the same way you will when I'm not around. Type nxt() to run your first R function and proceed with your introduction to R", "assignment")

assignment_state <- new_state("In R, you create variables with the arrow: <-, like a <- 1 or b <- 2.  To continue, create a new variable called d with the value 10, or type nxt()", "arith", function() {exists("d", globalenv()) && get("d", globalenv()) == 10 })

arith_state <- new_state("Good work!", NULL)