library(stringr)

swirlenv <- new.env(parent = emptyenv())

swirl_out <- function(...) {
  wrapped <- strwrap(str_c(..., sep = " "),
    width = getOption("width") - 2)
  message(str_c("| ", wrapped, collapse = "\n"))
}

hiswirl <- function() {
  addTaskCallback(auto_advance, name = "swirl")
  jmp("start")
  invisible()
}

byeswirl <- function() {
  removeTaskCallback(which(getTaskCallbackNames() == "swirl"))
  invisible()
}

jmp <- function(state) {
  if (is.null(state)) return(byeswirl())

  state <- get(str_c(state, "_state"))

  swirlenv$next_state <- state$next_state

  if (!is.null(state$test)) {
    swirlenv$test <- state$test
  }

  swirl_out(state$msg)
}

nxt <- function() {
  jmp(swirlenv$next_state)
}

auto_advance <- function(expr, val, ok, visible) {
  if (is.null(swirlenv$test)) return(TRUE)
  
  tree <- new_tree(expr, val, ok)
  if (swirlenv$test(tree)) {
    nxt()
  }
  
  return(TRUE)
}

new_state <- function(msg, next_state = NULL, test = NULL) {
  structure(list(msg = msg, next_state = next_state, test = test), 
    class = "state")
}

is.state <- function(x) inherits(x, "state")

start_state <- new_state("Hi! I'm swirl! I'm
  going to guide you through a quick introduction to R. You'll know it's me
  talking whenever you see output that starts with |. Otherwise you'll be
  interacting with R in exactly the same way you will when I'm not around.

  Type nxt() now to get started!", "assignment")

assignment_state_test <- function(tree) {
  is_assgn <- identical(treetop(tree), "<-")
  swirlenv$usersymbol <- treearg(tree, 1)
  return(is_assgn)
}

assignment_state <- new_state("In R, you create variables with the arrow: <-,
  like a <- 1 or b <- 2. To create a vector of numbers, you use the 'c' function,
  like c(1, 2, 3). Create a vector with the numbers 2, 3, 4 and assign it to a
  variable; you can name it anything you'd like.", "sum", assignment_state_test)

sum_state_test <- function(tree) {
  is_sum <- identical(treetop(tree), "sum")
  return(is_sum)
}

sum_state <- new_state("Great, I see you've stored it in some variable. To sum 
                       the elements of a vector, you use the 'sum' function, like
                       sum(v) if your vector is stored in a variable 'v'. Compute
                       the sum of the elements of your vector now.",
                       "final", sum_state_test)

final_state <- new_state("Congrats! Good work summing.", NULL, NULL)
