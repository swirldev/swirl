# Return random praise.
praise <- function() {
  swirl_is_fun <- getOption("swirl_is_fun")
  
  if(is.null(swirl_is_fun) || isTRUE(swirl_is_fun)) {
    phrases <- get_string("phrases", 1)
  } else {
    phrases <- get_string("phrases", 2)
  }
  sample(phrases, 1)
}

# Return random "try again" message.
tryAgain <- function() {
  swirl_is_fun <- getOption("swirl_is_fun")
  
  if(is.null(swirl_is_fun) || isTRUE(swirl_is_fun)) {
    phrases <- get_string("phrases", 3)
  } else {
    phrases <- get_string("phrases", 4)
  }
  sample(phrases, 1)
}
