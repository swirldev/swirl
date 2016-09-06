args_specification <- function(e, ...)UseMethod("args_specification")

args_specification.default <- function(e, ...) {
  # in normal, interactive mode, do nothing
  targs <- list(...)
  set_swirl_user(e, targs)
  
  # return(e)
  return(invisible(NULL))
}

args_specification.test <- function(e, ...) {
  # Capture ... args
  targs <- list(...)
  set_swirl_user(e, targs)
  
  # Check if appropriately named args exist
  if(is.null(targs$test_course) || is.null(targs$test_lesson)) {
    stop(s()%N%"Must specify 'test_course' and 'test_lesson' to run in 'test' mode!")
  } else {
    # Make available for use in menu functions
    e$test_lesson <- targs$test_lesson
    e$test_course <- targs$test_course
  }
  # Check that 'from' is less than 'to' if they are both provided
  if(!is.null(targs$from) && !is.null(targs$to)) {
    if(targs$from >= targs$to) {
      stop(s()%N%"Argument 'to' must be strictly greater than argument 'from'!")
    }
  }
  if(is.null(targs$from)) {
    e$test_from <- 1
  } else {
    e$test_from <- targs$from
  }
  if(is.null(targs$to)) {
    e$test_to <- 999 # Lesson will end naturally before this
  } else {
    e$test_to <- targs$to
  }
  return(invisible(NULL))
} 


set_swirl_user = function(e, targs) {
  swirl_user = getOption("swirl_user")
  if (is.null(targs$usr)) {
    targs$usr = swirl_user
  }  
  if (!is.null(targs$usr)) {
    e$usr = targs$usr
    udat <- file.path(progressDir(e), e$usr)
    if (!file.exists(udat)) {
      housekeeping(e)
      dir.create(udat, recursive = TRUE)
    }
    e$udat <- udat    
  }
  return(invisible(NULL))
}