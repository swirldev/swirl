do_nxt <- function(e)UseMethod("do_nxt")
do_reset <- function(e)UseMethod("do_rst")
do_submit <- function(e)UseMethod("do_submit")
do_play <- function(e)UseMethod("do_play")
do_main <- function(e)UseMethod("do_main")
do_restart <- function(e)UseMethod("do_restart")

do_nxt.default <- function(e) {
  ## Using the stored list of "official" swirl variables and values,
  #  assign variables of the same names in the global environment
  #  their "official" values, in case the user has changed them
  #  while playing.
  if(length(e$snapshot)>0)xfer(as.environment(e$snapshot), globalenv())
  swirl_out(get_string("actions", 1))
  e$playing <- FALSE
  e$iptr <- 1  
}

do_reset.default <- function(e) {
  e$playing <- FALSE
  e$reset <- TRUE
  e$iptr <- 2
  swirl_out(get_string("actions", 2), 
            skip_after = TRUE)
}

do_submit.default <- function(e) {
  e$playing <- FALSE
  # Get contents from user's submitted script
  e$script_contents <- readLines(e$script_temp_path, warn = FALSE)
  # Save expr to e
  e$expr <- try(parse(text = e$script_contents), silent = TRUE)
  swirl_out(get_string("actions", 3), skip_after = TRUE)
  try(source(e$script_temp_path))
}

do_play.default <- function(e) {
  swirl_out(get_string("actions", 4), skip_after=TRUE)
  e$playing <- TRUE
}

do_main.default <- function(e) {
  swirl_out(get_string("actions", 5))
  # Remove the current lesson. Progress has been saved already.
  if(exists("les", e, inherits=FALSE)){
    rm("les", envir=e, inherits=FALSE)
  }
}

do_restart.default <- function(e) {
  swirl_out(get_string("actions", 6))
}