post_init <- function(e) UseMethod("post_init")
post_exercise <- function(e, current.row) UseMethod("post_exercise")
post_mult_question <- function(e, choices) UseMethod("post_mult_question")
post_result <- function(e, passed, submission, feedback, hint) UseMethod("post_result")
post_progress <- function(e) UseMethod("post_progress")
post_finished <- function(e) UseMethod("post_finished")

post_init.default <- function(e) {
  # do nothing
}

post_exercise.default <- function(e, current.row) {
  # Suppress extra space if multiple choice
  is_mult <- is(e$current.row, "mult_question")
  # Present output to user
  swirl_out(current.row[, "Output"], skip_after = !is_mult)
}

post_mult_question.default <- function(e, choices) {
  return(select.list(sample(choices), graphics=FALSE))
}

post_result.default <- function(e, passed, feedback, hint) {
  swirl_out(feedback)
  if(!passed) {
    # If hint is specified, print it. Otherwise, just skip a line.
    if(!is.null(hint)) {
      # Suppress extra space if multiple choice
      is_mult <- is(e$current.row, "mult_question")
      swirl_out(hint, skip_after = !is_mult) 
    } else {
      message()
    }
  }
}

post_progress.default <- function(e) {
  cat("\n")
  setTxtProgressBar(e$pbar, e$pbar_seq[e$row])
}

post_finished.default <- function(e) {
  swirl_out(s()%N%"Lesson complete! Exiting swirl now...", skip_after=TRUE)
}
