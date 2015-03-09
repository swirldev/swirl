post_exercise <- function(e, current.row) UseMethod("post_exercise")
post_result <- function(e, passed, submission, feedback, hint) UseMethod("post_result")
post_progress <- function(e) UseMethod("post_progress")
post_finished <- function(e) UseMethod("post_finished")

post_exercise.default <- function(e, current.row) {
  # Suppress extra space if multiple choice
  is_mult <- is(e$current.row, "mult_question")
  # Present output to user
  swirl_out(current.row[, "Output"], skip_after = !is_mult)
}

#' @importFrom swirlcamp post_exercise
post_exercise.datacamp <- function(e, current.row) {
  swirlcamp::post_exercise(current.row[, "Output"], class(current.row)[1])
}

post_result.default <- function(e, passed, submission, feedback, hint) {
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

#' @importFrom swirlcamp post_result
post_result.datacamp <- function(e, passed, submission, feedback, hint) {
  if(!passed && !is.null(hint)) {
    swirlcamp::post_result(passed, submission, paste(feedback, hint))
  } else {
    swirlcamp::post_result(passed, submission, feedback)
  }
}

post_progress.default <- function(e) {
  cat("\n")
  setTxtProgressBar(e$pbar, e$pbar_seq[e$row])
}

#' @importFrom swirlcamp post_progress
post_progress.datacamp <- function(e) {
  swirlcamp::post_progress(e$pbar_seq[e$row])
}

post_finished.default <- function(e) {
  swirl_out("Lesson complete! Exiting swirl now...", skip_after=TRUE)
}

#' @importFrom swirlcamp post_finished
post_finished.datacamp <- function(e) {
  swirlcamp::post_finished()
}

