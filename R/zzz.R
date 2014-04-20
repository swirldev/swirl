.onAttach <- function(...) {
  if(length(ls(envir=globalenv())) > 0) {
    swirl_out("Hi! I see that you have some variables saved in your",
      "workspace. To keep things running smoothly, I recommend you clean up",
      "before starting swirl.")
    swirl_out("Type ls() to see a list of the variables in your workspace.",
      "Then, type rm(list=ls()) to clear your workspace.")
    swirl_out("Type swirl() when you are ready to begin.", skip_after=TRUE)
  } else {
    swirl_out("Hi! Type swirl() when you are ready to begin.", skip_after=TRUE)
  }
}