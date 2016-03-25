.onAttach <- function(...) {
  if(length(ls(envir=globalenv())) > 0) {
    packageStartupMessage(
      make_pretty(s()%N%"Hi! I see that you have some variables saved in your",
                  s()%N%"workspace. To keep things running smoothly, I recommend you clean up",
                  s()%N%"before starting swirl.", skip_after=TRUE),
      make_pretty(s()%N%"Type ls() to see a list of the variables in your workspace.",
                  s()%N%"Then, type rm(list=ls()) to clear your workspace.", skip_after=TRUE),
      make_pretty(s()%N%"Type swirl() when you are ready to begin.", skip_after=TRUE)
    )
  } else {
    packageStartupMessage(
      make_pretty(s()%N%"Hi! Type swirl() when you are ready to begin.",
                  skip_after=TRUE)
    )
  }
  invisible()
}

make_pretty <- function(..., skip_before=TRUE, skip_after=FALSE) {
  wrapped <- strwrap(str_c(..., sep = " "),
                     width = getOption("width") - 2)
  mes <- str_c("| ", wrapped, collapse = "\n")
  if(skip_before) mes <- paste0("\n", mes)
  if(skip_after) mes <- paste0(mes, "\n")
  mes
}