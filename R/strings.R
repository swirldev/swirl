get_string <- function(filename, index){
  eval(parse(text = paste0(get_swirl_option("language"), "_strings")))[[filename]][[index]]
}

english_strings <- list(
    actions = list(
      "Resuming lesson...",
      "I just reset the script to its original state. If it doesn't refresh immediately, you may need to click on it.",
      "Sourcing your script...",
      "Entering play mode. Experiment as you please, then type nxt() when you are ready to resume the lesson.",
      "Returning to the main menu...",
      "This feature is not implemented yet for Swirl."
    )
)

save(english_strings, file = "R/sysdata.rda")
