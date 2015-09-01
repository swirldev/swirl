get_string <- function(filename, index){
  strings <- list(
    english = english_strings() #,
    #espanol = kordos()
  )
  strings[[get_swirl_option("language")]][[filename]][[index]]
}

english_strings <- function(){
  list(
    actions = list(
      "Resuming lesson...",
      "I just reset the script to its original state. If it doesn't refresh immediately, you may need to click on it.",
      "Sourcing your script...",
      "Entering play mode. Experiment as you please, then type nxt() when you are ready to resume the lesson.",
      "Returning to the main menu...",
      "This feature is not implemented yet for Swirl."
    )
  )
}