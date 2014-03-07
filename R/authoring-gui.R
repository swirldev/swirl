# Write a single unit using shiny GUI
# TODO: include shiny in imports and remove '::' below?

write_unit <- function(lessonFile) {
  # Returns unit info, or NULL if done
  vals <- shiny::runApp(system.file("authoring-gui", package="swirl"),
                        launch.browser = rstudio::viewer)
  # If NULL, then user is done
  if(is.null(vals)) {
    return(NULL)
  }
  # Write unit info to file
  cat(paste0("\n- ", paste0(names(vals), ": ", vals, collapse="\n  "), "\n"),
      file=lessonFile, append=TRUE)
  # Return TRUE if user not done yet
  return(TRUE)
}

newYaml <- function(course, lesson) {
  lessonDir <- file.path(gsub(" ", "_", course), gsub(" ", "_", lesson))
  if(!file.exists(lessonDir)) {
    dir.create(lessonDir, recursive=TRUE)
  }
  writeLines("# Put initialization code in this file.", 
             file.path(lessonDir, "initLesson.R"))
  writeLines("# Put custom tests in this file.", 
             file.path(lessonDir,"customTests.R"))
  lessonFile <- file.path(lessonDir, "lesson")
  writeLines(c("- Class: meta", 
               paste("  Course:", course),
               paste("  Lesson:", lesson),
               "  Author: Your name goes here",
               "  Type: Standard",
               "  Organization: Your organization goes here (optional)",
               paste("  Version:", packageVersion("swirl"))),
             lessonFile)
  file.edit(lessonFile)
  return(lessonFile)
}

#' Author lesson using content authoring tool
#' 
#' @param course course name
#' @param lesson lesson name
#' @export
author_gui <- function(course, lesson) {
  # Create course skeleton and open new lesson file
  lessonFile <- newYaml(course, lesson)
  # Initialize result
  result <- TRUE
  # Loop until user is done
  while(!is.null(result)) {
    result <- write_unit(lessonFile)
  }
  invisible()
}