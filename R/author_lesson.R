#' Author a swirl lesson.
#' 
#' Takes full names of lesson and course as input, creates the appropriate 
#' subdirectories in the swirl package directory, then opens a lesson template
#' in an R Markdown file for the instructor to begin editing.
#' @param lesson_name Full name of the lesson being authored
#' @param course_name Full name of the course to which this lesson belongs
#' @export
#' @examples
#' \dontrun{
#' 
#' author_lesson("Example Lesson Name", "Example Course Name")
#' }
author_lesson = function(lesson_name, course_name) {
  
  # Convert lesson name and course name to more desirable file path formats.
  les_filename <- make_pathname(lesson_name)
  crs_dirname <- make_pathname(course_name)
  
  # Create directory for course in current working directory.
  path2course <- file.path(getwd(), "my_swirl_courses", crs_dirname)
  path2les <- file.path(path2course, les_filename, paste0(les_filename, ".Rmd"))
  
  # Copy course template into course directory.
  message("Preparing lesson template ...\n")
  scaffold <- system.file("skeleton", package = "swirl")
  copy_dir(scaffold, file.path(path2course, les_filename))
  
  # If successful, then rename file appropriately.
  if(file.exists(file.path(path2course, les_filename, "index.Rmd"))) {
    file.rename(file.path(path2course, les_filename, "index.Rmd"), path2les)
  }
  
  # Open R Markdown file for new lesson.
  message("Opening lesson (", les_filename, ".Rmd) for editing ...\n")
  file.edit(path2les)
  
  invisible()
}

## UTILS

# Takes a plain English name and turns it into a more proper file/directory name.
make_pathname <- function(name) {
  gsub(" ", "_", str_trim(name))
}

# Copies the contents of one directory to another. Copied directly from slidify.
copy_dir <- function(from, to){
  if (!(file.exists(to))){
    dir.create(to, recursive = TRUE)
    message('Copying file(s) to ', to, ' ...\n')
    file.copy(list.files(from, full.names = T), to, recursive = TRUE)
  }
}
