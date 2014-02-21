#' Author a swirl lesson.
#' 
#' Takes full names of lesson and course as input, creates the appropriate 
#' subdirectories in the swirl package directory, then opens a lesson template
#' in an R Markdown file for the instructor to begin editing.
#' @param lesson_name Full name of the lesson being authored
#' @param course_name Full name of the course to which this lesson belongs
#' @param new_course Boolean (TRUE or FALSE). Is this a new course?
#' @param type Optional argument used to specify special purpose content
#' @importFrom whisker whisker.render
#' @export
#' @examples
#' \dontrun{
#' 
#' author_lesson("Example Lesson Name", "Example Course Name", new_course=TRUE)
#' }
author_lesson = function(lesson_name, course_name, new_course, type=NULL) {
  
  # Convert lesson name and course name to more desirable file path formats.
  les_dirname <- make_pathname(lesson_name)
  crs_dirname <- make_pathname(course_name)
  
  # Create path for course directory in current working directory.
  path2course <- file.path(getwd(), crs_dirname)
  
  # User specifies new course but course dir already exists
  if(file.exists(path2course) && new_course) {
    stop(paste0("Course directory for \'", course_name, 
               "\' already exists in the current working directory!\n"))
  }
  
  # User specifies existing course but course dir does not exist
  if(!file.exists(path2course) && !new_course) {
    stop(paste0("Course directory for \'", course_name, 
               "\' not found in the current working directory!"))
  }
  
  # Create path to lesson directory
  path2les_dir <- file.path(path2course, les_dirname)
  # Create path for lesson within lesson subdirectory
  path2les <- file.path(path2les_dir, "lesson.Rmd")
  
  # Deals with case when user specifies lesson that already exists
  if(file.exists(path2les)) {
    message(paste0("A lesson called \'", lesson_name, "\' already exists in course \'", course_name, "\'!"))
  } else {
    # Read in course template and add custom YAML
    message("Customizing lesson template ...\n")
    path2temp <- file.path(path.package("swirl"), "templates", "lesson.Rmd")
    temp <- readLines(path2temp, warn=FALSE)
    dat <- list(lesson_name = lesson_name,
                course_name = course_name,
                swirl_version = packageVersion("swirl"),
                lesson_type = ifelse(is.null(type), "Standard", type)
    )
    out <- whisker.render(temp, dat)
    # Create lesson directory
    dir.create(path2les_dir, recursive=TRUE)
    # Write customized template to new lesson file in lesson directory
    message(paste("Writing lesson template to", path2les, "...\n"))
    writeLines(out, path2les)
  }
  
  # Open R Markdown file for new lesson.
  message("Opening lesson for editing ...\n")
  file.edit(path2les)
  
  invisible()
}

## UTILS

# Takes a plain English name and turns it into a more proper file/directory name
make_pathname <- function(name) {
  gsub(" ", "_", str_trim(name))
}