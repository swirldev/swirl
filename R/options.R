# Get path to a lesson
lesson_path <- function(course_name, lesson_name){
  file.path(swirl_courses_dir(), course_name, lesson_name)
}

# Get swirl data file path
swirl_data_dir <- function(){
  sdd <- getOption("swirl_data_dir")
  
  if(is.null(sdd)){
    file.path(find.package("swirl"), "user_data")
  } else {
    sdd
  }
}

# Get swirl courses dir
swirl_courses_dir <- function(){
  scd <- getOption("swirl_courses_dir")
  
  if(is.null(scd)){
    file.path(find.package("swirl"), "Courses")
  } else {
    scd
  }
}

#' Get swirl options
#' 
#' This function is a wrapper for \code{options()} that allows the user to
#' see the state of how certain options for swirl are set up.
#' 
#' @param ... any options can be defined, using name = value.
#' 
#' @export
#' @examples 
#' \dontrun{
#' # See current current swirl options
#' swirl_options()
#' 
#' # Set an option
#' swirl_options(swirl_logging = TRUE)
#' }
swirl_options <- function(...){
  if(length(list(...)) == 0){
    list(
      swirl_courses_dir = getOption("swirl_courses_dir"),
      swirl_data_dir = getOption("swirl_data_dir"),
      swirl_language = getOption("swirl_language"),
      swirl_logging = getOption("swirl_logging")
    )
  } else {
    options(...)
  }
}