# Get swirl data file path
#' @importFrom rappdirs user_data_dir
swirl_data_dir <- function(){
  # Find user data directory
  user_data_dir(appname = "swirl", appauthor = "swirldev", roaming = TRUE)
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
