find_lesson <- function(lesson_dir) {
  grep("^lesson\\.[A-Za-z]+$", list.files(lesson_dir), value=TRUE)
}

#' @importFrom tools file_ext
get_content_class <- function(file_name) {
  ext <- file_ext(file_name)
  tolower(ext)
}

parse_content <- function(file) UseMethod("parse_content")

parse_content.default <- function(file) {
  stop("Incorrect content class!")
}

parse_content.csv <- function(file) {
  read.csv(file, as.is=TRUE)
}

parse_content.rmd <- function(file) {
  rmd2df(file)
}