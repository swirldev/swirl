find_lesson <- function(lesson_dir) {
  # Find 'lesson' file with or without extension
  grep("^lesson[.]?[A-Za-z]*$", list.files(lesson_dir), value=TRUE)
}

#' @importFrom tools file_ext
get_content_class <- function(file_name) {
  ext <- file_ext(file_name)
  tolower(ext)
}

### FUNCTIONS THAT RETURN LESSON OBJECT WITH ASSOCIATED ATTRIBUTES ###

parse_content <- function(file, e) UseMethod("parse_content")

parse_content.default <- function(file, e) {
  # If no extension on lesson file, then assume it's yaml
  parse_content.yaml(file, e)
}

parse_content.csv <- function(file, e) {
  df <- read.csv(file, as.is=TRUE)
  # Return lesson object
  lesson(df, lesson_name=e$temp_lesson_name, course_name=e$temp_course_name)
}

parse_content.rmd <- function(file, e) {
  rmd2df(file)
}

wrap_encoding <- function(raw_yaml) {
  if (class(raw_yaml) == "list") {
    retval <- lapply(raw_yaml, wrap_encoding)
    attributes(retval) <- attributes(raw_yaml)
    retval
  } else {
    if (class(raw_yaml) == "character") {
      if (Encoding(raw_yaml) == "unknown") {
        Encoding(raw_yaml) <- "UTF-8"
      }
    }
    raw_yaml
  }
}

#' @importFrom yaml yaml.load_file
parse_content.yaml <- function(file, e){
  newrow <- function(element){
    temp <- data.frame(Class=NA, Output=NA, CorrectAnswer=NA,
                       AnswerChoices=NA, AnswerTests=NA, 
                       Hint=NA, Figure=NA, FigureType=NA, 
                       VideoLink=NA, Script=NA)
    for(nm in names(element)){
      # Only replace NA with value if value is not NULL, i.e. instructor
      # provided a nonempty value
      if(!is.null(element[[nm]])) {
        temp[,nm] <- element[[nm]]
      }
    }
    temp
  }
  raw_yaml <- yaml.load_file(file)
  raw_yaml <- wrap_encoding(raw_yaml)
  temp <- lapply(raw_yaml[-1], newrow)
  df <- NULL
  for(row in temp){
    df <- rbind(df, row)
  }
  meta <- raw_yaml[[1]]
  lesson(df, lesson_name=meta$Lesson, course_name=meta$Course,
         author=meta$Author, type=meta$Type, organization=meta$Organization,
         version=meta$Version, partner=meta$Partner)
}
