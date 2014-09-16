
find_lesson <- function(lesson_dir) {
  # Find 'lesson' file with or without extension
  grep("^lesson[.]?[A-Za-z]*$", list.files(lesson_dir), value=TRUE)
}

#' @importFrom tools file_ext
get_content_class <- function(file_name) {
  ext <- file_ext(file_name)
  tolower(ext)
}


# Parse a lesson text to generate content from R expressions defined in the Parse field
parse_element <- function(df, expressions){
	
	# Evaluate the expressions
	expressions <- sapply(unlist(expressions), function(x) eval(parse(text=x)))
	
	parse_text <- function(text){
		if(!is.na(text)){
			# Find the markers
			found <- gregexpr('_([^_]*)_', text)
			markers <- gsub('_', '', unlist(regmatches(text, found)), fixed=TRUE)
			
			# Fill in with the results of the expressions
			parsed <- expressions[markers]
			
			regmatches(text, found) <- list(parsed)
		}
		text
	}
	
	df <- lapply(df, parse_text)
	df <- as.data.frame(df)
	df	
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

#' @importFrom yaml yaml.load_file
parse_content.yaml <- function(file, e){
  newrow <- function(element){
    temp <- data.frame(Class=NA, Output=NA, CorrectAnswer=NA,
                       AnswerChoices=NA, AnswerTests=NA, 
                       Hint=NA, Figure=NA, FigureType=NA, 
                       VideoLink=NA, Script=NA)
    
    for(nm in names(element)){
      # Only replace NA with value if value is not NULL, i.e. instructor
      # provided a nonempty value. Ignore the Parse field.
      if(!is.null(element[[nm]]) & nm!='Parse') {
        temp[,nm] <- element[[nm]]
      }
    }

    # Parse lesson defined content to replace defined markers with the result
    # of the named expressions in the Parsed field of the element
    if(any(!is.null(element$Parse))){
    	temp$Parse <- NULL
    	temp <- parse_element(df=temp, expressions=list(element$Parse))
    }
    temp
  }
  raw_yaml <- yaml.load_file(file)
  # Get metadata (including marker for text parsing)
  meta <- raw_yaml[[1]]
  
  temp <- lapply(raw_yaml[-1], newrow)
  df <- NULL
  df <- do.call(rbind.data.frame, temp)
  
  lesson(df, lesson_name=meta$Lesson, course_name=meta$Course,
         author=meta$Author, type=meta$Type, organization=meta$Organization,
         version=meta$Version)
}