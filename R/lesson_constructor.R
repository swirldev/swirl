# Constructor function for objects of class "lesson"
lesson <- function(df, lesson_name=NULL, course_name=NULL, author=NULL, 
                   type=NULL, organization=NULL, version=NULL, partner=NULL) {
  
  if(!is.data.frame(df)) 
    stop("Argument 'df' must be a data frame!")
  
  # Adding secondary class of data.frame allows lessons to retain data.frame attributes (e.g. dim())
  structure(df, lesson_name=lesson_name, course_name=course_name, author=author,
            type=type, organization=organization, version=version, partner=partner,
            class=c("lesson", "data.frame"))
}