# Constructor function for objects of class lesson (as required by swirl package)
lesson <- function(mod, modName=NULL, courseName=NULL, instructor=NULL, 
                   packReqs=NULL, predefVars=NULL) {
  # Give constructive error messages regarding bad or missing arguments
  if(!is.data.frame(mod)) 
    stop("Argument 'mod' must be a data.frame!")
  if(is.null(modName)) 
    stop("You must specify a name for the lesson with the 'modName' argument!")
  if(is.null(courseName)) 
    stop("You must specify a name for the course to which this lesson belongs
         with the 'courseName' argument!")
  if(is.null(instructor)) 
    stop("You must specify the instructor's name with the 'instructor' argument!")
  
  # Adding secondary class of data.frame allows lessons to retain data.frame attributes (e.g. dim())
  structure(mod, modName=modName, courseName=courseName, instructor=instructor,
            packReqs=packReqs, predefVars=predefVars, class=c("lesson", "data.frame"))
}