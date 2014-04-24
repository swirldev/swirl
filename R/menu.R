## Method declarations

mainMenu <- function(e, ...)UseMethod("mainMenu")
welcome <- function(e, ...)UseMethod("welcome")
housekeeping <- function(e, ...)UseMethod("housekeeping")
inProgressMenu <- function(e, choices, ...)UseMethod("inProgressMenu")
courseMenu <- function(e, courses)UseMethod("courseMenu")
courseDir <- function(e)UseMethod("courseDir")
lessonMenu <- function(e, choices)UseMethod("lessonMenu")
restoreUserProgress <- function(e, selection)UseMethod("restoreUserProgress")
loadLesson <- function(e, ...)UseMethod("loadLesson")
loadInstructions <- function(e, ...)UseMethod("loadInstructions")

# Default course and lesson navigation logic
# 
# This method implements default course and lesson navigation logic, 
# decoupling menu presentation from internal processing of user
# selections. It relies on several methods for menu presentation,
# namely welcome(e), housekeeping(e), inProgressMenu(e, lessons),
# courseMenu(e, courses), and lessonMenu(e, lessons). Defaults 
# are provided.
# 
# @param e persistent environment accessible to the callback
#'@importFrom yaml yaml.load_file
mainMenu.default <- function(e){
  # Welcome the user if necessary and set up progress tracking
  if(!exists("usr",e,inherits = FALSE)){
    e$usr <- welcome(e)
    udat <- file.path(find.package("swirl"), "user_data", e$usr)
    if(!file.exists(udat)){
      housekeeping(e)
      dir.create(udat, recursive=TRUE)
    }
    e$udat <- udat
  }
  # If there is no active lesson, obtain one.
  if(!exists("les",e,inherits = FALSE)){
    # First, allow user to continue unfinished lessons
    # if there are any
    pfiles <- inProgress(e)
    response <- ""
    if(length(pfiles) > 0){
      response <- inProgressMenu(e, pfiles)
    }
    if(response != "" ){
      # If the user has chosen to continue, restore progress
      response <- gsub(" ", "_", response)
      response <- paste0(response,".rda")
      restoreUserProgress(e, response)
    } else {
      # Else load a new lesson.
      # Let user choose the course.
      coursesU <- dir(courseDir(e))
      # Eliminate empty directories
      idx <- unlist(sapply(coursesU, 
                    function(x)length(dir(file.path(courseDir(e),x)))>0))
      coursesU <- coursesU[idx]
      
      # If no courses are available, offer to install one
      if(length(coursesU)==0){
        suggestions <- yaml.load_file(file.path(courseDir(e), "suggested_courses.yaml"))
        choices <- sapply(suggestions, function(x)paste0(x$Course, ": ", x$Description))
        swirl_out("To begin, you must install a course. I can install a",
                  "course for you from the internet, or I can send you to a web page",
                  "(https://github.com/swirldev/swirl_courses)",
                  "which will provide course options and directions for", 
                  "installing courses yourself.",
                  "(If you are not connected to the internet, type 0 to exit.)")
        choices <- c(choices, "Don't install anything for me. I'll do it myself.")
        choice <- select.list(choices)
        n <- which(choice == choices)
        if(length(n) == 0)return(FALSE)
        if(n < length(choices)){
          repeat {
            temp <- try(eval(parse(text=suggestions[[n]]$Install)), silent=TRUE)
            if(is(temp, "try-error")){
              swirl_out("Sorry, but I'm unable to fetch ", sQuote(choice),
                        "right now. Are you sure you have an internet connection?",
                        "If so, would you like to try again or visit",
                        "the course repository for instructions on how to",
                        "install a course manually? Type 0 to exit.")
              ch <- c("Try again!", 
                      "Send me to the course repository for manual installation.")
              resp <- select.list(ch, graphics=FALSE)
              if(resp == "") return(FALSE)
              if(resp == ch[2]) {
                swirl_out("OK. I'm opening the swirl course respository in your browser.")
                browseURL("https://github.com/swirldev/swirl_courses#install-and-run-a-course-manually")
                return(FALSE)
              }
            } else {
              break # Break repeat loop if install is successful
            }
          }
          coursesU <- dir(courseDir(e))
          # Eliminate empty directories
          idx <- unlist(sapply(coursesU, 
                               function(x)length(dir(file.path(courseDir(e),x)))>0))
          coursesU <- coursesU[idx]
        } else {
          swirl_out("OK. I'm opening the swirl course respository in your browser.")
          browseURL("https://github.com/swirldev/swirl_courses#swirl-courses")
          return(FALSE)
        }
      }
      # path cosmetics
      coursesR <- gsub("_", " ", coursesU)
      lesson <- ""
      while(lesson == ""){
        course <- courseMenu(e, coursesR)
        if(!is.null(names(course)) && names(course)=="repo") {
          swirl_out("OK. I'm opening the swirl courses web page in your browser.")
          browseURL("https://github.com/swirldev/swirl_courses#swirl-courses")
          return(FALSE)
        }
        if(course=="")return(FALSE)
        # Set temp course name since csv files don't carry attributes
        e$temp_course_name <- course
        # reverse path cosmetics
        courseU <- coursesU[course == coursesR]
        course_dir <- file.path(courseDir(e), courseU)
        # Get all files/folders from course dir, excluding MANIFEST
        lessons <- dir(course_dir)
        lessons <- lessons[lessons != "MANIFEST"]
        # If MANIFEST exists in course directory, then order courses
        man_path <- file.path(course_dir, "MANIFEST")
        if(file.exists(man_path)) {
          manifest <- get_manifest(course_dir)
          lessons <- order_lessons(current_order=lessons, 
                                   manifest_order=manifest)
        }
        # Clean up lesson names
        lessons_clean <- gsub("_", " ", lessons)
        # Let user choose the lesson.
        lesson_choice <- lessonMenu(e, lessons_clean)
        # Set temp lesson name since csv files don't have lesson name attribute
        e$temp_lesson_name <- lesson_choice
        # reverse path cosmetics
        lesson <- ifelse(lesson_choice=="", "",
                         lessons[lesson_choice == lessons_clean])
        # Load the lesson and intialize everything
        e$les <- loadLesson(e, courseU, lesson)
        # Return to the course menu if the lesson failed to load
        if(is.logical(e$les) && !isTRUE(e$les)){
          rm("les", envir=e, inherits=FALSE)
          lesson <- ""
          next()
        }
      }
   # Remove temp lesson name and course name vars, which were surrogates
      # for csv attributes -- they've been attached via lesson() by now
      rm("temp_lesson_name", "temp_course_name", envir=e, inherits=FALSE)
      
      # Initialize the progress bar
      e$pbar <- txtProgressBar(style=3)
      e$pbar_seq <- seq(0, 1, length=nrow(e$les))
      
      # expr, val, ok, and vis should have been set by the callback.
      # The lesson's current row
      e$row <- 1
      # The current row's instruction pointer
      e$iptr <- 1
      # A flag indicating we should return to the prompt
      e$prompt <- FALSE
      # The job of loading instructions for this "virtual machine"
      # is relegated to an S3 method to allow for different "programs."
      loadInstructions(e)
      # An identifier for the active row
      e$current.row <- NULL
      # For sourcing files which construct figures etc
      e$path <- file.path(courseDir(e), courseU, lesson)
      # Set up paths and files to save user progress
      # Make file path from lesson info
      fname <- progressName(attr(e$les,"course_name"), attr(e$les,"lesson_name"))
      # path to file 
      e$progress <- file.path(e$udat, fname)
      # indicator that swirl is not reacting to console input
      e$playing <- FALSE
      # create the file
      suppressMessages(suppressWarnings(saveRDS(e, e$progress)))
    }
  }
  return(TRUE)
}

welcome.test <- function(e, ...){
  "author"
}

# Default version.
welcome.default <- function(e, ...){
  swirl_out("Welcome to swirl! Please sign in. If you've been here before, use the same name as you did then. If you are new, call yourself something unique.", skip_after=TRUE)
  return(readline("What shall I call you? "))
}

# Presents preliminary information to a new user
# 
# @param e persistent environment used here only for its class attribute
# 
housekeeping.default <- function(e){
  swirl_out(paste0("Thanks, ", e$usr,". Let's cover a couple of quick housekeeping items before we begin our first lesson. First off, you should know that when you see '...', that means you should press Enter when you are done reading and ready to continue."))
  readline("\n...  <-- That's your cue to press Enter to continue")
  swirl_out("Also, when you see 'ANSWER:', the R prompt (>), or when you are asked to select from a list, that means it's your turn to enter a response, then press Enter to continue.")
  select.list(c("Continue.", "Proceed.", "Let's get going!"),
              title="\nSelect 1, 2, or 3 and press Enter", graphics=FALSE)
  swirl_out("You can exit swirl and return to the R prompt (>) at any time by pressing the Esc key. If you are already at the prompt, type bye() to exit and save your progress. When you exit properly, you'll see a short message letting know you've done so.")
  info()
  swirl_out("Let's get started!", skip_before=FALSE)
  readline("\n...")
}

housekeeping.test <- function(e){}

# A stub. Eventually this should be a full menu
inProgressMenu.default <- function(e, choices){
  nada <- "No. Let me start something new."
  swirl_out("Would you like to continue with one of these lessons?")
  selection <- select.list(c(choices, nada), graphics=FALSE)
  # return a blank if the user rejects all choices
  if(identical(selection, nada))selection <- ""
  return(selection)
}

inProgressMenu.test <- function(e, choices) {
  ""
}

# A stub. Eventually this should be a full menu
courseMenu.default <- function(e, choices){
  repo_option <- "Take me to the swirl course repository!"
  choices <- c(choices, repo = repo_option)
  swirl_out("Please choose a course, or type 0 to exit swirl.")
  return(select.list(choices, graphics=FALSE))
}

courseMenu.test <- function(e, choices) {
  e$test_course
}

# A stub. Eventually this should be a full menu
lessonMenu.default <- function(e, choices){
  swirl_out("Please choose a lesson, or type 0 to return to course menu.")
  return(select.list(choices, graphics=FALSE))
}

lessonMenu.test <- function(e, choices) {
  e$test_lesson
}

loadLesson.default <- function(e, courseU, lesson){
  # Load the content file
  lesPath <- file.path(courseDir(e), courseU, lesson)
  shortname <- find_lesson(lesPath)
  dataName <- file.path(lesPath,shortname)
  # Handle dependencies
  if(!loadDependencies(lesPath))return(FALSE)
  # Before initializing the module, take a snapshot of 
  #  the global environment.
  snapshot <- as.list(globalenv())
  # initialize course lesson, assigning lesson-specific variables
  initFile <- file.path(lesPath,"initLesson.R")
  if (file.exists(initFile)){
    source(initFile)
  }
  #  After initializing, compare a current snapshot of the 
  #  global environment with the previous to detect any variables
  #  created or changed by initialization. Add these to the list
  #  of "official" swirl names and values.
  e$snapshot <- as.list(globalenv())
  idx <- !(e$snapshot %in% snapshot)
  e$official <- e$snapshot[idx]
  # load any custom tests, returning FALSE if they fail to load
  clearCustomTests()
  loadCustomTests(lesPath)
  
  # Attached class to content based on file extension
  class(dataName) <- get_content_class(dataName)
  
  # Parse content, returning object of class "lesson"
  return(parse_content(dataName, e))
}

restoreUserProgress.default <- function(e, selection){
  # read the progress file
  temp <- readRDS(file.path(e$udat, selection))
  # transfer its contents to e
  xfer(temp, e)
  # Since loadDepencies will have worked once, we don't
  # check for failure here. Perhaps we should.
  loadDependencies(e$path)
  # TODO: We probably shouldn't be doing this again.
  # source the initLesson.R file if it exists
  initf <- file.path(e$path, "initLesson.R")
  if(file.exists(initf))source(initf)
  # transfer swirl's "official" list of variables to the
  # global environment.
  xfer(as.environment(e$official), globalenv())
  # load any custom tests
  clearCustomTests()
  loadCustomTests(e$path)
  # eval retrieved user expr's in global env, but don't include
  # call to swirl (the first entry)
  if(length(e$usrexpr) > 1){
    for(n in 2:length(e$usrexpr)){
      expr <- e$usrexpr[[n]]
      eval(expr, globalenv())
    }
  }
  # Restore figures which precede current row (Issue #44)
  idx <- 1:(e$row - 1)
  figs <- e$les[idx,"Figure"]
  # Check for missing Figure column (Issue #47) and omit NA's 
  if(is.null(figs) || length(figs) == 0)return()
  figs <- figs[!is.na(figs)]
  figs <- file.path(e$path, figs)
  lapply(figs, function(x)source(file=x, local=TRUE))
}

loadInstructions.default <- function(e){
  e$instr <- list(present, waitUser, testResponse)
}


# UTILITIES

progressName <- function(courseName, lesName){
  pn <- paste0(courseName, "_", lesName, ".rda")
  gsub(" ", "_", pn)
}

inProgress <- function(e){
  pfiles <- dir(e$udat)[grep("[.]rda$", dir(e$udat))]
  pfiles <- gsub("[.]rda", "", pfiles)
  pfiles <- str_trim(gsub("_", " ", pfiles))
  return(pfiles)
}

completed <- function(e){
  pfiles <- dir(e$udat)[grep("[.]done$", dir(e$udat))]
  pfiles <- gsub("[.]done", "", pfiles)
  pfiles <- gsub("[.]rda", "", pfiles)
  pfiles <- str_trim(gsub("_", " ", pfiles))
  return(pfiles)
}

get_manifest <- function(course_dir) {
  man <- readLines(file.path(course_dir, "MANIFEST"), warn=FALSE)
  # Remove leading and trailing whitespace
  man <- str_trim(man)
  # Remove empty lines
  man <- man[which(man != "")]
}

# Take vector of lessons and return in order given by manifest.
# Any courses not included in manifest are excluded!
order_lessons <- function(current_order, manifest_order) {
  current_order[match(manifest_order, current_order)]
}

courseDir.default <- function(e){
  # e's only role is to determine the method used
  file.path(find.package("swirl"), "Courses")
}

# Default for determining the user
getUser <- function()UseMethod("getUser")
getUser.default <- function(){"swirladmin"}
