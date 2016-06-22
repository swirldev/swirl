#' Installing Courses
#' 
#' swirl is designed so that anyone can create interactive content
#' and share it with the world or with just a few people. Users can
#' install courses from a variety of sources using the 
#' functions listed here. Each of these functions has its own help
#' file, which you can consult for more details.
#' 
#' If you're just getting started, we recommend using 
#' \code{\link{install_course}} to install courses
#' from our official \href{https://github.com/swirldev/swirl_courses}{course repository}. Otherwise, check out the
#' help file for the relevant install function below.
#' 
#' You can uninstall a course from swirl at any time with 
#' \code{\link{uninstall_course}}.
#' 
#' Uninstall all courses with
#' \code{\link{uninstall_all_courses}}.
#' 
#' @name InstallCourses
#' @family InstallCourses
NULL

#' Install a course from The swirl Course Network or install a course from a
#' local .swc file.
#' 
#' @description 
#' Version 2.4 of swirl introduces a new, simple, and fast way of installing
#' courses in the form of \code{.swc} files. This function allows a user to grab
#' a \code{.swc} file from The swirl Course Network which is maintained by Team
#' swirl, or the user can use this function to install a local \code{.swc} file.
#' When using this function please only provide an argument for either
#' \code{course_name} or \code{swc_path}, never both.
#' 
#' @param course_name The name of the course you wish to install.
#' @param swc_path The path to a local \code{.swc} file. By default this
#' argument defaults to \code{file.choose()} so the user can select the file using
#' their mouse.
#' @param force Should course installation be forced? The 
#'  default value is \code{FALSE}.
#' @importFrom httr GET progress content
#' @export
#' @family InstallCourses
#' @examples 
#' \dontrun{
#' 
#' # Install the latest version of Team swirl's R Programming course.
#' install_course("R Programming")
#' 
#' # Install a local .swc file by using your mouse and keyboard to select the
#' # file.
#' install_course()
#' 
#' # Install a .swc file from a specific path.
#' install_course(swc_path = file.path("~", "Downloads", "R_Programming.swc"))
#' 
#' }
install_course <- function(course_name = NULL, swc_path = NULL, force = FALSE){
  if(is.null(course_name) && is.null(swc_path)){
    swc_path <- file.choose()
  } 
  
  if(!is.null(course_name) && !is.null(swc_path)){
    stop(s()%N%"Please specify a value for either course_name or swc_path but not both.")
  } else if(!is.null(swc_path)){
    unpack_course(swc_path, swirl_courses_dir())
  } else { # install from swirl course network
    course_name <- make_pathname(course_name)
    url <- paste0("http://swirlstats.com/scn/", course_name, ".swc")
    
    # Send GET request
    response <- suppressWarnings(GET(url, progress()))
    
    if(response$status_code != 200){
      swirl_out(s()%N%"It looks like your internet connection is not working.",
                s()%N%"Go to http://swirlstats.com/scn/ and download the .swc file that corresponds to the course you wish to install.",
                s()%N%"After downloading the .swc run install_course() and choose the file you downloaded.")
      stop(s()%N%"Could not connect to course file.")
    }
    
    temp_swc <- tempfile()
    writeBin(content(response, "raw"), temp_swc)
    unpack_course(temp_swc, swirl_courses_dir(), force = force)
  }
}

#' Install a course from the official course repository
#' 
#' @description
#' We are currently maintaining a central repository of contributed
#' swirl courses at \url{https://github.com/swirldev/swirl_courses}.
#' This function provides the easiest method of installing a course
#' form the repository.
#' 
#' We have another repository at 
#' \url{https://github.com/swirldev/swirl_misc}, where we keep 
#' experimental features and content. The \code{dev} argument allows
#' you to access this repository. Content in the swirl_misc repository
#' is not guaranteed to work.
#' 
#' The central repository of swirl courses is mirrored at
#' \url{https://bitbucket.org/swirldevmirror/swirl_courses}. If you cannot 
#' access GitHub you can download swirl courses from bitbucket by using the
#' \code{mirror = "bitbucket"} option (see below).
#' 
#' @param course_name The name of the course you wish to install.
#' @param dev Set to \code{TRUE} to install a course in development from the swirl_misc repository.
#' @param mirror Select swirl course repository mirror. Valid arguments are \code{"github"} and \code{"bitbucket"}.
#' @export
#' @importFrom httr GET content progress
#' @examples
#' \dontrun{
#' 
#' install_from_swirl("R_Programming") # Directory name
#' 
#' ### OR ###
#' 
#' install_from_swirl("R Programming") # Course name
#' 
#' # To install a course in development from the swirl_misc repository
#' install_from_swirl("Including Data", dev = TRUE)
#' 
#' # To install a course from the Bitbucket mirror
#' install_from_swirl("R Programming", mirror = "bitbucket")
#' }
#' @family InstallCourses
install_from_swirl <- function(course_name, dev = FALSE, mirror = "github"){
  # Validate arguments
  if(!is.character(course_name)) {
    stop(s()%N%"Argument 'course_name' must be surrounded by quotes (i.e. a character string)!")
  }
  if(!is.logical(dev)) {
    stop(s()%N%"Argument 'dev' must be either TRUE or FALSE!")
  }
  if(!(mirror == "github" || mirror == "bitbucket")){
    stop(s()%N%"Please enter a valid name for a mirror. ('github' or 'bitbucket')")
  }
    
  # make pathname from course_name
  course_name <- make_pathname(course_name)
  
  # Construct url to the appropriate zip file
  if(dev) {
    if(mirror != "github"){
      stop(s()%N%"To access swirl courses in development on Bitbucket go to https://bitbucket.org/swirldevmirror/swirl_misc")
    }
    url <- "http://github.com/swirldev/swirl_misc/zipball/master"
  } else {
    if(mirror == "bitbucket"){
      url <- "https://bitbucket.org/swirldevmirror/swirl_courses/get/HEAD.zip"
    } else {
      url <- "http://github.com/swirldev/swirl_courses/zipball/master"
    }
  }
  
  # Send GET request
  response <- GET(url, progress())
  
  # Construct path to Courses
  path <- file.path(swirl_courses_dir(), "temp.zip")
  
  # Write the response as a zip
  writeBin(content(response, "raw"), path)
  
  # Find list of files not in top level directory
  file_names <- unzip(path, list=TRUE)$Name
  
  # Filter list
  unzip_list <- Filter(function(x) 
    {grepl(paste0("/", course_name, "/"), x)}, 
    file_names
  )
  
  # Check if course exists
  if(length(unzip_list) == 0) {
    stop(paste0(s()%N%"Course '", course_name, s()%N%"' not found in course repository! ",
                s()%N%"Make sure you've got the name exactly right, then try again."))
  }
  
  # Extract
  unzip(path, exdir=swirl_courses_dir(), files=unzip_list)
  
  # Copy files from unzipped directory into Courses
  top_dir <- file.path(swirl_courses_dir(), sort(dirname(unzip_list))[1])
  dirs_to_copy <- list.files(top_dir, full.names=TRUE)
  if(file.copy(dirs_to_copy, swirl_courses_dir(), recursive=TRUE)){
    swirl_out(s()%N%"Course installed successfully!", skip_after=TRUE)
  } else {
    swirl_out(s()%N%"Course installation failed.", skip_after=TRUE)
  }
  
  # Delete unzipped directory
  unlink(top_dir, recursive=TRUE, force=TRUE)
  
  # If __MACOSX exists, delete it.
  unlink(file.path(swirl_courses_dir(), "__MACOSX"), recursive=TRUE, force=TRUE)
  
  # Delete temp.zip
  unlink(path, force=TRUE)
  
  invisible()
}


#' Zip a course directory
#' 
#' \strong{Warning:} This function will be deprecated after swirl version 2.4.
#' 
#' @param path Path to the course directory to be zipped.
#' @param dest Path to directory in which the \code{.zip} should be saved. The
#'    default value is \code{NULL}, which will cause the \code{.zip} to be
#'    created one level above the directory specified in \code{path}.
#' @export
#' @examples
#' \dontrun{
#' 
#' zip_course("~/Desktop/LOESS_Modeling")
#' zip_course("~/Desktop/SNA_Tutorial", "~/tutorials")
#' }
#' @family InstallCourses
zip_course <- function(path, dest=NULL){
  .Deprecated("swirlify::pack_course")
  # Cleanse the path of the trailing slash
  path <- sub("/$", "", path)
  
  # Get highest directory if necessary
  if(is.null(dest)){
    dest <- sub(basename(path), "", path)
  }
  
  # Cleanse dest of the trailing slash
  dest <- sub("/$", "", dest)
  
  # Push current directory
  curr_dir <- getwd()
  
  # Create directory in which to zip
  zip_dir <- paste0(dest, "/", "swirl_zip_creator_TEMP")
  dir.create(zip_dir)
  if(file.copy(path, zip_dir, recursive=TRUE)){
    swirl_out("Course directory was successfully zipped!", skip_after=TRUE)
  } else {
    swirl_out("Course installation failed.", skip_after=TRUE)
  }
  
  # Change directory to folder to be zipped
  setwd(zip_dir)
  
  # Zip-A-Dee-Doo-Dah
  zip(paste0(dest, "/", basename(path), ".zip"), 
      list.files(getwd(), recursive=TRUE))
  
  # Delete temporary directory
  unlink(zip_dir, recursive=TRUE, force=TRUE)
  
  # Pop the old directory
  setwd(curr_dir)

  invisible()
}

#' Uninstall a course
#' 
#' @param course_name Name of course to be uninstalled
#' @export
#' @examples
#' \dontrun{
#' 
#' uninstall_course("Linear Regression")
#' }
#' @family InstallCourses
uninstall_course <- function(course_name){
  path <- file.path(swirl_courses_dir(), make_pathname(course_name))
  if(file.exists(path)){
    unlink(path, recursive=TRUE, force=TRUE)
    message(s()%N%"Course uninstalled successfully!")
  } else {
    stop(s()%N%"Course not found!")
  }
  invisible()
}

#' Uninstall all courses
#' 
#' @param force If \code{TRUE} the user will not be asked if they're sure they
#' want to delete the contents of the directory where courses are stored. The
#' default value is \code{FALSE}
#' @export
#' @examples
#' \dontrun{
#' 
#' uninstall_all_courses()
#' }
#' @family InstallCourses
uninstall_all_courses <- function(force = FALSE){
  path <- swirl_courses_dir()
  yaml_exists <- file.exists(file.path(path, "suggested_courses.yaml"))
  if(yaml_exists){
    temp_file <- tempfile()
    file.copy(file.path(path, "suggested_courses.yaml"), temp_file)
  }
  if(file.exists(path)){
    if(!force){
      swirl_out(s()%N%"Are you sure you want to uninstall all swirl courses?",
                s()%N%"This will delete all of the contents of your swirl course directory.")
      selection <- select.list(c(s()%N%"Yes", s()%N%"No"))
      if(selection == s()%N%"Yes"){
        unlink(path, recursive=TRUE, force=TRUE)
        message(s()%N%"All courses uninstalled successfully!")
      } else {
        message("No courses were uninstalled.")
        return()
      }
    } else {
      unlink(path, recursive=TRUE, force=TRUE)
      message(s()%N%"All courses uninstalled successfully!")
    }
  } else {
    stop(s()%N%"No courses found!")
  }
  
  dir.create(path, showWarnings = FALSE)
  
  if(yaml_exists){
    file.copy(temp_file, path)
    file.rename(list.files(path, full.names = TRUE), file.path(path, "suggested_courses.yaml"))
  }
  
  invisible()
}

#' Install a course from a zipped course folder
#' 
#' @param path The path to the zipped course.
#' @param multi Set to \code{TRUE} if the zipped directory contains multiple courses. The default value is \code{FALSE}.
#' @param which_course A vector of course names. Only for use when zip file contains multiple courses, but you don't want to install all of them.
#' @export
#' @examples
#' \dontrun{
#' 
#' install_course_zip("~/Desktop/my_course.zip")
#' 
#' install_course_zip("~/Downloads/swirl_courses-master.zip", multi=TRUE,
#'                    which_course=c("R Programming", "Data Analysis"))
#' }
#' @family InstallCourses
install_course_zip <- function(path, multi=FALSE, which_course=NULL){
  if(!is.logical(multi) || is.na(multi)) {
    stop(s()%N%"Argument 'multi' must be either TRUE or FALSE.")
  }
  if(!multi && !is.null(which_course)) {
    stop(s()%N%"Argument 'which_course' should only be specified when argument 'multi' is TRUE.")
  }
  if(multi){
    # Find list of files not in top level directory
    file_names <- unzip(path, list=TRUE)$Name
    
    # Filter list and extract
    unzip_list <- Filter(function(x){grepl("/.+/", x)}, file_names)
    unzip(path, exdir = swirl_courses_dir(), files=unzip_list)
    
    # Copy files from unzipped directory into Courses
    top_dir <- file.path(swirl_courses_dir(), sort(dirname(unzip_list))[1])
    dirs_to_copy <- list.files(top_dir, full.names=TRUE)
    # Subset desired courses if specified with which_courses arg
    if(!is.null(which_course)) {
      match_ind <- match(make_pathname(which_course), basename(dirs_to_copy),
                   nomatch=-1)
      nomatch <- match_ind < 0
      if(any(nomatch)) {
        stop(s()%N%"Course ", sQuote(which_course[nomatch][1]), s()%N%" not in specified directory. Be careful, course names are case sensitive!")
      }
      dirs_to_copy <- dirs_to_copy[match_ind]
    }
    if(file.copy(dirs_to_copy, swirl_courses_dir(), recursive=TRUE)){
      swirl_out(s()%N%"Course installed successfully!", skip_after=TRUE)
    } else {
      swirl_out(s()%N%"Course installation failed.", skip_after=TRUE)
    }
    
    # Delete unzipped directory
    unlink(top_dir, recursive=TRUE, force=TRUE)
    
  } else {
    # Unzip file into courses
    file_list <- unzip(path, exdir = swirl_courses_dir())
  }
  
  # If __MACOSX exists, delete it.
  unlink(file.path(swirl_courses_dir(), "__MACOSX"), recursive=TRUE, force=TRUE)

  invisible()
}

#' Install a course from a course directory
#' 
#' @param path The path to the course directory.
#' @export
#' @examples
#' \dontrun{
#' 
#' install_course_directory("~/Desktop/my_course")
#' }
#' @family InstallCourses
install_course_directory <- function(path){
  # Check for size of directory to prevent copying a huge directory into swirl/Courses
  garbage_result <- tryCatch(
    {setTimeLimit(elapsed=1); list.files(path, recursive=TRUE)},
    finally = {setTimeLimit(elapsed=Inf)}
  )
  
  # Check to make sure there are fewer than 1000 files in course directory
  if(length(garbage_result) > 1000){
    stop(s()%N%"Course directory is too large to install")
  }
  
  # Copy files
  if(file.copy(path, swirl_courses_dir(), recursive=TRUE)){
    swirl_out(s()%N%"Course installed successfully!", skip_after=TRUE)
  } else {
    swirl_out(s()%N%"Course installation failed.", skip_after=TRUE)
  }
  
  invisible()
}

#' Install a course from a GitHub repository
#' 
#' @param github_username The username that owns the course repository.
#' @param course_name The name of the repository which should be the name of the course.
#' @param branch The branch of the repository containing the course. The default branch is \code{"master"}.
#' @param multi The user should set to \code{TRUE} if the repository contains multiple courses. The default value is \code{FALSE}.
#' @export
#' @examples
#' \dontrun{
#' 
#' install_course_github("bcaffo", "Linear_Regression")
#' install_course_github("jtleek", "Twitter_Map", "geojson")
#' }
#' @family InstallCourses
install_course_github <- function(github_username, course_name, 
                                  branch="master", multi=FALSE){
  
  # Construct url to the zip file
  zip_url <- paste0("http://github.com/", github_username, "/", 
                    course_name,"/zipball/", branch)

  install_course_url(zip_url, multi=multi)
}

#' Install a course from a zipped course directory shared on Dropbox
#' 
#' @param url URL of the shared file
#' @param multi The user should set to \code{TRUE} if the zipped directory contains multiple courses. The default value is \code{FALSE}.
#' @export
#' @examples
#' \dontrun{
#' 
#' install_course_dropbox("https://www.dropbox.com/s/xttkmuvu7hh72vu/my_course.zip")
#' }
#' @family InstallCourses
install_course_dropbox <- function(url, multi=FALSE){
  # Construct url to the zip file
  zip_url <- paste0(sub("www.dropbox", "dl.dropboxusercontent", url), "?dl=1")
  
  install_course_url(zip_url, multi=multi)
}

#' Install a course from a zipped course directory shared on Google Drive
#' 
#' @param url URL of the shared file
#' @param multi The user should set to \code{TRUE} if the zipped directory contains multiple courses. The default value is \code{FALSE}.
#' @export
#' @examples
#' \dontrun{
#' 
#' install_course_google_drive("https://drive.google.com/file/d/F3fveiu873hfjZZj/edit?usp=sharing")
#' }
#' @family InstallCourses
install_course_google_drive <- function(url, multi=FALSE){
  # Construct url to the zip file
  zip_url <- sub("file/d/", "uc?export=download&id=", 
                 sub("/edit\\?usp=sharing", "", url))
  
  install_course_url(zip_url, multi=multi)
}

#' Install a course from a url that points to a zip file
#' 
#' @param url URL that points to a zipped course directory
#' @param multi The user should set to \code{TRUE} if the zipped directory contains multiple courses. The default value is \code{FALSE}.
#' @export
#' @importFrom httr GET content progress
#' @importFrom stringr str_extract
#' @examples
#' \dontrun{
#' 
#' install_course_url("http://www.biostat.jhsph.edu/~rpeng/File_Hash_Course.zip")
#' }
#' @family InstallCourses
install_course_url <- function(url, multi=FALSE){
  # Send GET request
  response <- GET(url, progress())
  
  # Construct path to Courses
  path <- file.path(swirl_courses_dir(), "temp.zip")
  
  # Write the response as a zip
  writeBin(content(response, "raw"), path)
  
  # Unzip downloaded file
  install_course_zip(path, multi=multi)

  # Clean up GitHub directory name
  if(grepl("github.com", url) && !multi){
    # Get paths of every file in zip that will be extracted
    file_names <- dirname(unzip(path, list = TRUE)$Name)
    
    # Find subset of those names which is not equal to root, then get the shortest string from that subset
    old_name <- head( sort( file_names[which(file_names != ".")] ) , 1)
    
    # Extract course name
    course_name <- sub("/zipball", "", 
                       str_extract(url, "[^/]+/{1}zipball") )
    
    # Rename unzipped directory
    file.rename(file.path(swirl_courses_dir(), old_name), 
                file.path(swirl_courses_dir(), course_name))
  }
  
  # Delete downloaded zip
  unlink(path, force=TRUE)
  
  invisible()
}

unpack_course <- function(file_path, export_path, force = FALSE){
  # Remove trailing slash
  export_path <- sub(paste0(.Platform$file.sep, "$"), replacement = "", export_path)
  
  pack <- readRDS(file_path)
  course_path <- file.path(export_path, pack$name)
  if(!force && file.exists(course_path) && interactive()){
    response <- ""
    while(response != "Y"){
      response <- select.list(c("Y", "n"), title = paste("\n\n", course_path, "already exists.\nAre you sure you want to overwrite it? [Y/n]"))
      if(response == "n") return(invisible(course_path))
    }
  }
  dir.create(course_path, showWarnings = FALSE)
  for(i in 1:length(pack$files)){
    
    # Make file's ultimate path
    if(length(pack$files[[i]]$path) >= 2){
      lesson_file_path <- Reduce(function(x, y){file.path(x, y)}, pack$files[[i]]$path[2:length(pack$files[[i]]$path)], pack$files[[i]]$path[1])
    } else {
      lesson_file_path <- pack$files[[i]]$path
    }
    file_path <- file.path(course_path, lesson_file_path)
    
    # If the directory the file needs to be in does not exist, create the dir
    if(!file.exists(dirname(file_path))){
      dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)
    }
    
    writeBin(pack$files[[i]]$raw_file, file_path, endian = pack$files[[i]]$endian)
  }
  swirl_out(s()%N%"Course installed successfully!", skip_after=TRUE)
  invisible(course_path)
}
