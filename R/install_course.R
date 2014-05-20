#' Install a course from the official course repository
#' 
#' We are currently maintaining a central repository of contributed
#' swirl courses at \url{https://github.com/swirldev/swirl_courses}.
#' This function provides the easiest method of installing a course
#' form the repository.
#' 
#' @param course_name The name of the course you wish to install.
#' @export
#' @importFrom httr GET content
#' @examples
#' \dontrun{
#' 
#' install_from_swirl("R_Programming") # Directory name
#' 
#' ### OR ###
#' 
#' install_from_swirl("R Programming") # Course name
#' }
install_from_swirl <- function(course_name){
  # make pathname from course_name
  course_name <- make_pathname(course_name)
  
  # Construct url to the zip file
  url <- "http://github.com/swirldev/swirl_courses/zipball/master"
  
  # Send GET request
  response <- GET(url)
  
  # Construct path to Courses
  path <- file.path(system.file("Courses", package = "swirl"), "temp.zip")
  
  # Write the response as a zip
  writeBin(content(response, "raw"), path)
  
  # Find list of files not in top level directory
  file_names <- unzip(path, list=TRUE)$Name
  
  # Filter list
  unzip_list <- Filter(function(x){grepl(course_name, x)}, file_names)
  
  # Check if course exists
  if(length(unzip_list) == 0) {
    stop(paste0("Course '", course_name, "' not found in course repository! ",
                "Make sure you've got the name exactly right, then try again."))
  }
  
  # Extract
  unzip(path, exdir=file.path(system.file(package = "swirl"), "Courses"), 
        files=unzip_list)
  
  # Copy files from unzipped directory into Courses
  top_dir <- file.path(system.file(package = "swirl"), "Courses", 
                       sort(dirname(unzip_list))[1])
  dirs_to_copy <- list.files(top_dir, full.names=TRUE)
  if(file.copy(dirs_to_copy, file.path(system.file(package = "swirl"), "Courses"),
            recursive=TRUE)){
    swirl_out("Course installed successfully!", skip_after=TRUE)
  } else {
    swirl_out("Course installation failed.", skip_after=TRUE)
  }
  
  # Delete unzipped directory
  unlink(top_dir, recursive=TRUE, force=TRUE)
  
  # If __MACOSX exists, delete it.
  unlink(file.path(system.file(package = "swirl"), "Courses", "__MACOSX"),
         recursive=TRUE, force=TRUE)
  
  # Delete temp.zip
  unlink(path, force=TRUE)
  
  invisible()
}


#' Zip a course directory
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
zip_course <- function(path, dest=NULL){
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
uninstall_course <- function(course_name){
  path <- file.path(system.file(package = "swirl"), "Courses", 
                    make_pathname(course_name))
  if(file.exists(path)){
    unlink(path, recursive=TRUE, force=TRUE)
    message("Course uninstalled successfully!")
  } else {
    stop("Course not found!")
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
install_course_zip <- function(path, multi=FALSE, which_course=NULL){
  if(!is.logical(multi) || is.na(multi)) {
    stop("Argument 'multi' must be either TRUE or FALSE.")
  }
  if(!multi && !is.null(which_course)) {
    stop("Argument 'which_course' should only be specified when argument 'multi' is TRUE.")
  }
  if(multi){
    # Find list of files not in top level directory
    file_names <- unzip(path, list=TRUE)$Name
    
    # Filter list and extract
    unzip_list <- Filter(function(x){grepl("/.+/", x)}, file_names)
    unzip(path, exdir=file.path(system.file(package = "swirl"), "Courses"),
          files=unzip_list)
    
    # Copy files from unzipped directory into Courses
    top_dir <- file.path(system.file(package = "swirl"), "Courses", 
                         sort(dirname(unzip_list))[1])
    dirs_to_copy <- list.files(top_dir, full.names=TRUE)
    # Subset desired courses if specified with which_courses arg
    if(!is.null(which_course)) {
      match_ind <- match(make_pathname(which_course), basename(dirs_to_copy),
                   nomatch=-1)
      nomatch <- match_ind < 0
      if(any(nomatch)) {
        stop("Course ", sQuote(which_course[nomatch][1]), " not in specified directory. Be careful, course names are case sensitive!")
      }
      dirs_to_copy <- dirs_to_copy[match_ind]
    }
    if(file.copy(dirs_to_copy, file.path(system.file(package = "swirl"),
                                      "Courses"), recursive=TRUE)){
      swirl_out("Course installed successfully!", skip_after=TRUE)
    } else {
      swirl_out("Course installation failed.", skip_after=TRUE)
    }
    
    # Delete unzipped directory
    unlink(top_dir, recursive=TRUE, force=TRUE)
    
  } else {
    # Unzip file into courses
    file_list <- unzip(path, exdir=file.path(system.file(package = "swirl"),
                                             "Courses"))
  }
  
  # If __MACOSX exists, delete it.
  unlink(file.path(system.file(package = "swirl"), "Courses", "__MACOSX"),
         recursive=TRUE, force=TRUE)

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
install_course_directory <- function(path){
  # Check for size of directory to prevent copying a huge directory into swirl/Courses
  garbage_result <- tryCatch(
    {setTimeLimit(elapsed=1); list.files(path, recursive=TRUE)},
    finally = {setTimeLimit(elapsed=Inf)}
  )
  
  # Check to make sure there are fewer than 1000 files in course directory
  if(length(garbage_result) > 1000){
    stop("Course directory is too large to install")
  }
  
  # Copy files
  if(file.copy(path, file.path(system.file(package = "swirl"), "Courses"),
            recursive=TRUE)){
    swirl_out("Course installed successfully!", skip_after=TRUE)
  } else {
    swirl_out("Course installation failed.", skip_after=TRUE)
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
#' @importFrom httr GET content
#' @importFrom stringr str_extract perl
#' @examples
#' \dontrun{
#' 
#' install_course_url("http://www.biostat.jhsph.edu/~rpeng/File_Hash_Course.zip")
#' }
install_course_url <- function(url, multi=FALSE){
  # Send GET request
  response <- GET(url)
  
  # Construct path to Courses
  path <- file.path(system.file(package = "swirl"), "Courses", "temp.zip")
  
  # Write the response as a zip
  writeBin(content(response, "raw"), path)
  
  # Unzip downloaded file
  install_course_zip(path, multi=multi)

  # Clean up GitHub directory name
  if(grepl("github.com", url) && !multi){
    # Get paths of every file in zip that will be extracted
    file_names <- dirname(unzip(path,  list = TRUE)$Name)
    
    # Find subset of those names which is not equal to root, then get the shortest string from that subset
    old_name <- head( sort( file_names[which(file_names != ".")] ) , 1)
    
    # Extract course name
    course_name <- sub("/zipball", "", 
                       str_extract(url, perl("[^/]+/{1}zipball")) )
    
    # Rename unzipped directory
    file.rename(file.path(system.file(package = "swirl"), 
                          "Courses", old_name), 
                file.path(system.file(package = "swirl"), 
                          "Courses", course_name))
  }
  
  # Delete downloaded zip
  unlink(path, force=TRUE)
  
  invisible()
}
