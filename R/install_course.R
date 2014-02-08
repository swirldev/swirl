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
  path <- file.path(path.package("swirl"), "Courses", make_pathname(course_name))
  if(file.exists(path)){
    unlink(path, recursive=T, force=T)
  }
}

#' Install a course from a zipped course folder
#' 
#' @param path The path to the zipped course.
#' @export
#' @examples
#' install_course_zip("~/Desktop/my_course.zip")
install_course_zip <- function(path){
  # Unzip file into courses
  file_list <- unzip(path, exdir=file.path(path.package("swirl"), "Courses"))
  
  # If __MACOSX exists, delete it.
  unlink(file.path(path.package("swirl"), "Courses", "__MACOSX"), recursive=T, force=T)
  
  # Compile course
  suppressWarnings(invisible(sapply(file_list, compile_csv)))
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
    {setTimeLimit(elapsed=1); list.files(path, recursive=T)},
    finally = {setTimeLimit(elapsed=Inf)}
  )
  
  # Check to make sure there are fewer than 1000 files in course directory
  if(length(garbage_result) > 1000){
    stop("Course directory is too large to install")
  }
  
  # Copy files
  file.copy(path, file.path(path.package("swirl"), "Courses"), recursive=T)
  
  # Compile course
  suppressWarnings(invisible(sapply(
    list.files(file.path(path.package("swirl"), "Courses", basename(path)), recursive=T, full.names=T),
    compile_csv)))
}

#' Install a course from a GitHub repository
#' 
#' @param github_username The username that owns the course repository.
#' @param course_name The name of the repository which should be the name of the course.
#' @param branch The branch of the repository containing the course. The default branch is \code{"master"}.
#' @export
#' @examples
#' \dontrun{
#' 
#' install_course_github("bcaffo", "Linear_Regression")
#' install_course_github("jtleek", "Twitter_Map", "geojson")
#' }
install_course_github <- function(github_username, course_name, branch="master"){
  
  # Construct url to the zip file
  zip_url <- paste0("http://github.com/", github_username, "/", course_name,"/zipball/", branch)

  install_course_url(zip_url, type="github", course_name=course_name)
}

#' Install a course from a zipped course directory shared on Dropbox
#' 
#' @param url URL of the shared file
#' @export
#' @examples
#' \dontrun{
#' 
#' install_course_dropbox("https://www.dropbox.com/s/xttkmuvu7hh72vu/my_course.zip")
#' }
install_course_dropbox <- function(url){
  # Construct url to the zip file
  zip_url <- paste0(sub("www.dropbox", "dl.dropboxusercontent", url), "?dl=1")
  
  install_course_url(zip_url)
}

#' Install a course from a zipped course directory shared on Google Drive
#' 
#' @param url URL of the shared file
#' @export
#' @examples
#' \dontrun{
#' 
#' install_course_google_drive("https://drive.google.com/file/d/F3fveiu873hfjZZj/edit?usp=sharing")
#' }
install_course_google_drive <- function(url){
  # Construct url to the zip file
  zip_url <- sub("file/d/", "uc?export=download&id=", sub("/edit\\?usp=sharing", "", url))
  
  install_course_url(zip_url)
}

#' Install a course from a url that points to a zip file
#' 
#' @param url URL that points to a zipped course directory
#' @param course_name name of course
#' @param type optional parameter, either \code{"url"} or \code{"github"}, specifying whether download is from an arbitrary URL or a GitHub repository, respectively. Default is \code{"url"}.
#' @export
#' @importFrom httr GET content
#' @examples
#' \dontrun{
#' 
#' install_course_url("http://www.biostat.jhsph.edu/~rpeng/File_Hash_Course.zip")
#' }
install_course_url <- function(url, course_name, type="url"){
  # Send GET request
  response <- GET(url)
  
  # Construct path to Courses
  path <- file.path(path.package("swirl"), "Courses", "temp.zip")
  
  # Write the response as a zip
  writeBin(content(response, "raw"), path)
  
  # Unzip downloaded file
  install_course_zip(path)

  # Clean up GitHub directory name
  if(type=="github"){
    # Get paths of every file in zip that will be extracted
    file_names <- dirname(unzip(path,  list = T)$Name)
    
    # Find subset of those names which is not equal to root, then get the shortest string from that subset
    old_name <- head( sort( file_names[which(file_names != ".")] ) , 1)
    
    # Rename unzipped directory
    file.rename(file.path(path.package("swirl"), "Courses", old_name), 
                file.path(path.package("swirl"), "Courses", course_name))
  }
  
  # Delete downloaded zip
  unlink(path, force=T)
}

# Converts .Rmd to .csv so swirl can interpret content
compile_csv <- function(path){
  if(grepl(".[r|R][m|M][d|D]$", path)){
    rmd2csv(path, sub(".[r|R][m|M][d|D]$", ".csv", path))
  }
}