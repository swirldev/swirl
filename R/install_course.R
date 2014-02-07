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
#' install_course_directory("~/Desktop/my_course")
install_course_directory <- function(path){
  # Copy files
  file.copy(path, file.path(path.package("swirl"), "Courses"), recursive=T)
  
  # Compile course
  suppressWarnings(invisible(sapply(
    list.files(file.path(path.package("swirl"), "Courses", basename(path)), recursive=T, full.names=T),
    compile_csv)))
}

#' Install a course from a GitHub repository
#' 
#' @param github.username The username that owns the course repository.
#' @param course.name The name of the repository which should be the name of the course.
#' @param branch The branch of the repository containing the course. The default branch is \code{"master"}.
#' @export
#' @examples
#' install_course_github("bcaffo", "Linear_Regression")
#' install_course_github("jtleek", "Twitter_Map", "geojson")
install_course_github <- function(github.username, course.name, branch="master"){
  
  # Construct url to the zip file
  zip_url <- paste0("http://github.com/", github.username, "/", course.name,"/zipball/", branch)

  install_course_url(zip_url, type="github", course.name=course.name)
}

#' Install a course from a zipped course directory shared on Dropbox
#' 
#' @param url URL of the shared file
#' @export
#' @examples
#' install_course_dropbox("https://www.dropbox.com/s/xttkmuvu7hh72vu/my_course.zip")
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
#' install_course_google_drive("https://drive.google.com/file/d/F3fveiu873hfjZZj/edit?usp=sharing")
install_course_google_drive <- function(url){
  # Construct url to the zip file
  zip_url <- sub("file/d/", "uc?export=download&id=", sub("/edit\\?usp=sharing", "", url))
  
  install_course_url(zip_url)
}

#' Install a course from a url that points to a zip file
#' 
#' @param url URL that points to a zipped course directory
#' @export
#' @importFrom httr GET content
#' @examples
#' install_course_url("http://www.biostat.jhsph.edu/~rpeng/File_Hash_Course.zip")
install_course_url <- function(url, type="url", course.name=""){
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
                file.path(path.package("swirl"), "Courses", course.name))
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