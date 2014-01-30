# Install a course from a zipped course folder

install_course_zip <- function(path){
  # Unzip file into courses
  unzip(path, exdir=file.path(path.package("swirl"), "Courses"))
}

# Install a course from a directory which contains modules

install_course_directory <- function(path){
  # Copy files
  file.copy(path, file.path(path.package("swirl"), "Courses"), recursive=T)
}

# Must use the httr package

# Install a course from a github repository

install_course_github <- function(github.username, course.name, branch="master"){
  
  # Construct url to the zip file
  zip_url <- paste0("http://github.com/", github.username, "/", course.name,"/zipball/", branch)
  
  # Construct path to Courses
  path <- file.path(path.package("swirl"), "Courses", paste0(course.name, ".zip"))
  
  # Get http response
  response <- GET(zip_url)
  
  # Write the response as a zip
  writeBin(content(response, "raw"), path)
  
  # The name of the directory that is extracted from the zip is messy
  # I concocted this way of giving the directory the same name as course.name
  
  # Get paths of every file in zip that will be extracted
  file_names <- dirname(unzip(path,  list = T)$Name)
  
  # Find subset of those names which is not equal to root, then get the shortest string from that subset
  old_name <- head( sort( file_names[which(file_names != ".")] ) , 1)
  
  # Unzip downloaded file
  unzip(path,  exdir=file.path(path.package("swirl"), "Courses"))
  
  # Rename unzipped directory
  file.rename(file.path(path.package("swirl"), "Courses", old_name), 
              file.path(path.package("swirl"), "Courses", course.name))
  
  # Delete downloaded zip
  unlink(path)
}

# install_course_github("seankross", "test_course")

# Install a course from a url that points to a zip file
#This needs to be tested

install_course_url <- function(url){
  # Send GET request
  response <- GET(url)
  
  # Write the response as a zip
  writeBin(content(response, "raw"), path)
  
  # Unzip downloaded file
  unzip(path,  exdir=file.path(path.package("swirl"), "Courses"))
  
  # Delete downloaded zip
  unlink(path)
}