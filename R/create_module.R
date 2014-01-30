#' Takes full name of module and course as input, creates the appropriate course
#' and module subdirectories in the swirl package directory, then opens an
#' R Markdown file for the instructor to begin editing.
create_module = function(module_name, course_name, ...) {
  
  # Convert module name and course name to more desirable file path formats.
  mod_filename <- make_pathname(module_name)
  crs_dirname <- make_pathname(course_name)
  
  # Create directory for course in current working directory.
  path2course <- file.path(getwd(), "my_swirl_courses", crs_dirname)
  path2mod <- file.path(path2course, mod_filename, paste0(mod_filename, ".Rmd"))
  
  # Copy course template into course directory.
  message("Preparing module template ...\n")
  scaffold <- system.file("skeleton", package = "swirl")
  copy_dir(scaffold, file.path(path2course, mod_filename))
  
  # If successful, then rename file appropriately.
  if(file.exists(file.path(path2course, mod_filename, "index.Rmd"))) {
    file.rename(file.path(path2course, mod_filename, "index.Rmd"), path2mod)
  }
  
  # Open R Markdown file for new module.
  message("Opening module (", mod_filename, ".Rmd) for editing ...\n")
  file.edit(path2mod)
  
  invisible()
}

## UTILS

# Takes a plain English name and turns it into a more proper file/directory name.
make_pathname <- function(name) {
  gsub(" ", "_", str_trim(name))
}

# Copies the contents of one directory to another. Copied directly from slidify.
copy_dir <- function(from, to){
  if (!(file.exists(to))){
    dir.create(to, recursive = TRUE)
    message('Copying file(s) to ', to, ' ...\n')
    file.copy(list.files(from, full.names = T), to, recursive = TRUE)
  }
}
