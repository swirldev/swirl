#' Create a new swirl course
#' 
#' This function creates a new directory encompassing a swirl course, initializes it as a git repo,
#' creates a subdirectory for the first module, and opens module1.Rmd for users to edit.
#' @param course_name name of the new course
#' @param open_doc opens the Rmd file to be edited if True
#' @param init_git initializes Git reposity if True
#' @export
new_course <- function(course_name, open_doc = TRUE, init_git = TRUE){
  message('Creating course ', course_name, '...')
  #   if (file.exists(deckdir)){
  #     return('Course already exists. Please choose a different name.')
  #   }
  copy_dir(scaffold, deckdir)
  message('Finished creating slide directory...')
  message('Switching to slide directory...')
  setwd(deckdir)
  if (use_git && Sys.which('git') != ""){
    init_repo()
  }
  if (open_rmd){
    message('Opening slide deck...')
    file.edit('index.Rmd')
  }
}

#' Initialize a git repository, create and switch to gh-pages branch.
#' 
#' @keywords internal
#' @noRd
init_repo <- function(){
  message('Initializing Git Repo')
  system("git init")
  system("git commit --allow-empty -m 'Initial Commit'")
  message("Checking out gh-pages branch...")
  system('git checkout -b gh-pages')
  message('Adding .nojekyll to repo')
  file.create('.nojekyll')
}