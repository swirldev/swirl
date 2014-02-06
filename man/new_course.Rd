\name{new_course}
\alias{new_course}
\title{Create a new swirl course}
\usage{
  new_course(course_name, open_doc = TRUE, init_git = TRUE)
}
\arguments{
  \item{course_name}{name of the new course}

  \item{open_doc}{opens the Rmd file to be edited if True}

  \item{init_git}{initializes Git reposity if True}
}
\description{
  This function creates a new directory encompassing a
  swirl course, initializes it as a git repo, creates a
  subdirectory for the first module, and opens module1.Rmd
  for users to edit.
}

