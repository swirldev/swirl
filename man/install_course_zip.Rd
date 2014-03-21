\name{install_course_zip}
\alias{install_course_zip}
\title{Install a course from a zipped course folder}
\usage{
install_course_zip(path, multi = FALSE)
}
\arguments{
  \item{path}{The path to the zipped course.}

  \item{multi}{Set to \code{TRUE} if the zipped directory
  contains multiple courses. The default value is
  \code{FALSE}.}
}
\description{
Install a course from a zipped course folder
}
\examples{
install_course_zip("~/Desktop/my_course.zip")
}

