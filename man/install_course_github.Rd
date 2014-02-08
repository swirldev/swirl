\name{install_course_github}
\alias{install_course_github}
\title{Install a course from a GitHub repository}
\usage{
install_course_github(github_username, course_name, branch = "master")
}
\arguments{
  \item{github_username}{The username that owns the course
  repository.}

  \item{course_name}{The name of the repository which
  should be the name of the course.}

  \item{branch}{The branch of the repository containing the
  course. The default branch is \code{"master"}.}
}
\description{
Install a course from a GitHub repository
}
\examples{
\dontrun{

install_course_github("bcaffo", "Linear_Regression")
install_course_github("jtleek", "Twitter_Map", "geojson")
}
}

