\name{install_course_url}
\alias{install_course_url}
\title{Install a course from a url that points to a zip file}
\usage{
install_course_url(url, course_name, type = "url")
}
\arguments{
  \item{url}{URL that points to a zipped course directory}

  \item{course_name}{name of course}

  \item{type}{optional parameter, either \code{"url"} or
  \code{"github"}, specifying whether download is from an
  arbitrary URL or a GitHub repository, respectively.
  Default is \code{"url"}.}
}
\description{
Install a course from a url that points to a zip file
}
\examples{
\dontrun{

install_course_url("http://www.biostat.jhsph.edu/~rpeng/File_Hash_Course.zip")
}
}

