\name{author_lesson}
\alias{author_lesson}
\title{Author a swirl lesson.}
\usage{
author_lesson(lesson_name, course_name)
}
\arguments{
  \item{lesson_name}{Full name of the lesson being
  authored}

  \item{course_name}{Full name of the course to which this
  lesson belongs}
}
\description{
Takes full names of lesson and course as input, creates the
appropriate subdirectories in the swirl package directory,
then opens a lesson template in an R Markdown file for the
instructor to begin editing.
}
\examples{
\dontrun{

author_lesson("Example Lesson Name", "Example Course Name")
}
}

