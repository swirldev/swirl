\name{swirl}
\alias{swirl}
\title{An interactive learning environment for R and statistics}
\usage{
swirl(resume.class = "default")
}
\arguments{
  \item{resume.class}{for development only; please accept
  the default.}
}
\description{
This function presents a choice of course modules and
interactively tutors a user through them. A user may be
asked to watch a video, to answer a multiple-choice or
fill-in-the-blanks question, or to enter a command in the R
console precisely as if he or she were using R in practice.
Emphasis is on the last, interacting with the R console.
User responses are tested for correctness and hints are
given if appropriate. Progress is automatically saved so
that a user may quit at any time and later resume without
losing work.
}
\details{
The usual way to exit swirl is to type bye() in the R
console. A user may return to the R console at any time by
pressing the Esc key. The Esc key alone, however, does not
cause swirl to exit. A user must type bye() in addition.
Swirl will print a goodbye message whenever it exits to
signal that it is no longer in operation.

While swirl is in operation, it may be controlled by
entering special commands in the R console, using the Esc
key to return to the R console if necessary. One of the
special commands is bye() as discussed above. Others are
play(), nxt(), skip(), and info(). The parentheses are
important.

Sometimes a user will want to play around in the R console
without interference or commentary from swirl. This can be
accomplished by using the special command, play(). Swirl
will remain in operation, silently, until the special
command nxt() is entered. In general, nxt() can always be
used to display the upcoming question.

The special command, skip(), can be used to skip a question
if necessary. Swirl will enter the correct answer and
notify the user of the names of any new variables which it
may have created in doing so. These may be needed for
subsequent questions.

Finally, info() may be used to display a list of the
special commands themselves with brief explanations of what
they do.
}

