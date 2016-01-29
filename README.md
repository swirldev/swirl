# swirl

[![Build Status](https://travis-ci.org/swirldev/swirl.png?branch=master)](https://travis-ci.org/swirldev/swirl)
[![Downloads](http://cranlogs.r-pkg.org/badges/swirl?color=3399ff)](http://cran-logs.rstudio.com/)

### [http://swirlstats.com](http://swirlstats.com)

swirl is a platform for learning (and teaching) statistics and R simultaneously and interactively. It presents a choice of course lessons and interactively tutors a user through them. A user may be asked to watch a video, to answer a multiple-choice or fill-in-the-blanks question, or to enter a command in the R console precisely as if he or she were using R in practice. Emphasis is on the last, interacting with the R console. User responses are tested for correctness and hints are given if appropriate. Progress is automatically saved so that a user may quit at any time and later resume without losing work.

swirl leans heavily on exercising a student's use of the R console. A callback mechanism, suggested and first demonstrated for the purpose by Hadley Wickham, is used to capture student input and to provide immediate feedback relevant to the course material at hand.

[swirlify](https://github.com/swirldev/swirlify) is a separate R package that provides a comprehensive toolbox for swirl instructors. Content is authored in [YAML](http://en.wikipedia.org/wiki/YAML) using the handy tools described on the [instructors page](http://swirlstats.com/instructors.html) of our website.

The program is initiated with `swirl()`. Functions which control swirl's behavior include `bye()` to quit, `skip()` to skip a question, `main()` to return to the main menu, `play()` to allow experimentation in the R console without interference from swirl, `nxt()` to resume interacting with swirl, and `info()` to display a help menu.


## Installing swirl (from CRAN)

The easiest way to install and run swirl is by typing the following from the R console:

```
install.packages("swirl")
library(swirl)
swirl()
```

As we continue adding new features and content, we will make new versions available on CRAN as appropriate (every 1-2 months, most likely).

## Installing the latest development version (from GitHub)

To access the most recent features and content, you can install and run the development version of swirl using the [devtools](https://github.com/hadley/devtools) package:

```
install.packages("devtools")
devtools::install_github("swirldev/swirl")
library(swirl)
swirl()
```

## Contributing to swirl's development

If you'd like to get involved, please fork this repository and submit a pull request with your proposed changes. We're happy to chat if you have any questions about the source code.

## Using swirl in the classroom

Instructors around the world are using swirl in their classrooms. We think this is awesome. If you're an instructor, please feel free to do the same -- free of charge. While your students may be paying to take your course or attend your institution, we simply ask that you don't charge people *directly* for the use of our software or instructional content.

If you are not sure about a particular use case, don't hesitate to send us an email at info@swirlstats.com.
