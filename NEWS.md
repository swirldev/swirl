# swirl 2.4.5

* Thank you @HenrikBengtsson for fixing a warning when 
`warnPartialMatchArgs=TRUE`. (#779)

# swirl 2.4.4

* Fixed encoding test which was failing on CRAN (debian-clang-devel).

# swirl 2.4.3

* Added "swirl_is_fun" option to `swirl_options()`.

* Added Portuguese menu translations.

# swirl 2.4.2

* Script questions behave more appropriately in RStudio. (#434, thank you @jimhester)

# swirl 2.4.1

* Added new answer test: `calculates_same_value()`.

* Now compatible with versions of testthat later than 0.11.0.

# swirl 2.4.0

* Added support for multiple languages, including Spanish, French, German,
Turkish, Simplified Chinese, and Korean. The default language can be changed
using the function `select_language()`.

* Added `install_course()` in order to install swirl courses that are
distributed in the .swc format.

* The directories where swirl courses and where user data is stored can now be
explicitly specified. These options can be set using the function 
`swirl_options()`.

* It's now possible to log and collect a student's progression
through a swirl course. Enable logging with `swirl_options()`.

* Improved support for displaying non-ASCII character sets
through UTF-8 encoding.

* Now compatible with `swirlify::demo_lesson()`.

# swirl 2.3.1

* Add progress bar to track download progress of a course.

# swirl 2.3.0

* Add basic developer API to swirl, courtesy of @filipsch and @seankross.

* Change license to MIT, which is slightly less restrictive than GPL-3 and will make it easy for developers to tap into the new API.

# swirl 2.2.21

* Add `mirror` argument to `install_from_swirl()` to accommodate installing courses from the Bitbucket mirror of our swirl-courses GitHub repository. (Prompted by India's blocking of GitHub.)

* Check for existence of variable in swirl.R to address issue with using `rm()` command.

# swirl 2.2.20

* Fix troublesome bug in `omnitest` due to typo (thanks to @reginaastri).

# swirl 2.2.19

* Add `uninstall_all_courses` function.

# swirl 2.2.18

* Fix small bug in `omnitest` due to missing exclamation point (thanks to @wilcrofter).

* Add `delete_progress` function (thanks to @seankross).

# swirl 2.2.17

* Use of `partner.coursera.org` websites for Coursera submission is enabled. 

* `Omnitest` uses `rmatch_calls` (recursive `match.call`) to deal with legitimate variations of function and S3 method calls.

# swirl 2.2.16

* Fix bug in `install_from_swirl()` that was causing `install_from_swirl("R Programming")` to install both `R Programming` and `R Programming Alt`.

* Fix troublesome links to the swirl_courses repo.

# swirl 2.2.15

* Fix annoying typo in one of the "praise" messages.

# swirl 2.2.14

* Add `?InstallCourses`, which gives a brief overview of installing swirl courses and includes links to all relevant help files.

* Add `email_admin()` feature, which allows the user to automatically generate an email to be sent to `info@swirlstats.com`. The email includes space for a description of the problem along with the output from `sessionInfo()`.

* Clean up swirl's core function, `swirl()` in swirl.R, so that we can view the administrative environment `e` with `as.list(e)` without complaint from R.

* Add empty cran-comments.md to appease new devtools `release()`.

# swirl 2.2.13

* Add a new `script` question type, which allows an instructor to present a preformatted R script to the user for editing and submission. swirl was originally designed for interactive programming at the prompt. It now accommodates multiline input, which allows for exploration of topics like writing functions and control structures, as well as more extended function calls.

* Allow user to make swirl feedback less playful with `options(swirl_is_fun = FALSE)`.

# swirl 2.2.12

* In `welcome.default()`, make sure the user doesn't put any special characters (using the `[[:punct:]]` regex) in his or her name, as this might lead to an invalid file path for their user data. Thanks to @Sarpwus for bringing this to my attention.

* Trim leading/trailing whitespace when reading lesson dependencies from dependson.txt.

* Add `dev` argument to `install_from_swirl()` to allow installation of courses in development from the [swirl_misc](http://github.com/swirldev/swirl_misc/zipball/master) repository.

* Update phrases.R to include new praise and 'try again' phrases. Thanks to @sagevann for the suggestions.

* Use `capture.output()` to avoid double printing due to second evaluation by `safeEval()` when `AUTO_DETECT_NEWVAR` is `TRUE`.

# swirl 2.2.11

* Add `testit()` to functions for callback to ignore, so that swirl plays nicely with swirlify.

* Add from/to feature for testing specific units of content during course authoring.

# swirl 2.2.10

* Corrected order of `mergeList` arguments in `swirl.R`.

# swirl 2.2.9

* Add Regression Models to recommended courses.

# swirl 2.2.8

* Alternate user progress tracking strategy without previous lag or freeze problems. Backwards compatible with existing course content. Tracks large or small lesson data sets by default, but these may be excluded by sourcing with local=FALSE from the initLesson.R file. 

* Make course installation success/failure messages more robust.

* Add documentation for `?AnswerTests`.

* Create a file for manual submission to Coursera immediately after the user enters his or her credentials. This way, no matter what happens during the submission process, manual submission is still an option. The file is deleted if automatic submission succeeds.

* Add `any_of_exprs()` to library of answer tests.

# swirl 2.2.7

* Another check that Coursera challenge url is valid

# swirl 2.2.6

* Fix bug related to user entering their Coursera Course ID with quotes.

* Fix bug causing swirl to fail when exiting from course menu.

# swirl 2.2.5

* Add `packageStartupMessage()` that detects a cluttered workspace and warns the user that this may cause swirl to run slowly.

* Add `main()` function, which allows user to return to the main menu while a lesson is in progress.

* Add `which_course` argument to `install_course_zip()` that will facilitate manual installation. In particular, if a student downloads a zip file from the swirl courses repo, it comes with all courses in it. This function will allow the user to install only those that she wants.

# swirl 2.2.4

* Bug fix related to `skip()` count not resetting upon lesson completion.

# swirl 2.2.3

* Add confirmation step to Coursera submission process.

* Stash Course ID along with other Coursera creds.

* Display correct answer when user `skip()`s a question.

# swirl 2.2.2

* Fix bug in old answer test caused by upgrade to R 3.1.0 and made evident in the Data Analysis course.

# swirl 2.2.1

* Check for missing entries in content YAML to prevent failure when loading a course.

# swirl 2.2

* Instructional content is no longer shipped with swirl. Instead, it is located in our [course repo](https://github.com/swirldev/swirl_courses). When the user starts swirl, he or she is given the option to install the R Programming course automatically or be taken to the course repo page. Courses can also be installed with the `install_from_swirl()` function.

* Content authoring tools have also been removed from the swirl package. We've created a new package called [swirlify](https://github.com/swirldev/swirlify), which is a comprehensive toolbox for swirl instructors. Instructions for authoring content are on the [Instructors page](http://swirlstats.com/instructors.html) of the swirl website.

* Package dependencies for a lesson are now managing by including a file called `dependson.txt` in the lesson directory, which lists required packages one line at a time. This strategy is mainly for backwards compatibility and will take a different form for new content in future releases. When the user begins a lesson with package dependencies, swirl attempts to load each package in turn and prompts the user to automatically install any packages not found.

* Added help files for answer tests contained in `answerTests.R.`

* Added progress bar feature using `utils::txtProgressBar()`.

* Added `test` mode for compatibility with the [swirlify](https://github.com/swirldev/swirlify) package.

* Integrated with Coursera API to allow enrolled students to receive credit for swirl lessons associated their Coursera course.

* `rmd2df()` can finally handle `figure` and `video` units of instruction.

# swirl 2.1.1

* Fixed a bug in the third lesson of Intro to R.

# swirl 2.1

* `parse_content()` now parses content (at runtime) in its original form (R Markdown, YAML, etc.), making conversion to CSV files unnecessary. The appropriate parsing method is called based on the extension of the lesson file. Creating a new course authoring format is as simple as writing a new method for `parse_content()` that accepts the content as input and returns a properly structured `lesson` object.

* `author_lesson()` function creates and opens a customized lesson template for authoring content.

* Suite of functions for installing (and uninstalling) swirl courses:
  * `install_course_directory()`: Install a course from a course directory
  * `install_course_dropbox()`: Install a course from a zipped course directory shared on Dropbox
  * `install_course_github()`: Install a course from a GitHub repository
  * `install_course_google_drive()`: Install a course from a zipped course directory shared on Google Drive
  * `install_course_url()`:	Install a course from a url that points to a zip file
  * `install_course_zip()`:	Install a course from a zipped course folder
  * `uninstall_course()`: Uninstall a course
  * `zip_course()`: Zip a course directory
  
* Course authors can add custom tests for student responses.
  * Custom tests may be defined in the lesson directory in file named customTests.R.
  * Custom tests run in the same environment as tests provided with the package.
  
* Revised suite of answer tests (contained in answerTests2.R) using a more natural function call syntax.

* Revised user progress tracking and restoration.
  * Improved tracking by taking environmental "snapshots."
  * Keep a list of variables created or modified by correct responses.
  * Restore list to global environment after a lesson is suspended and resumed.
  
* Message notifying the user when she's completed a lesson, just prior to returning to the main menu.

* Miscellaneous big fixes

# swirl 2.0.1

* Fixed bug related to package dependencies (via imports)

# swirl 2.0

* Uses `addTaskCallback()` as a mechanism to capture user input directly from the R prompt.

* During instruction, `info()` brings up a menu of options including `bye()`, `skip()`, `play()`, and `nxt()`.

* `skip()` allows the user to skip the current question. swirl automatically evaluates the correct answer in the user's workspace in case future questions depend on the result.

* Includes a library of answer tests based on [testthat](https://github.com/hadley/testthat), an R package designed by Hadley Wickham for unit testing.

* Tests user responses for correctness based various combinations of the aforementioned answer tests. A user is judged to have answered a question correctly when the answer tests specified for that question are satisfied.

* The answer tests operate on the R expression entered by the user, as opposed to the string representation of it (see swirl 1.0). This avoids marking a user incorrect for stylistic discrepancies such as including single spaces between function arguments, etc.

* Makes heavy use of R's S3 object oriented programming dialect to promote an easily extensible architecture. Incorporating new functionality simply requires writing new methods for existing "core" functions.

* Instructors can now author content in an R Markdown (.Rmd) file, then use `rmd2csv()` to create the CSV file from which swirl reads content. This is an experimental feature based on syntax employed by [slidify](https://github.com/ramnathv/slidify), an R package designed by Ramnath Vaidyanathan for creating interactive web presentations.