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
  
* Course authors can add custom tests for student repsonses.
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