# swirl 2.0

* Uses `addTaskCallback()` as a mechanism to capture user input directly from the R prompt.

* During instruction, `info()` brings up a menu of options including `bye()`, `skip()`, `play()`, and `nxt()`.

* `skip()` allows the user to skip the current question. swirl automatically evaluates the correct answer in the user's workspace in case future questions depend on the result.

* Includes a library of answer tests based on [testthat](https://github.com/hadley/testthat), an R package designed by Hadley Wickham for unit testing.

* Tests user responses for correctness based various combinations of the aforementioned answer tests. A user is judged to have answered a question correctly when the answer tests specified for that question are satisfied.

* The answer tests operate on the R expression entered by the user, as opposed to the string representation of it (see swirl 1.0). This avoids marking a user incorrect for stylistic discrepancies such as including single spaces between function arguments, etc.

* Instructors can now author content in an R Markdown (.Rmd) file, then use `rmd2csv()` to create the CSV file from which swirl reads content. This is an experimental feature based on syntax employed by [slidify](https://github.com/ramnathv/slidify), an R package designed by Ramnath Vaidyanathan for creating interactive web presentations.