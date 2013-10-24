demo callback use for command answer types
==========

A demo illustrating the use of addTaskCallback for questions with command answer type. In this illustrative case the user answers from the R prompt rather than in response to readline() or select.list(). Output types "range" and "exact" *could* be handled from the R prompt, and probably should be, but for clarity and simplicity are not. These and other output and answer types are handled by pared-down versions of swirl 1.0 code. 

The demo is limited to Module 2 of Data Analysis, represented as a csv file with a few manual alterations. Features such as support for multiple users, or tracking user progress are not implemented.

An annotated version of Hadley's frndly.R code is included for reference. To run either, source the relevant file, type "hi()" to begin, "nxt()" to advance, and "bye()" to quit.
