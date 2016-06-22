# The versions of the functions below have been graciously borrowed
# from version 0.11.0 of the testthat package by 
# Hadley Wickham and others at RStudio. These APIs
# were broken in later versions of testthat and we know the
# old version works for our purposes.

expectation_legacy <- function(passed, failure_msg, 
                            success_msg = "unknown", 
                            srcref = NULL) {
  structure(
    list(
      passed = passed,
      error = FALSE,
      skipped = FALSE,
      failure_msg = failure_msg,
      success_msg = success_msg,
      srcref = srcref
    ),
    class = "expectation"
  )
}

#' @importFrom testthat compare
equals_legacy <- function(expected, label = NULL, ...) {
  if (is.null(label)) {
    label <- findExpr("expected")
  } else if (!is.character(label) || length(label) != 1) {
    label <- deparse(label)
  }
  
  function(actual) {
    same <- compare(actual, expected, ...)
    
    expectation_legacy(
      same$equal,
      paste0("not equal to ", label, "\n", same$message),
      paste0("equals ", label)
    )
  }
}

is_a_legacy <- function(class) {
  function(x) {
    actual_s <- paste0(class(x), collapse = ", ")
    class_s <- paste(class, collapse = ", ")
    expectation_legacy(
      inherits(x, class),
      paste0("inherits from ", actual_s, " not ", class_s),
      paste0("inherits from ", class_s)
    )
  }
}

is_equivalent_to_legacy <- function(expected, label = NULL) {
  if (is.null(label)) {
    label <- findExpr("expected")
  } else if (!is.character(label) || length(label) != 1) {
    label <- deparse(label)
  }
  function(actual) {
    equals_legacy(expected, check.attributes = FALSE)(actual)
  }
}

is_identical_to_legacy <- function(expected, label = NULL) {
  if (is.null(label)) {
    label <- findExpr("expected")
  } else if (!is.character(label) || length(label) != 1) {
    label <- deparse(label)
  }
  
  function(actual) {
    if (identical(actual, expected)) {
      diff <- ""
    } else {
      same <- all.equal(expected, actual)
      if (isTRUE(same)) {
        diff <- "Objects equal but not identical"
      } else {
        diff <- paste0(same, collapse = "\n")
      }
    }
    
    expectation_legacy(
      identical(actual, expected),
      paste0("is not identical to ", label, ". Differences: \n", diff),
      paste0("is identical to ", label)
    )
  }
}

matches_legacy <- function(regexp, all = TRUE, ...) {
  stopifnot(is.character(regexp), length(regexp) == 1)
  function(char) {
    matches <- grepl(regexp, char, ...)
    if (length(char) > 1) {
      values <- paste0("Actual values:\n",
                       paste0("* ", encodeString(char), collapse = "\n"))
    } else {
      values <- paste0("Actual value: \"", encodeString(char), "\"")
    }
    
    expectation_legacy(
      length(matches) > 0 && if (all) all(matches) else any(matches),
      paste0("does not match '", encodeString(regexp), "'. ", values),
      paste0("matches '", encodeString(regexp), "'")
    )
  }
}