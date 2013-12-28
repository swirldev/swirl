#' Hadley's expect_that, annotated
#' > expect_that
#' function (object, condition, info = NULL, label = NULL) 
#' {
#'   # if label is NULL give it the name of the first argument
#'   # in the calling environment of this function
#'   if (is.null(label)) {
#'     label <- find_expr("object")
#'   }
#'   # apply the condition to object
#'   results <- condition(object)
#'   # prepend the label to the results message
#'   results$message <- str_c(label, " ", results$message)
#'   # post-pend info to message after line break
#'   if (!is.null(info)) {
#'     results$message <- str_c(results$message, "\n", info)
#'   }
#'   # add the result to the current reporter
#'   test_reporter()$add_result(results)
#'   invisible()
#' }
#' <environment: namespace:testthat>
#' > testthat:::find_expr
#' function (name, env = parent.frame()) 
#' {
#'   subs <- do.call("substitute", list(as.name(name), env))
#'   str_c(deparse(subs, width.cutoff = 500), collapse = "\n")
#' }
#' 