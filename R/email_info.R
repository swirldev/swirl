#' Send diagnostic email to swirl admin
#' 
#' Typing \code{email_admin()} at the prompt will attempt to open
#' a new email in your default browser or email client. The email
#' will include space for you to describe the problem you are
#' experiencing. It will also have the output from \code{sessionInfo},
#' which you should not alter.
#' 
#' @export
email_admin <- function() {
  # Get session info and swirl package version
  si <- capture.output(sessionInfo())
  pv <- packageVersion('swirl')
  
  # Set up email elements address, subject, and body
  address <- 'info@swirlstats.com'
  subject <- paste('Need help: swirl', pv)
  body <- paste('Brief description of problem: \n\n',
                paste(rep('#', 15),  collapse = ''),
                paste(si, collapse = '\n'),
                sep = '\n\n')

  # Send email
  email(address, subject, body)

  invisible()
}

# email() and email_browser() were copied directly from Hadley 
# Wickham's devtools package.

# http://tools.ietf.org/html/rfc2368
email <- function(address, subject, body) {
  url <- paste(
    "mailto:",
    URLencode(address),
    "?subject=", URLencode(subject),
    "&body=", URLencode(body),
    sep = ""
  )
  
  tryCatch({
    browseURL(url, browser = email_browser())},
    error = function(e) {
      message("Sending failed with error: ", e$message)
      cat("To: ", address, "\n", sep = "")
      cat("Subject: ", subject, "\n", sep = "")
      cat("\n")
      cat(body, "\n", sep = "")
    }
  )
  
  invisible(TRUE)
}

email_browser <- function() {
  if (!identical(.Platform$GUI, "RStudio"))
    return (getOption("browser"))
  
  # Use default browser, even if RStudio running
  if (.Platform$OS.type == "windows")
    return (NULL)
  
  browser <- Sys.which(c("xdg-open", "open"))
  browser[nchar(browser) > 0][[1]]
}
