courseraCheck <- function(e){
  modtype <- attr(e$les, "type")
  course_name <- gsub(" ", "_", attr(e$les, "course_name"))
  lesson_name <- gsub(" ", "_", attr(e$les, "lesson_name"))
  if(is.null(modtype) || modtype != "Coursera")return()
 
  
  swirl_out("Do you want Coursera credit for this lesson? (I'll need some info from you if you want credit.)")
  choice <- select.list(c("Yes.","No.","Maybe later."), graphics=FALSE)
  if(choice=="No.")return()
  # Get submission credentials
  r <- getCreds(e)
  email <- r["email"]
  passwd <- r["passwd"]
  if(choice=="Yes."){
    swirl_out("I'll try to tell Coursera you've completed this lesson now.")
    challenge.url <- paste("http://class.coursera.org", course_name,
                           "assignment/challenge", sep = "/")
    submit.url <- paste("http://class.coursera.org", course_name,
                        "assignment/submit", sep = "/")
    ch <- try(getChallenge(email), silent=TRUE)
    # Continue only if the challenge has worked
    if(!is(ch, "try-error")){
      ch.resp <- challengeResponse(passwd, ch$ch.key)
      results <- try(submitSolution(email, ch.resp, lesson_name, "complete", ch$state))
      if(!is(results, "try-error")){
        swirl_out(paste0("I've notified Coursera that you have completed ",
                         course_name, ", ", lesson_name,"."))
        return()
      } else {
        swirl_out("I'm sorry, something went wrong with automatic submission.")
      }
    } else {
      swirl_out("I'm sorry, something went wrong with establishing connection.")
    }
  }#yes branch
  writeLines(c(email, passwd), paste0(course_name,"_",lesson_name,".txt"))
  swirl_out("To notify Coursera that you have completed this lesson, please upload ")
  swirl_out(paste0(course_name,"_",lesson_name,".txt "))
  swirl_out(" to Coursera manually.")
  readline("...")
}

getCreds <- function(e) {
  credfile <- file.path(e$udat, paste0(e$les$course_name,".txt"))
  if(!file.exists(credfile)){
    email <- readline("Submission login (email): ")
    passwd <- readline("Submission  password: ")
    writeLines(c(email, passwd), credfile)
    return(c(email = email, passwd = passwd))
  } else {
    r <- readLines(credfile)
    names(r) <- c("email", "passwd")
    return(r)
  }
}

#' @importFrom RCurl getForm
getChallenge <- function(email) {
  params <- list(email_address = email, response_encoding = "delim")
  result <- getForm(challenge.url, .params = params)
  s <- strsplit(result, "|", fixed = TRUE)[[1]]
  list(ch.key = s[5], state = s[7])
}

#' @importFrom digest digest
challengeResponse <- function(password, ch.key) {
  x <- paste(ch.key, password, sep = "")
  digest(x, algo = "sha1", serialize = FALSE)
}

#' @importFrom RCurl postForm base64
submitSolution <- function(email, ch.resp, sid, output, signature, src = "",
                           http.version = NULL) {
  output <- as.character(base64(output))
  src <- as.character(base64(src))
  params <- list(assignment_part_sid = sid,
                 email_address = email,
                 submission = output,
                 submission_aux = src,
                 challenge_response = ch.resp,
                 state = signature)
  params <- lapply(params, URLencode)
  result <- postForm(submit.url, .params = params)
  s <- strsplit(result, "\\r\\n")[[1]]
  tail(s, 1)
}