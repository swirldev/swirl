#' @importFrom stringr str_detect
courseraCheck <- function(e){
  modtype <- attr(e$les, "type")
  lesson_name <- gsub(" ", "_", attr(e$les, "lesson_name"))
  if(is.null(modtype) || modtype != "Coursera")return()
 
  
  swirl_out("Are you currently enrolled in the Coursera course associated with this lesson?")
  yn <- select.list(c("Yes","No"), graphics=FALSE)
  if(yn=="No")return()
  
  swirl_out("Would you like me to notify Coursera that you've completed this lesson?",
            "If so, I'll need to get some more info from you.")
  choice <- select.list(c("Yes","No","Maybe later"), graphics=FALSE)
  if(choice=="No")return()
  # Get submission credentials
  r <- getCreds(e)
  email <- r["email"]
  passwd <- r["passwd"]
  course_name <- r["courseid"]
  output <- substr(e$coursera, 1, 16)
  if(choice=="Yes"){
    swirl_out("I'll try to tell Coursera you've completed this lesson now.")
    challenge.url <- paste("http://class.coursera.org", course_name,
                           "assignment/challenge", sep = "/")
    submit.url <- paste("http://class.coursera.org", course_name,
                        "assignment/submit", sep = "/")
    ch <- try(getChallenge(email, challenge.url), silent=TRUE)
    # Continue only if the challenge has worked
    if(!is(ch, "try-error")){
      ch.resp <- challengeResponse(passwd, ch$ch.key)
      # If submit.url is invalid, submitSolution should return a try-error.
      # However, that is not the only way it can fail; see below.
      results <- submitSolution(email, submit.url, ch.resp, 
                                sid=lesson_name, 
                                output=output,
                                signature=ch$state)
      # If incorrect, empty string will be returned
      if(!length(results)) {
        swirl_out("You skipped too many questions! You'll need to complete",
                  "this lesson again if you'd like to receive credit. Please",
                  "don't skip more than one question next time.")
        return()
      }
      if(!is(results, "try-error")){
        # TODO: It would be best to detect success here, rather than
        # failure, but as of Feb 23 2014, submit.url may not throw
        # an error indicating failure but instead return an HTML
        # notification beginning with the word, "Exception".
        # Here we detect failure by the presence of this word.
        # Server-side behavior could easily change and could easily
        # be course dependent, so some standard handshake will have
        # to be set up eventually.
        swirl_out(results)
        if(!str_detect(results, "[Ee]xception")){
          swirl_out(paste0("I've notified Coursera that you have completed ",
                           course_name, ", ", lesson_name,"."))
          return()
        }
        swirl_out("I'm sorry, something went wrong with automatic submission.")
      } else {
        swirl_out("I'm sorry, something went wrong with automatic submission.")
      }
    } else {
      swirl_out("I'm sorry, something went wrong with establishing connection.")
    }
  }#yes branch
  writeLines(output, paste0(course_name,"_",lesson_name,".txt"))
  swirl_out("To notify Coursera that you have completed this lesson, please upload",
            sQuote(paste0(course_name,"_",lesson_name,".txt")),
            "to Coursera manually. I've placed the file in the following directory:",
            getwd(), skip_after=TRUE)
  readline("...")
}

getCreds <- function(e) {
  credfile <- file.path(e$udat, paste0(e$les$course_name,".txt"))
  e$coursera <- digest(paste0("complete", paste0(
    rep("_", ifelse(is.null(e$skips), 0, e$skips)), collapse="")),
    algo="sha1", serialize = FALSE)
  swirl_out("The first item I need is your course ID. If the homepage for your",
            "Coursera course is 'https://class.coursera.org/rprog-001',",
            "then your course ID is 'rprog-001' (without the quotes).",
            skip_after=TRUE)
  courseid <- readline("Course ID: ")
  if(!file.exists(credfile)){
    email <- readline("Submission login (email): ")
    passwd <- readline("Submission password: ")
    writeLines(c(email, passwd), credfile)
    r <- c(email = email, passwd = passwd)
  } else {
    r <- readLines(credfile, warn=FALSE)
    names(r) <- c("email", "passwd")
  }
  return(c(r, courseid = courseid))
}

#' @importFrom RCurl getForm
getChallenge <- function(email, challenge.url) {
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
submitSolution <- function(email, submit.url, ch.resp, sid, output, 
                           signature, src = "",http.version = NULL) {
  output <- as.character(base64(output))
  src <- as.character(base64(src))
  params <- list(assignment_part_sid = sid,
                 email_address = email,
                 submission = output,
                 submission_aux = src,
                 challenge_response = ch.resp,
                 state = signature)
  params <- lapply(params, URLencode)
  result <- try(postForm(submit.url, .params = params), silent=TRUE)
  if(is(result,"try-error")){
    return(result)
  } else {
    s <- strsplit(result, "\\r\\n")[[1]]
    return(tail(s, 1))
  }
}