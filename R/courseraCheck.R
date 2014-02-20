courseraCheck <- function(e){
  modtype <- attr(e$mod, "Type")
  courseName <- attr(e$mod, "courseName")
  lessonName <- attr(e$mod, "modName")
  if(is.null(modtype) || modtype != "Coursera")return()
  swirl_out("I can automatically notify Coursera that you have completed this lesson, provided you are currently connected to the internet. Should I attempt this now?")
  if("Yes." == select.list(c("Yes.", "No."))){
    challenge.url <- paste("http://class.coursera.org", courseName,
                           "assignment/challenge", sep = "/")
    submit.url <- paste("http://class.coursera.org", courseName,
                        "assignment/submit", sep = "/")
    # Get submission credentials
    r <- getCreds(e)
    email <- r["email"]
    passwd <- r["passwd"]
    ch <- try(getChallenge(email), silent=TRUE)
    # Continue only if the challenge has worked
    if(!is(ch, "try-error")){
      ch.resp <- challengeResponse(passwd, ch$ch.key)
      results <- try(submitSolution(email, ch.resp, lessonName, "complete", ch$state))
      if(!is(results, "try-error")){
        swirl_out(paste0("I've notified Coursera that you have completed ", courseName, 
                         ", ", lessonName,"."))
        return()
      } else {
        swirl_out("I'm sorry, something went wrong with automatic submission.")
      }
    } else {
      swirl_out("I'm sorry, something went wrong with establishing connection.")
    }
  }
  writeLines(c(email, passwd), paste0(courseName,"_",lessonName,".txt"))
  swirl_out(paste0("If you would like to notify Coursera that you have completed this lesson, please upload",
                   paste0(courseName,"_",lessonName,".txt"), " to Coursera manually.")
}

getCreds <- function(e) {
  credfile <- file.path(e$udat, paste0(e$courseName,".txt"))
  if(!file.exists(credfile)){
  email <- readline("Submission login (email): ")
  passwd <- readline("Submission  password: ")
  writeLines(credfile, c(email, passwd))
  return(c(email = email, passwd = passwd))
  } else {
    r <- readLines(credfile)
    names(r) <- c("email", "passwd")
    return(r)
  }
}

getChallenge <- function(email) {
  params <- list(email_address = email, response_encoding = "delim")
  result <- getForm(challenge.url, .params = params)
  s <- strsplit(result, "|", fixed = TRUE)[[1]]
  list(ch.key = s[5], state = s[7])
}

challengeResponse <- function(password, ch.key) {
  x <- paste(ch.key, password, sep = "")
  digest(x, algo = "sha1", serialize = FALSE)
}

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