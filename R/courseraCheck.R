#' @importFrom stringr str_detect
courseraCheck <- function(e){
  modtype <- attr(e$les, "type")
  lesson_name <- gsub(" ", "_", attr(e$les, "lesson_name"))
  if(is.null(modtype) || modtype != "Coursera")return()
  tt <- c(rep(letters, 3), seq(100))
  swirl_out(NLang$"Are you currently enrolled in the Coursera...")
  yn <- select.list(c(NLang$"Yes",NLang$"No"), graphics=FALSE)
  if(yn==NLang$"No")return()
  ss <- lapply(1:2, function(i) {
    paste0(sample(tt, sample(seq(400), 1), replace=TRUE), collapse="")
  })
  swirl_out(NLang$"Would you like me to notify Coursera...",
            NLang$"If so, I'll need to get some more info from you.")
  choice <- select.list(c(NLang$"Yes",NLang$"No",NLang$"Maybe later"), graphics=FALSE)
  if(choice==NLang$"No") return()
  # Begin submission loop
  ok <- FALSE
  while(!ok) {
    # Get submission credentials
    r <- getCreds(e)
    email <- r["email"]
    passwd <- r["passwd"]
    course_name <- r["courseid"]
    output <- paste0(ss[[1]], substr(e$coursera, 1, 16), ss[[2]],
                     collapse="")
    # Name output file
    output_filename <- paste0(course_name,"_",lesson_name,".txt")
    # Write output to text file
    writeLines(output, output_filename)
    # If going straight to manual submission, then exit loop.
    if(choice=="Maybe later") ok <- TRUE
    # If doing automatic submission, then give it a try.
    if(choice=="Yes"){
      swirl_out(NLang$"I'll try to tell Coursera...")
      challenge.url <- paste("http://class.coursera.org", course_name,
                             "assignment/challenge", sep = "/")
      submit.url <- paste("http://class.coursera.org", course_name,
                          "assignment/submit", sep = "/")
      ch <- try(getChallenge(email, challenge.url), silent=TRUE)
      # Check if url is valid, i.e. challenge received
      ch_ok <- is.list(ch) && exists("ch.key", ch) && !is.na(ch$ch.key)
      if(!is(ch, "try-error") && ch_ok) {
        ch.resp <- challengeResponse(passwd, ch$ch.key)
        # If submit.url is invalid, submitSolution should return a try-error.
        # However, that is not the only way it can fail; see below.
        results <- submitSolution(email, submit.url, ch.resp, 
                                  sid=lesson_name, 
                                  output=output,
                                  signature=ch$state)
        # If incorrect, empty string will be returned
        if(!length(results)) {
          swirl_out(NLang$"You skipped too many questions!...",
                    NLang$"this lesson again...",
                    NLang$"don't skip more...")
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
            swirl_out(paste0(NLang$"I've notified Coursera...",
                             course_name, ", ", lesson_name,"."))
            # Remove manual submission text file
            unlink(output_filename)
            # Exit loop since submission successful
            return()
          }
          swirl_out(NLang$"I'm sorry, something went wrong with automatic submission.")
          # Exit loop if user doesn't want to retry auto submission
          ok <- !retry()
        } else {
          swirl_out(NLang$"I'm sorry, something went wrong with automatic submission.")
          # Exit loop if user doesn't want to retry auto submission
          ok <- !retry()
        }
      } else {
        swirl_out(NLang$"I'm sorry, something went wrong with establishing connection.")
        # Exit loop if user doesn't want to retry auto submission
        ok <- !retry()
      }
    } # end of yes branch
  } # end of while loop
  writeLines(output, paste0(course_name,"_",lesson_name,".txt"))
  swirl_out(NLang$"To notify Coursera...please upload",
            sQuote(paste0(course_name,"_",lesson_name,".txt")),
            NLang$"to Coursera manually...",
            skip_after=TRUE)
  message(getwd(), "\n")
  readline("...")
}

# Returns TRUE if user would like to retry, FALSE if not
retry <- function() {
  swirl_out(NLang$"Would you like to retry automatic submission...")
  ans <- select.list(c(NLang$"Retry automatic submission", NLang$"Submit manually"), graphics=FALSE)
  # Return TRUE if user would like to retry
  return(ans == NLang$"Retry automatic submission")
}

get_courseid <- function() {
  swirl_out(NLang$"The first item I need is your Course ID...",
            skip_after=TRUE)
  repeat {
    courseid <- readline("Course ID: ")
    # Remove quotes if there are any
    courseid <- gsub("\'|\"", "", courseid)
    # Set up test cases
    is_url <- str_detect(courseid, "www[.]|http:|https:")
    is_numbers <- str_detect(courseid, "^[0-9]+$")
    is_example <- str_detect(courseid, fixed("rprog-001"))
    
    # Check if courseid is none of the bad things
    if(!any(is_url, is_numbers, is_example)){
      break
    # courseid is one of the bad things
    } else {
      # Check if courseid is a url
      if(is_url) {
        swirl_out(NLang$"It looks like you entered a web address...")
      }
      # Check if courseid is all numbers
      if(is_numbers) {
        swirl_out(NLang$"It looks like you entered a numeric ID...")
      }
      # Check if the user stole the example courseid
      if(is_example) {
        swirl_out(NLang$"It looks like you entered the Course ID...")
      }
    }
    swirl_out(NLang$"Instead, I need your Course ID, which is...",
              skip_after=TRUE)
  }
  courseid
}

getCreds <- function(e) {
  cn <- make_pathname(attr(e$les, "course_name"))
  credfile <- file.path(e$udat, paste0(cn, ".txt"))
  e$coursera <- digest(paste0("complete", paste0(
    rep("_", ifelse(is.null(e$skips), 0, e$skips)), collapse="")),
    algo="sha1", serialize = FALSE)
  
  confirmed <- FALSE 
  need2fix <- FALSE
  while(!confirmed) {
    if(!file.exists(credfile) || need2fix) {
      courseid <- get_courseid()
      email <- readline("Submission login (email): ")
      passwd <- readline("Submission password: ")
      writeLines(c(courseid, email, passwd), credfile)
      r <- c(courseid = courseid, email = email, passwd = passwd)
    } else {
      r <- readLines(credfile, warn=FALSE)
      names(r) <- c("courseid", "email", "passwd")
    }
    swirl_out(NLang$"Is the following information correct?", skip_after=TRUE)
    message("Course ID: ", r['courseid'],
            "\nSubmission login (email): ", r['email'], 
            "\nSubmission password: ", r['passwd'])
    yn <- c(NLang$"Yes, go ahead!", 
            NLang$"No, I need to change something.")
    confirmed <- identical(select.list(yn, graphics=FALSE), yn[1])
    if(!confirmed) need2fix <- TRUE
  }
  return(r)
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