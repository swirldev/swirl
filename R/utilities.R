swirl_out <- function(..., skip_before=TRUE, skip_after=FALSE) {
  wrapped <- strwrap(str_c(..., sep = " "),
                     width = getOption("width") - 2)
  mes <- str_c("| ", wrapped, collapse = "\n")
  if(skip_before) mes <- paste0("\n", mes)
  if(skip_after) mes <- paste0(mes, "\n")
  message(mes)
}

# Takes a plain English name and turns it into a more proper 
# file or directory name
make_pathname <- function(name) {
  gsub(" ", "_", str_trim(name))
}

xfer <- function(env1, env2){
  lapply(ls(env1), function(var)getAssign(var, env1, env2))
}

getAssign <- function(var, env1, env2){
  assign(var, get(var, env1, inherits=FALSE), envir=env2)
}

cleanAdmin <- function(){
  udat <- file.path(find.package("swirl"), "user_data", "swirladmin")
  file.remove(dir(udat, pattern="*[.]rda", full.names=TRUE))
  invisible()
}

mergeLists <- function(sourceList, destList){
 for (n in names(sourceList)){
   destList[[n]] <- sourceList[[n]]
 }
 return(destList)
}

# Evaluates a user's expression in a clean environment
# whose parent is a snapshot of the previous global 
# environment, i.e., the same environment in which
# the user entered the expression. Any values required
# for evaluation will be found in the snapshot. Any variables
# changed or created by the expression will appear in the 
# clean environment, even if nothing changes in the global. 
#
# For example, if x already has the value c(1, 2, 3) and
# the user enters x <- c(1, 2, 3), nothing will change
# in the global environment, but x with the value c(1, 2, 3)
# will appear in the clean environment.
#
# In case the user's expression involves random numbers, the
# values of variables which appear in the clean environment
# are copied from the global environment.
# 
# For example, if the user enters x <- rnorm(100), then
# evaluating the expression in a clean environment will create
# a variable named x, but it will have a different value
# than that created by the user.
#
safeEval <- function(expr, e){
  e1 <- cleanEnv(e$snapshot)
  ans <- list()
  suppressMessages(suppressWarnings(eval(expr,e1)))
  for (x in ls(e1)){
    if(exists(x,globalenv()))
      ans[[x]] <- get(x,globalenv())
  }
  return(ans)
}

# Creates a clean environment whose parent is
# a snapshot of the global environment in an
# earlier state. The snapshot itself is given the
# same parent as the global environment, which will
# consist of loaded namespaces and packages.
#
# Environments in R are subject to reference semantics, 
# i.e., all references refer to the same copy. Hence, 
# the state of an environment cannot be saved for later 
# comparison merely by creating a second reference. Any 
# change in the environment will affect all references. 
# Lists, however, have R's usual copy-on-modify semantics. 
# If snapshot <- as.list(globalenv()), a subsequent change 
# in the global environment will not cause a change in 
# the list (with the exotic exception of environments
# contained in the global environment.)
# 
# Clean environments can be used to detect variables 
# changed or created by a user, as in function safeEval.
# They can also be used to check the correctness of
# a value computed by a user.
#
# For example, if the user enters x <- 2*x, then the
# value of x in the global environment will have changed,
# but if the expression is evaluated in a clean environment
# the value of x on the right will be found in the snapshot
# hence will be the same as that found by the user. 
#
cleanEnv <- function(snapshot){
  # clone of previous environment
  pe <- as.environment(as.list(snapshot))
  parent.env(pe) <- parent.env(globalenv())
  # return new environment whose parent is pe
  return(new.env(parent=pe))
}


# LESSON PACKAGE DEPENDENCY SUPPORT

# Load lesson package dependencies quietly
loadDependencies <- function(lesson_dir) {
  depends <- file.path(lesson_dir, "dependson.txt")
  if(file.exists(depends)) {
    packages_as_chars <- setdiff(readLines(depends, warn=FALSE), "")
    # If the dependson file is empty, then proceed with lesson
    if(length(packages_as_chars) == 0) return(TRUE)
    swirl_out("Attemping to load lesson dependencies...")
    for(p in packages_as_chars) {
      if(suppressPackageStartupMessages(
        suppressWarnings(
          suppressMessages(require(p, character.only=TRUE, quietly=TRUE))))) {
        swirl_out("Package", sQuote(p), "loaded correctly!")
      } else {
        swirl_out("This lesson requires the", sQuote(p), 
                  "package. Would you like me to install it for you now?")
        yn <- select.list(choices=c("Yes", "No"), graphics=FALSE)
        if(yn == "Yes") {
          swirl_out("Trying to install package", sQuote(p), "now...")
          install.packages(p, quiet=TRUE)
          if(suppressPackageStartupMessages(
            suppressWarnings(
              suppressMessages(require(p, 
                                       character.only=TRUE, 
                                       quietly=TRUE))))) {
            swirl_out("Package", sQuote(p), "loaded correctly!")
          } else {
            swirl_out("Could not install package", paste0(sQuote(p), "!"))
            return(FALSE)
          }
        } else {
          return(FALSE)
        }
      }
    }
  }
  # If loop completes, then print a blank line and return TRUE
  cat("\n")
  return(TRUE)
}