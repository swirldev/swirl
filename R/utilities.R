swirl_out <- function(..., skip_before=TRUE, skip_after=FALSE) {
  wrapped <- strwrap(str_c(..., sep = " "),
                     width = getOption("width") - 2)
  mes <- str_c("| ", wrapped, collapse = "\n")
  if(skip_before) mes <- paste0("\n", mes)
  if(skip_after) mes <- paste0(mes, "\n")
  message(mes)
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

# we evaluate user's expression in a safe environment
# whose parent is the global environment
# substitutes global values of any variables thus
# created in case they are different as in rng
# returns list of names and values
safeEval <- function(expr){
  e1 <- new.env(parent=globalenv())
  ans <- list()
  eval(expr,e1,NULL)
  for (x in ls(e1)){
    if(exists(x,globalenv()))
      ans[[x]] <- get(x,globalenv())
  }
  return(ans)
}

cleanEnv <- function(snapshot){
  # clone of previous environment
  pe <- as.environment(as.list(snapshot))
  parent.env(pe) <- parent.env(globalenv())
  # return new environment whose parent is pe
  return(new.env(parent=pe))
}