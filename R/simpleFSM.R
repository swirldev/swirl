

makeFSM <- function(loadfile=FALSE){
  if(loadfile){
    load("data/saved.RData")
  } else {
    n <- 1
  }
  function(expr, val, ok, vis, data=n){
    if(is.call(expr)){
      if(expr[[1]] == "brk"){
        save(n, file="data/saved.RData" )
        return(FALSE)
      }
    }
    print(paste("n =", n))
    n <<- n+1
    return(n <= 5)
  }
}

nxt <- function(){invisible()}

brk <- function(){invisible()}
  
hi <- function(loadfile=FALSE){
  removeTaskCallback(id="poc")
  addTaskCallback(makeFSM(loadfile), name="poc")
}