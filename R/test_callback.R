hi <- function(){
  addTaskCallback(testCallback, name = "testy", data="data")
  invisible()
}

bye <- function(){
  removeTaskCallback(which(getTaskCallbackNames() == "testy"))
  invisible()
}

testCallback <- function(...){
  print("testing")
  print(paste(class(..1), ": ", as.expression(..1), sep=""))
  if(class(..1)=="<-"){
    print(as.list(..1))
  }
  try(print(paste("  ", class(..2), ":", ..2)), silent=TRUE)
  print(paste("\t", class(..5), ":", ..5))
  return(TRUE)
}