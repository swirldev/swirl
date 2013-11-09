hi <- function(){
  # Create a new container for global data and state of user's progress
  module <- new.env(parent = emptyenv())
  # Read the test module into it
  module$mod <- read.csv("data/testMod.csv", as.is=TRUE)
  # As a convenience, store mod's number of rows there too.
  module$rows <- nrow(module$mod)
  # Assign module, which is only known inside this function
  # at the moment, to a variable of the same name in the
  # global environment.
  assign("module", module, envir=globalenv() )
}



cback <- function(expr, val, ok, vis){
  if(identical(expr, parse(text="nxt()")[[1]]) ||
       identical(expr, parse(text="hi()")[[1]])){
    module$suspended <- FALSE
  }
  if(module$suspended)return(TRUE)
  
  n <- 1 # to avoid any infinte loop
  while(n < 20){
    n <- n+1
    state <- module$state
    if(is.null(state))return(FALSE) # will unregister callback
    response <- doStage(state, expr, val)
    module$suspended <- response$suspend
    if(response$finished){
      module$state <- nextState(state)
    } else {
      module$state <- response$state
    }
    if(response$prompt)break
  } 
  return(TRUE)
}

