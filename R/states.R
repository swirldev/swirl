### STATES

doStage <- function(state, expr, val)UseMethod("doStage")
nextState <- function(state)UseMethod("nextState")

newState <- function(n){
  cls <- c("testMod")
    
}

doStage.testMod <- function(state, expr, val){
  frndlyOut(state$content[1,"Output"])
  state$stage <- state$stage+1
  temp <- readline("...")
  suspend <- suspendQ()
  return(list(finished=TRUE, prompt=suspend, suspend=suspend, state=state))
}

nextState.testMod <- function(state){
 
}

suspendQ <- function(){
  suspend <- temp == "play"
  if(suspend)frndlyOut("Type nxt() to continue.")
}