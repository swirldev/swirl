#' Takes two environments as arguments and returns name and value of variable 
#' that is found in env2, but not in env1. Meant to detect and capture newly 
#' created variable.
#' 
envDiff <- function(env1, env2) {
  # Check that env1 and env2 are both environments
  stopifnot(is.environment(env1) && is.environment(env2))
  
  # Look at contents of each environment
  # str(as.list(env1))
  # str(as.list(env2))
  
  # Store (as vector) names of all variables in each environment
  n1 <- as.vector(names(as.list(env1)))
  n2 <- as.vector(names(as.list(env2)))
  
  # Store name of newly created variable
  newVarName <- setdiff(n2, n1)  # Order matters here!
  
  # Get value of newly created variable
  newVarValue <- get(newVarName, envir=env2)
  
  # Return name and value of new variable
  list(name=newVarName, value=newVarValue)
}

### EXAMPLE

# Create two new environments
firstEnv <- new.env(parent=emptyenv())
secondEnv <- new.env(parent=emptyenv())

# Assign 10 to variable x in second environment
assign("x", 10, envir=secondEnv)

# Call envDiff to detect difference between two environments
# (i.e. get name and value of newly created variable)
envDiff(env1=firstEnv, env2=secondEnv)