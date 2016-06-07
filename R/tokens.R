##################################################################
## generates and assigns values to tokens for a given row
##################################################################
token.generate <- function(row, token.list){
  tokens = NULL 
  if(!is.na(row$Token)){                #If there's anything in the 'Token' row,
    token.list <- tokens.create(as.character(row$Token), token.list)  #create the tokens
    row <- tokens.replace(row, token.list)  #then replace the tokens
  } else if (!is.null(token.list)){	# if there's anything in the token.list	
    row <-tokens.replace(row, token.list) # then replace the tokens
  }
  ans = list(row = row, token.list = token.list)
  return(ans)
}


########################################################################
# executes R code in .token.str and returns a list containing objects
# in the .token.list as well as any new objects created in the function
########################################################################
tokens.create <- function(.token.str, .token.list) {

  # add token.list objects to function namespace,
  # since these may be used by .token.str
  if (!is.null(.token.list)) {
     .n = length(.token.list)
     for(.i in 1:.n) {
       .na = names(.token.list)[.i]
       assign(.na, .token.list[[.na]])
     }
   }
 
  #executes token code
  eval(parse(text = .token.str))   
  # creates a vector of tokens in the function environment
  .tokens = ls() 
  # creates list of (token,value) pairs
  .vals = lapply(1:length(.tokens), function(i,t) {get(t[i])}, 
	t= .tokens)
  names(.vals) = .tokens

  return(.vals)
}

###################################################################
# For given row, replace each token <T> with its value
# Note: only some token types are valid here (see code below), and 
#  vectors/matrix values are formatted to be comma-separated   
###################################################################
tokens.replace <- function(row,tokens){
  replace<-function(s,t.name,t.val) {
	if (is.na(s)) return(NA)
        gsub(paste0("<",t.name,">"),t.val[1], s)
  }

  valid = c("logical", "integer", "double", "character")
  token.names = names(tokens)[which(sapply(tokens, typeof) %in% valid)]
  for (n in token.names){
    row = lapply(row, replace,t.name = n,t.val = paste0(tokens[[n]],collapse = ","))
  }
  row = data.frame(row, stringsAsFactors = FALSE) 
  return(row)
}


