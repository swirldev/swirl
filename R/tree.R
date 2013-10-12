new_tree <- function(expr, val, ok) {
  structure(list(expr=expr, val=val, ok=ok), class="tree")
}

treetop <- function(tree) {
  return(as.character(tree$expr[[1]]))
}

treearg <- function(tree, argn) {
  if (length(tree$expr) < argn + 1) {
    return(NULL)
  } else {
    return(tree$expr[[argn + 1]])
  }
}
