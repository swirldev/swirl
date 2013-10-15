new_tree <- function(...) UseMethod("new_tree")

new_tree.call <- function(expr, val, ok) {
  structure(list(expr=expr, val=val, ok=ok), class=c("calltree", "tree"))
}

`new_tree.<-` <- function(expr, val, ok) {
  structure(list(expr=expr, val=val, ok=ok), class=c("assgntree", "tree"))  
}

new_tree.default <- function(expr, val, ok) {
  structure(list(expr=expr, val=val, ok=ok), class="tree")
}

treetop <- function(...) UseMethod("treetop")

treetop.calltree <- function(tree) {
  return(as.character(tree$expr[[1]]))
}

treetop.assgntree <- function(tree) {
  return(as.character(tree$expr[[1]]))  
}

treetop.default <- function(tree) {
  return(as.character(tree$expr))
}

treearg <- function(...) UseMethod("treearg")

treearg.calltree <- function(tree, argn) {
  if (length(tree$expr) < argn + 1) {
    return(NULL)
  } else {
    return(tree$expr[[argn + 1]])
  }
}

treearg.assgntree <- function(tree, argn) {
  if (length(tree$expr) < argn + 1) {
    return(NULL)
  } else {
    return(tree$expr[[argn + 1]])
  }
}

treearg.default <- function(tree, argn) {
  return(NULL)
}
