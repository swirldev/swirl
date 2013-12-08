  assign("cars", read.csv("data/cars.csv", as.is=TRUE, comment.char="#"), envir=globalenv())
  assign("mpg.midsize", cars[cars$type=="midsize","mpgCity"], envir=globalenv())
