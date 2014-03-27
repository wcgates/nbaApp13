normalize <- function(x) { 
  x <- sweep(x, 2, apply(x, 2, min)) 
  sweep(x, 2, apply(x, 2, max), "/")
  
}