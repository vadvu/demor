idmat <- function(x){
  xmat = matrix(nrow = x, ncol = x, data = 0)
  diag(xmat) <- 1
  return(xmat)
}

diagmat <- function(x){
  id <- idmat(length(x))
  diag(id) <- x
  return(id)
}
