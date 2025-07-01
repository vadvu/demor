#' Leslie Matrix
#'
#' @param mx Numeric array of age specific mortality rates.
#' @param fx Numeric array of age specific fertility rates.
#' @param age.mx Numeric array of ages for mx.
#' @param age.fx Numeric array of ages for fx.
#' @param srb Numeric. Sex ratio at birth. Usually it is assumed that for males it is `105/205`, for females it is `100/205`. By default, it is `100/205`.
#' @param fin Logical. Should the survival rate for the last age-group be nonzero? By default it is `FALSE`, so the last survival rate is 0 as in classical model. Otherwise, it is \deqn{T_{x}/T_{x-1}}.
#' @param ... Optional. Additional arguments for `LT` function.
#'
#' @return Matrix.
#' @export
leslie <- function(mx, fx, age.mx, age.fx, srb = 100/205, fin = FALSE, ...){
  lt <- LT(age = age.mx, mx = mx, ...)
  n <- length(age.mx)

  A <- matrix(0, n, n)

  Lx = lt[,"Lx"]
  Sx <- rep(0, 101)
  Sx[1] <- Lx[1]/(lt[1,"lx"] * diff(age.mx)[1])
  Sx[-1] <-  Lx[-1] / Lx[-n]
  Sx[n] <- A[n,n] <- ifelse(fin, lt[n,"Tx"]/lt[n-1,"Tx"], 0)

  diag(A[-1,]) <- Sx[-1]

  names(fx) <- age.fx
  fert = sapply(age.mx, FUN = function(x) ifelse(x %in% age.fx, fx[paste0(x)], 0) )
  A[1,] <- srb * (0.5 * Sx[1]) * ( fert + c(fert[-1]*Sx[-1], 0) )

  return(A)
}
