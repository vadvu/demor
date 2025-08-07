#' Leslie Matrix
#'
#' @param mx Numeric array of age specific mortality rates.
#' @param fx Numeric array of age specific fertility rates.
#' @param age.mx Numeric array of ages for mx.
#' @param age.fx Numeric array of ages for fx.
#' @param srb Numeric. Sex ratio at birth. Usually it is assumed that for males it is `105/205`, for females it is `100/205`. By default, it is `100/205`.
#' @param fin Logical. Should the survival rate for the last age-group be nonzero? By default it is `FALSE`, so the last survival rate is 0 as in classical model. Otherwise, it is \eqn{T_{x}/T_{x-1}}.
#' @param ... Optional. Additional arguments for [LT()] function.
#' @return Matrix.
#' @export
leslie <- function(mx, fx, age.mx, age.fx, srb = 100/205, fin = TRUE, ...){
  lt <- LT(age = age.mx, mx = mx, ...)
  radix <- lt[1,"lx"]
  Tx <- lt[,"Tx"]
  n0 <- length(age.mx)
  if(length(unique(diff(age.mx))) > 1){
    ux <- unique(diff(age.mx))
    agegroup <- ux[which.max(tabulate(match(diff(age.mx), ux)))]
    newage <- seq(0, max(age.mx), agegroup)
    newage0 <- newage
    newage0[1] = newage0[2] - 1
    lt <- cbind(lt, newage = demor::ages(x = age.mx, groups = newage0, char = FALSE))
    lt[which(lt[,"newage"] == newage0[1]), "newage"] <- newage[1]
    lt <- aggregate(data = lt, Lx ~ newage, FUN = sum)
    age.mx = newage
  }
  n <- length(age.mx)

  Lx = lt[,"Lx"]
  Sx <- Lx[-1] / Lx[-n]
  Sx[n-1] <- Tx[n0]/Tx[n0-1]
  if(any(is.na(Sx))){
    Sx[is.na(Sx)] = 0
    warning("There are NAs in survival rates, change them to 0")
  }

  A <- matrix(0, n, n)
  A[n,n] <- ifelse(fin, Sx[n-1], 0)
  diag(A[-1,1:(n-1)]) <- Sx[-n]
  names(fx) <- age.fx
  fert = sapply(age.mx, FUN = function(x) ifelse(x %in% age.fx, fx[paste0(x)], 0) )
  k <- srb * ( Lx[1] / (2*radix) )
  A[1,] <- k * ( fert + c(fert[-1] * Sx[-n], 0) )

  return(A)
}
