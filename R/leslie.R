#' Leslie Matrix
#'
#' @param mx Numeric array of age specific mortality rates.
#' @param fx Numeric array of age specific fertility rates.
#' @param age.mx Numeric array of ages for mx.
#' @param age.fx Numeric array of ages for fx.
#' @param srb Numeric. Sex ratio at birth. Usually it is assumed that for males it is `105/205`, for females it is `100/205`. By default, it is `100/205`.
#' @param fin Logical. Should the survival rate for the last age-group be nonzero? By default it is `FALSE`, so the last survival rate is 0 as in classical model. Otherwise, it is \eqn{T_{x}/T_{x-1}}.
#' @param ... Optional. Additional arguments for `LT` function.
#'
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
  A <- matrix(0, n, n)
  Sx <- rep(0, n)
  Sx[1] <- Lx[1]/(radix * diff(age.mx)[1])
  Sx[-1] <- Lx[-1] / Lx[-n]
  Sx[n] <- ifelse(fin, Tx[n0]/Tx[n0-1], 0)
  if(0 %in% Sx){
    rta <- Sx[n - length(which(Sx == 0 | is.na(Sx)))] / Sx[n - length(which(Sx == 0 | is.na(Sx))) - 1]
    Sx[which(Sx == 0 | is.na(Sx))] = cumprod(Sx[n - length(which(Sx == 0 | is.na(Sx)))] * rep(rta, length(which(Sx == 0 | is.na(Sx)))))
  }
  A[n,n] <- Sx[n]
  diag(A[-1,1:(n-1)]) <- Sx[-1]
  names(fx) <- age.fx
  fert = sapply(age.mx, FUN = function(x) ifelse(x %in% age.fx, fx[paste0(x)], 0) )
  k <- srb * (0.5 * Sx[1])
  A[1,] <- k * diff(age.fx)[1] * ( fert + c(fert[-1]*Sx[-1], 0) )

  return(A)
}
