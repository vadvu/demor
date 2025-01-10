#' Leslie Matrix
#'
#' @param mx Numeric array of age specific mortality rates.
#' @param fx Numeric array of age specific fertility rates.
#' @param age1 Numeric array of ages for mx.
#' @param age2 Numeric array of ages for fx.
#' @param sex Character. Sex. "m" for males or "f" for females.
#' @param fin Logical. Should the survival rate for the last age-group be not 0? By default it is `FALSE`, so the last survival rate is 0 as in classical model. Otherwise, it is \deqn{T_{x}/T_{x-1}}
#' @param ... Optional. Additional arguements for `LT` function.
#'
#' @return Matrix.
#' @export
leslie <- function(mx, fx, age1, age2, sex, fin = FALSE, ...){
  lt <- LT(age = age1, sex = sex, mx = mx, ...)
  n <- length(age1)

  Sx <- array(0,c(n-1))
  Lx <- lt[,"Lx"]
  for (i in 1:n-1){
    Sx[i]<-(Lx[i+1]/Lx[i])
  }
  sf1<-rbind(0,cbind(diag(Sx),0))
  sf1[n,n-1] <- Lx[n]/(Lx[n]+Lx[n-1])
  sf1[n,n] <- ifelse(fin, lt[n,"Tx"]/lt[n-1,"Tx"], 0)
  A <- sf1

  ffab<-ifelse(sex == "m", 105/205, 100/205)
  names(fx) <- age2
  fert = sapply(age1, FUN = function(x){ifelse(x %in% age2, fx[paste0(x)],0)})
  for(j in 1:(n-1)){
    A[1,j]<-(Lx[1]/2*lt[1,"lx"])*(fert[j]+fert[j+1]*Sx[j] )*ffab
  }

  return(A)
}
