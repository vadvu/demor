#' Leslie Matrix
#'
#' @param mx Vector of age specific mortality rates.
#' @param fx Vector of age specific fertility rates.
#' @param age1 Vector of ages for mx.
#' @param age2 Vector of ages for fx.
#' @param sex Sex. "m" for males or "f" for females.
#'
#' @return Matrix.
#' @export
leslie <- function(mx, fx, age1, age2, sex){
  lt <- LT(age = age1, sex = sex, mx = mx)
  n <- length(age1)

  A <- matrix(0, n, n)
  Sx <- array(0,c(n-1))
  Lx <- lt[,"Lx"]
  for (i in 1:n-1){
    Sx[i]<-(Lx[i+1]/Lx[i])
  }
  sf1<-rbind(0,cbind(diag(Sx),0))
  sf1[n,n]<-sf1[n,n-1]<-Lx[n]/(Lx[n]+Lx[n-1])
  ffab<-ifelse(sex == "m", 105/205, 100/205)
  names(fx) <- age2
  fert = sapply(age1, FUN = function(x){ifelse(x %in% age2, fx[paste0(x)],0)})

  for(j in 1:n-1){
    sf1[1,j]<-(Lx[1]/2)*(fert[j]+fert[j+1]*Sx[j] )*ffab
  }

  sf1[n,n]<-0
  A<-sf1
  return(A)
}
