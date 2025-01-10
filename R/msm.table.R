#' Multi-state life table based on multi-state markov model from msm package
#'
#' @param model `msm` model. See details.
#' @param ages Numeric vector of ages.
#' @param start Start values of lx for each group presented as numeric vector.
#' @param covs List of (age-invariant) covariates as for `msm` models.
#' @details
#' `msm` is a Multi-state Markov model in continuous time, where age is both time and covariate.
#' For example, `model = msm::msm(state~age, subject = id, covariates = ~ age + sex, data = data, ...)`
#'
#' @import msm
#' @return Table with age (x), mx (transition rates to death), lx (root of the table = 1), Lx, Tx and ex for all groups and the whole population.
#' @export
#' @examples
#' # model <- msm::msm(state~age, subject = id, covariates = ~ age + sex, data = data, ...)
#' # table <- msm.table(model = model, ages = 20:80, start = c(1,0,0), covs = list(sex = 1))
#'
msm.table <- function(model, ages, start, covs){

  #this function implements linear method

  ns = ncol(model$Qmatrices$baseline) - 1
  newmat = data.frame(age = ages, lx = 1)

  for(i in 1:ns){
    newmat[paste0("m", i, "x")] <- NA
  }

  for(i in 1:ns){
    newmat[paste0("l", i, "x")] <- NA
  }
  newmat$ldx <- NA
  newmat[1,(3+ns):(2 + 2*ns)] <- start
  newmat[1,]$ldx <- 1 - sum(start)

  for(i in 1:ns){
    newmat[paste0("L", i, "x")] <- NA
  }

  for(i in 1:ns){
    newmat[paste0("T", i, "x")] <- NA
  }

  for(i in 1:(nrow(newmat)-1)){
    covs$age = ages[i]
    ndif = ifelse(i == 1, 1, ages[i] - ages[i-1])

    Qxn =  msm::qmatrix.msm(model, ci = "none", covariates = covs)
    Mxn = -1*Qxn

    mx <- Mxn[1:(nrow(Mxn)-1), ncol(Mxn)]
    newmat[i,3:(2+ns)] <- abs(mx)

    lx = diagmat(unlist(newmat[i,(3+ns):(3+2*ns)]))
    pr.mat = (idmat(ns+1) - 0.5 * ndif * Mxn) %*% solve(idmat(ns+1) + 0.5 * ndif * Mxn)
    lxn = lx %*% pr.mat
    Lxn = 0.5 * ndif * (lx + lxn)

    Lx <- diagmat(colSums(Lxn))

    lx <- diagmat(colSums(lxn))

    newmat[i+1,(3+ns):(3 + 2*ns)] <- diag(lx)

    newmat[i,(4 + 2*ns):(4 + 2*ns + ns - 1)] <- diag(Lx)[-ncol(Lx)]
  }

  covs$age = ages[length(ages)]
  Mxn = -1*msm::qmatrix.msm(model, ci = "none", covariates = covs)
  lx = diagmat(unlist(newmat[length(ages),(3+ns):(3 + 2*ns)]))
  newmat[length(ages),(4 + 2*ns):(4 + 2*ns + ns - 1)] <- diag(lx[-ncol(lx),-ncol(lx)] %*% solve(Mxn[-ncol(lx),-ncol(lx)]))

  newmat$lx <- rowSums(newmat[,(3+ns):(3 + 2*ns - 1)])

  for(i in 1:ns){
    newmat[paste0("T", i, "x")] <- rev(cumsum(rev(unlist(newmat[paste0("L", i, "x")]))))
  }
  for(i in 1:ns){
    newmat[paste0("e", i, "x")] <- round(newmat[paste0("T", i, "x")]/newmat$lx,2)
  }
  newmat$ex <- rowSums(newmat[,(3 + 4*ns + 1):ncol(newmat)])
  return(newmat)
}
