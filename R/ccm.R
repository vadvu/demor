#' Cohort Component Model for Projecting Population.
#'
#' @param Mx.f Matrix or dataframe with mx for females, where row represents age, column represents a forecasted period. Thus, first column should be the first projected period, while each row represents age-specific mortality rate.
#' @param Mx.m Optional. Matrix or dataframe with mx for males, where row represents age, column represents a forecasted period. Thus, first column should be the first projected period, while each row represents age-specific mortality rate.
#' @param Fx Matrix or dataframe with fx, where row represents age, column represents a forecasted period. Thus, first column should be the first projected period, while each row represents age-specific fertility rate.
#' @param Ix.f Optional. Matrix or dataframe with net number of female migrants, where row represents age, column represents a forecasted period. Thus, first column should be the first projected period, while each row represents net number of migrants.
#' @param Ix.m Optional. Matrix or dataframe with net number of male migrants, where row represents age, column represents a forecasted period. Thus, first column should be the first projected period, while each row represents net number of migrants.
#' @param age.mx Numeric vector. Age groups for Mx.f rows.
#' @param age.fx Numeric vector. Age groups for Fx rows.
#' @param N0.f Numeric vector. Female population in the initial period by age groups.
#' @param N0.m Optional. Numeric vector. Male population in the initial period by age groups.
#' @param srb Numeric. Sex ratio at birth for females. By default, it is 100/205.
#' @param ... Optional. Additional arguments for [leslie()] function.
#' @details
#' The model is calculated in matrix form as
#' \deqn{\mathbf{N}_{t+h} = \mathbf{L}_t(\mathbf{N}_{t} + \mathbf{I}_{t}/2) + \mathbf{I}_{t}/2}
#' where \eqn{\mathbf{N}_t} is a column vector of population for time \eqn{t} with \eqn{h} as a step of projection (it is the length of age interval), \eqn{\mathbf{L}_t} is Leslie matrix and \eqn{\mathbf{I}_{t}} is a column vector of net migration.
#'
#' Note that the model assumes that in \eqn{\mathbf{N}} all age intervals are the same (i.e. \eqn{age = \{0-1, 1-4, 5-9, ...\}} is not permitted). Fortunately (and thanks to me), the function handles such situations automatically by transforming all age groups into a unified standard.
#'
#' @return (List of) Matrices with projected population. Columns represent periods, rows represent age groups. The first column is `N0` (initial period).
#' @export
ccm <- function(Mx.f, Mx.m = NULL, Fx, Ix.f = NULL, Ix.m = NULL, age.mx, age.fx, N0.f, N0.m = NULL, srb = 100/205, ...){

  # checks
  if(ncol(Mx.f) != ncol(Fx)){
    stop("Number of periods (columns) in Mx.f and Fx differs, prepare data carefully")
  }else if(nrow(Mx.f) != length(N0.f)){
    stop("Number of age groups (rows) in Mx.f and N0 differs, prepare data carefully")
  }
  n <- length(age.mx)
  h <- ncol(Mx.f) - 1
  if(is.null(Ix.f)){
    Ix.f = matrix(0, n, h)
  }else if (nrow(Mx.f) != nrow(Ix.f)){
    stop("Number of age groups (rows) in Mx and Ix differs, prepare data carefully")
  }else if (ncol(Mx.f) != ncol(Ix.f)){
    stop("Number of periods (columns) in Mx and Ix differs, prepare data carefully")
  }

  n0 <- length(age.mx)
  newage = NULL
  if(length(unique(diff(age.mx))) > 1){
    ux <- unique(diff(age.mx))
    agegroup <- ux[which.max(tabulate(match(diff(age.mx), ux)))]
    newage <- seq(0, max(age.mx), agegroup)
    newage0 <- newage
    newage0[1] = newage0[2] - 1

    N0.f <- cbind(N0.f, newage = demor::ages(x = age.mx, groups = newage0, char = FALSE))
    N0.f[which(N0.f[,"newage"] == newage0[1]), "newage"] <- newage[1]
    N0.f <- aggregate(data = N0.f, N0.f ~ newage, FUN = sum)
    N0.f <- N0.f[,"N0.f"]
    if(!is.null(N0.m)){
      N0.m <- cbind(N0.m, newage = demor::ages(x = age.mx, groups = newage0, char = FALSE))
      N0.m[which(N0.m[,"newage"] == newage0[1]), "newage"] <- newage[1]
      N0.m <- aggregate(data = N0.m, N0.m ~ newage, FUN = sum)
      N0.m <- N0.m[,"N0.m"]
    }

    Ix.f <- cbind(Ix.f, newage = demor::ages(x = age.mx, groups = newage0, char = FALSE))
    Ix.f <- apply(Ix.f, MARGIN = 2, FUN = function(x){
      ft <- cbind(x, newage = demor::ages(x = age.mx, groups = newage0, char = FALSE))
      ft[which(ft[,"newage"] == newage0[1]), "newage"] <- newage[1]
      st <- aggregate(data = ft, x ~ newage, FUN = sum)
      st$x
    })
    if(is.null(Ix.m)){
      Ix.m <- Ix.f
      Ix.m <- matrix(0, nrow = nrow(Ix.f), ncol = ncol(Ix.f))
    }else{
      Ix.m <- cbind(Ix.m, newage = demor::ages(x = age.mx, groups = newage0, char = FALSE))
      Ix.m <- apply(Ix.m, MARGIN = 2, FUN = function(x){
        ft <- cbind(x, newage = demor::ages(x = age.mx, groups = newage0, char = FALSE))
        ft[which(ft[,"newage"] == newage0[1]), "newage"] <- newage[1]
        st <- aggregate(data = ft, x ~ newage, FUN = sum)
        st$x
      })
    }
  }

  dat.f <- cbind(N0.f)
  for(t in 1:h){
    les.t <- leslie(mx = Mx.f[,t], fx = Fx[,t], age.mx = age.mx, age.fx = age.fx, srb = srb, sex = "f", ...)
    dat.f = cbind(dat.f, N0.f)
    dat.f[,t+1] = les.t %*% (dat.f[,t] + Ix.f[,t]/2) + Ix.f[,t]/2
  }


  if(!is.null(Mx.m)){

    if(is.null(Ix.m)){
      Ix.m <- Ix.f
      Ix.m <- matrix(0, nrow = nrow(Ix.f), ncol = ncol(Ix.f))
    }

    dat.m <- cbind(N0.m)
    for(t in 1:h){
      les.t <- leslie(mx = Mx.m[,t], fx = Fx[,t], age.mx = age.mx, age.fx = age.fx, srb = 1-srb, sex = "m", ...)
      B.t <- les.t[1,] %*% (dat.f[,t] + Ix.f[,t]/2)
      les.t[1,] <- 0
      dat.m = cbind(dat.m, N0.m)
      dat.m[,t+1] = les.t %*% (dat.m[,t] + Ix.m[,t]/2) + Ix.m[,t]/2
      dat.m[1,t+1] <- B.t
    }
  }

  if(is.null(Mx.m)){
    rownames(dat.f) <- if(is.null(newage)) age.mx else newage
    colnames(dat.f) <- 0:h
    return(dat.f)
  }else{
    rownames(dat.f) <- rownames(dat.m) <- if(is.null(newage)) age.mx else newage
    colnames(dat.f) <- colnames(dat.m) <- 0:h
    list(
      female = dat.f,
      male = dat.m,
      all = dat.f + dat.m
    )
  }

}
