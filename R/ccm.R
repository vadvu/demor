#' Cohort Component Model for Projecting Population
#'
#' @param Mx Matrix or dataframe with mx, where row represents age, column represents a forecasted period. Thus, first column should be the first projected period, while each row represents age-specific mortality rate.
#' @param Fx Matrix or dataframe with fx, where row represents age, column represents a forecasted period. Thus, first column should be the first projected period, while each row represents age-specific fertility rate.
#' @param Ix Optional. Matrix or dataframe with net number of migrants, where row represents age, column represents a forecasted period. Thus, first column should be the first projected period, while each row represents net number of migrants.
#' @param age.mx Numeric vector. Age groups for Mx rows.
#' @param age.fx Numeric vector. Age groups for Fx rows.
#' @param N0 Numeric vector. Population in the initial period by age groups.
#' @param ... Optional. Additional arguments for `leslie` function.
#'
#' @return Matrix with projected population. Columns represent periods, rows represent age groups. The first column is `N0` (initial period).
#' @export
ccm <- function(Mx, Fx, Ix = NULL, age.mx, age.fx, N0, ...){

  if(ncol(Mx) != ncol(Fx)){
    stop("Number of periods (columns) in Mx and Fx differs, prepare data carefully")
  }else if(nrow(Mx) != length(N0)){
    stop("Number of age groups (rows) in Mx and N0 differs, prepare data carefully")
  }

  if(is.null(Ix)){
    Ix = matrix(0, length(N0), ncol(Mx))
  }else if (nrow(Mx) != nrow(Ix)){
    stop("Number of age groups (rows) in Mx and Ix differs, prepare data carefully")
  }else if (ncol(Mx) != ncol(Ix)){
    stop("Number of periods (columns) in Mx and Ix differs, prepare data carefully")
  }

  dat <- cbind(N0)

  for(t in 1:ncol(Mx)){
    les.t <- leslie(mx = Mx[,t], fx = Fx[,t], age.mx = age.mx, age.fx = age.fx, ...)
    dat = cbind(dat, N0)
    dat[,t+1] = les.t %*% (dat[,t] + Ix[,t]/2) + Ix[,t]/2
  }
  dat

}
