#' Mean Age at Childbearing (MAC)
#'
#' @param fx Vector of age specific fertility rates.
#' @param ages Vector of ages. For example, `15:55` for 1-year age-groups
#'
#' @return Numeric value.
#' @export
mac <- function(fx, ages){
  fx <- as.numeric(fx)
  delta <- (diff(ages)[1] * fx)/tfr(fx, diff(ages)[1])
  return(sum( (ages + 0.5*diff(ages)[1])*delta ))
}
