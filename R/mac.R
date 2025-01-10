#' Mean Age at Childbearing (MAC)
#'
#' @param fx Numeric array of age specific fertility rates.
#' @param age Numeric array of ages. For example, `15:55` for 1-year age-groups
#'
#' @return Numeric value.
#' @export
mac <- function(fx, age){
  fx <- as.numeric(fx)
  delta <- (diff(age)[1] * fx)/tfr(fx, diff(age)[1])
  fin = sum( (age + 0.5*diff(age)[1])*delta )
  return(round(fin, 2))
}
