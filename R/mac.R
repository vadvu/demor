#' Mean Age at Childbearing (MAC)
#'
#' @param fx Numeric array of age specific fertility rates.
#' @param age Numeric array of ages. For example, `15:55` for 1-year age-groups
#'
#' @return A length-1 numeric value giving the mean age at childbearing implied
#'   by `fx`.
#' @examples
#' age <- seq(15, 45, 5)
#' fx <- c(0.03, 0.10, 0.14, 0.12, 0.07, 0.03, 0.01)
#' mac(fx, age)
#' @export
mac <- function(fx, age){
  fx <- as.numeric(c(fx))
  delta <- (diff(age)[1] * fx)/tfr(fx, diff(age)[1])
  fin = sum( (age + 0.5*diff(age)[1])*delta )
  return(round(fin, 2))
}
