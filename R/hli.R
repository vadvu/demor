#' The Human Life Indicator (HLI)
#' Calculating HLI as in Ghislandi, S., Sanderson, W.C., Scherbov, S., 2019. A Simple Measure of Human Development: The Human Life Indicator. Population and Development Review 45, 219–233.. https://doi.org/10.1111/padr.12205
#' @param age Array of age intervals; for full life table = `0:100`; for concise life table = `c(0:1, seq(5,85,5))`
#' @param sex Sex. "m" for males or "f" for females.
#' @param mx Age specific mortality rates.
#'
#' @return value
#' @export
hli <- function(age, sex, mx){
  lt <- LT(age = age, sex = sex, mx = mx)
  val <- exp(sum(log(lt[,1] + lt[,3])*lt[,6]))
  return(val)
}
