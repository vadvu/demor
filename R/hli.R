#' The Human Life Indicator (HLI)
#'
#' @param age Numeric array of age intervals; for full life table = `0:100`; for concise life table = `c(0:1, seq(5,85,5))`
#' @param mx Numeric array with age specific mortality rates.
#' @param ... Optional. Additional arguments for `LT` function.
#' @references Ghislandi, S., Sanderson, W.C., & Scherbov, S. (2019). A Simple Measure of Human Development: The Human Life Indicator. *Population and Development Review*, *45*, 219–233.
#' @return HLI value
#' @export
hli <- function(age, mx, ...){
  lt <- LT(age = age, mx = mx, ...)
  val <- exp(sum(log(lt[,"age"] + lt[,"ax"])*lt[,"dx"]))
  return(val)
}
