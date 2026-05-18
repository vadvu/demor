#' The Human Life Indicator (HLI)
#'
#' @param age Numeric array of age intervals; for full life table = `0:100`; for concise life table = `c(0:1, seq(5,85,5))`
#' @param mx Numeric array with age specific mortality rates.
#' @param ... Optional. Additional arguments for [LT()] function.
#' @references Ghislandi, S., Sanderson, W.C., & Scherbov, S. (2019). A Simple Measure of Human Development: The Human Life Indicator. *Population and Development Review*, *45*, 219–233.
#' @details
#' It is calculated as
#' \deqn{HLI = \prod_{x=\alpha}^{\omega}(x + a_x)^{d_x}}
#' where \eqn{\alpha, \omega} are the first and last age groups, \eqn{x} is age, \eqn{a_x, d_x} are life table functions (s.t. \eqn{\sum_{x=\alpha}^{\omega} d_x = 1}).
#' @return A length-1 numeric value giving the Human Life Indicator, i.e. the
#'   geometric mean age at death implied by the life table.
#' @examples
#' age <- 0:5
#' mx <- c(0.02, 0.01, 0.012, 0.015, 0.02, 0.03)
#' hli(age, mx)
#' @export
hli <- function(age, mx, ...){
  lt <- LT(age = age, mx = mx, ...)
  if(round(sum(lt[,"dx"]), 2) > 1){
    lt[,"dx"] <- lt[,"dx"] / sum(lt[,"dx"])
  }
  val <- exp(sum(log(lt[,"age"] + lt[,"ax"])*lt[,"dx"]))
  return(val)
}
