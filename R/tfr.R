#' Total Fertility Rate (TFR)
#'
#' @param fx Numeric array of age specific fertility rates.
#' @param age.int Numeric. Age group: `1` for one-year, `5` for five-year. Any age groups are allowed.
#'
#' @return A length-1 numeric value equal to the sum of age-specific fertility
#'   rates multiplied by `age.int`.
#' @examples
#' fx <- c(0.02, 0.08, 0.12, 0.09, 0.04, 0.01, 0.001)
#' tfr(fx, age.int = 5)
#' @export
tfr <- function(fx, age.int = 1){
  fx <- as.numeric(c(fx))
  return(sum(fx)*age.int)
}
