#' Total Fertility Rate (TFR)
#'
#' @param fx Vector of age specific fertility rates.
#' @param age.int Numeric. Age group: `1` for one-year, `5` for five-year. Any age groups are allowed.
#'
#' @return Numeric value.
#' @export
tfr <- function(fx, age.int = 1){
  fx <- as.numeric(fx)
  return(sum(fx)*age.int)
}
