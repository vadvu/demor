#' Median age calculation
#'
#' @param N Numeric array. Population counts by age groups (from young to old)
#' @param age Numeric array. Lower bounds of age groups, same length as `N`
#'
#' @return Numeric value.
#' @export
med.age <- function(N, age){
  if(length(age) != length(N)){
    stop("lengths of age groups (age) and Population counts (N) are not the same")
  }
  cN <- cumsum(N)
  m <- cN[length(N)]/2
  inter <- which(cN >= m)[1]
  prev_cN <- ifelse(inter == 1, 0, cN[inter - 1])
  int <- ifelse(inter == length(age), max(diff(age)), age[inter+1] - age[inter])
  fin <- age[inter] + (m - prev_cN) / N[inter] * int
  return(round(fin, 2))
}
