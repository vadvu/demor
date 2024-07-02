#' Median age calculation
#'
#' @param N Array with numeric values of population by age group (from young to old)
#' @param age Array with numeric values age
#' @param int Numeric value of age interval. For example, 1 or 5
#'
#' @return Numeric value
#' @export
med.age <- function(N, age, int = 1){
  cN <- cumsum(N)
  m <- cN[length(age)]/2
  inter <- c()
  for(i in 1:length(age)){
    inter[i] <- ifelse(cN[i] > m, 1, 0)
    if(inter[i] == 1){
      inter[i+1] <- ifelse(cN[i+1] > m, 2, 0)
      break
    }
  }
  ages <- age[which(inter %in% c(1,2))]
  int/2 + (ages[1]+int/2) + (m-cN[which(age == ages[1])])/
    (cN[which(age == ages[2])]-cN[which(age == ages[1])])
}
