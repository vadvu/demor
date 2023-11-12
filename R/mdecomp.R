#' Age and cause decomposition of differences in life expectancies
#'
#' @param mx1 List of arrays. 1st array should be all-cause nmx in the 1st population, other arrays are cause-specific nmx in the 1st population
#' @param mx2 List of arrays. 1st array should be all-cause nmx in the 2nd population, other arrays are cause-specific nmx in the 2nd population
#' @param age Array of age intervals; for full life table = `0:100`; for concise life table = `c(0:1, seq(5,85,5))`
#' @param sex Sex. "m" for males and "f" for females
#'
#' @return Dataframe with 1st column as overall decomposition (`ex12`), and other columns are decompositions by causes (`cause(i)`)
#' @export
mdecomp <- function(mx1, mx2, age, sex){
  if (length(mx1)!=length(mx2)){
    stop("lengths (number of causes) of mx1 and mx2 are not equal")
  }
  if (length(mx1[[1]])!=length(mx2[[1]]) | length(mx1[[1]])!=length(age)){
    stop("lengths (number of age-groups) of mx1 and mx2 are not equal")
  }
  ex12 <- decomp(mx1[[1]], mx2[[1]], age = age, sex = sex)$ex12
  mdec <- data.frame(age = age, ex12 = ex12)
  for (i in 2:length(mx1)){
    mdec[paste0(names(mx1)[i])] <- NA
    for(j in 1:nrow(mdec)){
      mdec[j,ncol(mdec)] <- mdec[j,2]*( mx2[[i]][j]-mx1[[i]][j] ) / ( mx2[[1]][j]-mx1[[1]][j] )
    }
  }
  return(mdec)
}
