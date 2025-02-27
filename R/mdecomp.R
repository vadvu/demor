#' Age and cause decomposition of differences in life expectancies
#'
#' @param mx1 List of numeric arrays. 1st array should be all-cause nmx in the 1st population, other arrays are cause-specific nmx in the 1st population
#' @param mx2 List of numeric arrays. 1st array should be all-cause nmx in the 2nd population, other arrays are cause-specific nmx in the 2nd population
#' @param age Numeric array of age intervals; for full life table = `0:100`; for concise life table = `c(0:1, seq(5,85,5))`
#' @param method Character. Decomposition method. "andreev" (1982) or "arriaga" (1984) - slightly different in their results. By default, `method = "andreev"`.
#' @param ... Optional. Additional arguments for `decomp` function.
#'
#' @return Dataframe with 1st column as overall decomposition (`ex12`), and other columns are decompositions by causes (`cause(i)`)
#' @export
mdecomp <- function(mx1, mx2, age, method = "andreev", ...){
  if (length(mx1)!=length(mx2)){
    stop("lengths (number of causes) of mx1 and mx2 are not equal")
  }
  if (length(mx1[[1]])!=length(mx2[[1]]) | length(mx1[[1]])!=length(age)){
    stop("lengths (number of age-groups) of mx1 and mx2 are not equal")
  }
  ex12 <- decomp(mx1[[1]], mx2[[1]], age = age, method = method, ...)$ex12
  mdec <- data.frame(age = age, ex12 = ex12)
  for (i in 2:length(mx1)){
    mdec[paste0(names(mx1)[i])] <- NA
    mdec[paste0(names(mx1)[i])] <- ex12 * (mx1[[i]] - mx2[[i]]) / (mx1[[1]] - mx2[[1]])
  }
  return(mdec)
}
