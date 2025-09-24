#' Age and cause decomposition of differences in life expectancies
#'
#' @param mx1 List of numeric arrays. 1st array should be all-cause nmx in the 1st population, other arrays are cause-specific nmx in the 1st population
#' @param mx2 List of numeric arrays. 1st array should be all-cause nmx in the 2nd population, other arrays are cause-specific nmx in the 2nd population
#' @param age Numeric array of age intervals; for full life table = `0:100`; for concise life table = `c(0:1, seq(5,85,5))`
#' @param method Character. Decomposition method. "andreev" (1982) or "arriaga" (1984) - slightly different in their results. By default, `method = "andreev"`.
#' @param ... Optional. Additional arguments for [decomp()].
#' @details
#' The contribution of each cause \eqn{c} to the absolute difference in life expectancies between the first and second population is caculated as
#' \deqn{\Delta_{x,c} = \frac{m^1_{x,c} - m^2_{x,c}}{m^1_{x} - m^2_{x}} \times \Delta_{x}}
#' where \eqn{\Delta_{x}} is contribution of age \eqn{x} to difference \eqn{e_0^2 - e_0^1} from function [decomp()], \eqn{m^i_{x,c}} is age-specific mortality rate for population \eqn{i} from cause \eqn{c}, and \eqn{m^i_x} is total age-specific mortality rate.
#' @seealso [decomp()] for just age decomposition and [plot.mdecomp()] for graph of `mdecomp` results
#' @return Dataframe with 1st column as overall decomposition (`ex12`), and other columns are decomposition by causes (`cause(i)`)
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
  class(mdec) <- c("mdecomp", "data.frame")
  return(mdec)
}
