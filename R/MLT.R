#' Multiple Decrement Life Table
#'
#' @param age Numeric array of age intervals; for full life table = `0:100`; for concise life table = `c(0:1, seq(5,85,5))`
#' @param mx List of numeric arrays. 1st array should be all-cause mx in the population, other arrays are cause-specific mx in the population.
#' @param ... Other parameters for the function `LT()` including `ax` (by default, the middle of the interval), `sex` (by default = `"m"` - males), `l0` (by default = 1).
#'
#' @return Extended `LT` matrix
#' @export
MLT <- function(age, mx, ...){
  if(length(mx)<2){
    stop("In the list should be at least 2 arrays (1 - mx, 2 - mx_i, ...)")
  }
  lt <- LT(age = age, mx = mx[[1]], ...)
  for(i in names(mx)[-1]){
    qx_i = round(lt[,"qx"]*mx[[i]]/lt[,"mx"],5)
    lt <- cbind(lt, qx_i)
    colnames(lt)[ncol(lt)] <- paste0("qx_", i)
    dx_i = round(lt[,paste0("qx_", i)]*lt[,"lx"],5)
    lt <- cbind(lt, dx_i)
    colnames(lt)[ncol(lt)] <- paste0("dx_", i)
    lx_i = round(rev(cumsum(rev(lt[,paste0("dx_", i)]))),5)
    lt <- cbind(lt, lx_i)
    colnames(lt)[ncol(lt)] <- paste0("lx_", i)
    ex_i = asdt(age = age, m_all = mx[[1]], m_i = mx[[i]], full = F, ...)[,"ex_without_i"]
    lt <- cbind(lt, ex_i)
    colnames(lt)[ncol(lt)] <- paste0("ex_no_", i)
  }
  return(lt)
}
