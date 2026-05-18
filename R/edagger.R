#' e-dagger
#'
#' @param age Numeric array of age intervals
#' @param mx Numeric array with age specific mortality rates.
#' @param ... Optional. Additional arguments for [LT()] function.
#'
#' @return A named numeric vector of the same length as `age`, where each
#'   element is `e^\dagger_x`, the average remaining life years lost because of
#'   death from age `x` onward.
#' @examples
#' age <- 0:5
#' mx <- c(0.02, 0.01, 0.012, 0.015, 0.02, 0.03)
#' edagger(age, mx)[1:3]
#' @export
edagger <- function(age, mx, ...){
  lt <- LT(age = age, mx = mx, ...)
  ed = rep(0, length(age))
  for(i in 1:(length(ed) - 1)){
    ed[i] <- (1/lt[i,"lx"]) *
      sum( lt[i:(length(ed)-1),"dx"] *
             # ( lt[(i+1):(length(ed)), "ex"] + 1 - lt[i:(length(ed)-1),"ax"] )
             ( lt[(i+1):length(ed), "ex"] + lt[i:(length(ed)-1),"ex"] ) / 2
      ) +
      lt[length(ed),"dx"] / 2*lt[i,"lx"]
  }
  names(ed) <- age
  return(ed)
}
