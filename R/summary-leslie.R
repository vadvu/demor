#' Leslie matrix summary
#'
#' @param object A leslie matrix from [leslie()].
#' @param d Time step. By default it is 1. Only affects \eqn{r}.
#' @param ... Ignored.
#' @details
#' The function calculates \eqn{\lambda}, \eqn{r}, \eqn{w} and \eqn{v}.
#' \itemize{
#'    \item \eqn{\lambda} -- asymptotic growth factor that is dominant eigenvalue.
#'    \item \eqn{r} -- asymptotic growth rate that is \eqn{ln \lambda / \delta t}.
#'    \item \eqn{w} -- stable age distribution normalized to 1 s.t. \eqn{\sum_x^{\omega} w_x = 1} where \eqn{x} is age.
#'    \item \eqn{v} -- reproductive values normalized s.t. \eqn{v'w = 1}.
#' }
#'
#' @seealso [leslie()]
#' @export
#' @method summary leslie
summary.leslie <- function(object, d = 1, ...){
  ev <- eigen(object)
  k <- which.max(Re(ev$values))

  # lambda = exp growth rate
  lambda <- Re(ev$values[k])

  # stable age dist
  w <- Re(ev$vectors[,k])
  w <- w / sum(w)

  # reproductive value
  evL <- eigen(t(object))
  kL <- which.max(Re(evL$values))
  v <- Re(evL$vectors[,kL])
  v <- v / as.numeric(t(v) %*% w)

  out <- list(
    lambda = lambda,
    r = log(lambda)/d,
    w = w,
    v = v
    )
  class(out) <- "summary.leslie"
  out
}
