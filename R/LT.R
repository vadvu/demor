#' Life table
#'
#' @param age Numeric array of age intervals; for full life table = `0:100`; for concise life table = `c(0:1, seq(5,85,5))`
#' @param sex Character. Sex. `"m"` for males or `"f"` for females. By default = `"m"`.
#' @param mx Numeric array with age specific mortality rates.
#' @param ax Optional. Numeric array with ax. By default, it is the middle of the interval, while ax for age 0 is modeled as in Andreev & Kingkade (2015).
#' @param w Optional. Numeric array with weights for each age interval for calculating weighted life expectancy (`wex`).
#' @param l0 Numeric. Life table radix. By default, = `1` but it can be any positive real number. In "human" demography tradition it is 100'000, in "ecological" and "evolutionary" demography tradition it is 1.
#' @details
#' By default, \eqn{a_x} for age 0 (first entity in `ax`) is modeled as in Andreev & Kingkade (2015, p. 390, see table 3-2).
#'
#' The weighted life expectancy is calculated as follows:
#' \deqn{e_x^w = \frac{\sum_{i = x}^{\omega}L_x w_x}{l_x}}
#' where \eqn{\omega} is the last age, \eqn{w} is weight s.t. \eqn{w \in [0,1]}, and other variables are life table functions.
#'
#' @references Andreev, E. M., & Kingkade, W. W. (2015). Average age at death in infancy and infant mortality level: Reconsidering the Coale-Demeny formulas at current levels of low mortality. *Demographic Research*, *33*, 363-390.
#' @return Matrix of (age x 9). Columns are: age, mx, ax, qx, lx, dx, Lx, Tx, ex
#' @export
LT <- function(age, sex = "m", mx, ax = NULL, w = NULL, l0 = 1) {
  if(length(mx) != length(age)){
    stop("The length of user-specific array of age does not equal mx array's length!")
  }
  if(any(mx < 0)){
    mx[mx < 0] = 0
    warning("There are mx < 0, replace them with 0")
  }
  last <- length(mx)
  nx <- c(diff(age), 1)
  if(is.null(ax)){
    ax <- nx * 0.5
    if (sex == "m") {
      if (mx[1] < 0.02300) {
        ax[1] <- 0.14929 - (1.99545 * mx[1])
      }
      if ((0.0230 <= mx[1]) &
          (mx[1] < 0.08307)) {
        ax[1] <- 0.02832 + (3.26021 * mx[1])
      }
      if (0.08307 <= mx[1]) {
        ax[1] <- 0.29915
      }
    }
    if (sex == "f") {
      if (mx[1] < 0.01724) {
        ax[1] <- 0.14903 - (2.05527 * mx[1])
      }
      if ((0.01724 <= mx[1]) &
          (mx[1] < 0.06891)) {
        ax[1] <- 0.04667 + (3.88089 * mx[1])
      }
      if (0.06891 <= mx[1]) {
        ax[1] <- 0.31411
      }
    }
  }else{
    if(length(ax) != length(mx)){
      stop("The length of user-specific array of ax does not equal mx array's length!")
    }
  }

  if (mx[last] > 0) {
    ax[last] <- 1 / mx[last]
  }
  qx <- (nx * mx) / (1 + (nx - ax) * mx)
  qx[last] <- 1
  px <- 1 - qx
  lx <- l0
  for (i in 1:(last - 1)) {
    lx[i + 1] <- lx[i] * px[i]
  }
  dx <- lx * qx
  dx[last] <- lx[last]
  Lx = nx * lx - ax * dx
  Lx[last] = lx[last] * ax[last]
  Tx = rev(cumsum(rev(Lx)))
  ex <- Tx / lx
  lt <-
    as.matrix(
      data.frame (
        age,
        mx = round(mx, 5),
        ax = round(ax, 3),
        qx = round(qx, 5),
        lx = round(lx, 5),
        dx = round(dx, 5),
        Lx = round(Lx, 5),
        Tx = round(Tx, 5),
        ex = round(ex, 2)
      )
    )
  if(!is.null(w)){
    if(length(age) != length(w)){
      stop("The length of user-specific array of weights does not equal age array's length!")
    }else{
      wLx = Lx*w
      wex <- round((1/lx) * rev(cumsum(wLx)), 2)
      lt <- cbind(lt, w, wLx = round(wLx, 5), wex)
    }
  }
  return(lt)
}
