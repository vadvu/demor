#' Life table with corrected nax
#'
#' @param age Array of age intervals; for full life table = `0:100`; for concise life table = `c(0:1, seq(5,85,5))`
#' @param sex Sex. "m" for males or "f" for females.
#' @param mx Age specific mortality rates.
#'
#' @return Matrix
#' @export
LT <- function(age, sex, mx) {
  mx [mx < 0] = 0
  last <- length(mx)
  nx <- c(diff(age), 1)
  ax <- rep(0.5, last)
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
  if (mx[last] > 0) {
    ax[last] <- 1 / mx[last]
  }
  qx <- (nx * mx) / (1 + nx * (1 - ax) * mx)
  qx[last] <- 1
  px <- 1 - qx
  lx <- 1
  for (i in 1:(last - 1)) {
    lx[i + 1] <- lx[i] * px[i]
  }
  dx <- lx * qx
  dx[last] <- lx[last]
  Lx = nx * lx - nx * (1 - ax) * dx
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
  return(lt)
}
