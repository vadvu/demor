
#' Age decomposition of mortality: single decrement
#'
#' @param mx1 Numeric array with age specific mortality rates of population 1 (basic population).
#' @param mx2 Numeric array with age specific mortality rates of population 2 (compared population).
#' @param sex Character. Sex. "m" for males and "f" for females
#' @param age Numeric array of age intervals; for full life table = `0:100`; for concise life table = `c(0:1, seq(5,85,5))`
#' @param method Character. Decomposition method. "andreev" (1982) or "arriaga" (1984) - slightly different in their results.
#' @param ax1 Optional. Numeric array with ax for the 1st population. By default, it is a the middle of the interval, while ax for age 0 is modeled as in Andreev & Kingkade (2015).
#' @param ax2 Optional. Numeric array with ax for the 2nd population. By default, it is a the middle of the interval, while ax for age 0 is modeled as in Andreev & Kingkade (2015).
#' @references
#' 1. Arriaga, E. E. (1984). Measuring and explaining the change in life expectancies. *Demography*, *21*, 83-96.
#' 2. Андреев Е.М. (1982). Метод компонент в анализе продолжительности жизни. *Вестник статистики*, *9*, 42-47.
#'
#' @return dataframe with some parameters of decomposition (depends on method) and decomposition in years or percents.
#' @export
decomp <- function(mx1, mx2, sex, age, method = "andreev", ax1 = NULL, ax2 = NULL) {
  if (method != "andreev" & method != "arriaga"){stop("Choose method (misspelling?)")}
  lt1 <- LT(age = age, sex = sex, mx = mx1, ax = ax1)
  lt2 <- LT(age = age, sex = sex, mx = mx2, ax = ax2)
  if (method == "andreev") {
    comp = data.frame(
      age = age,
      ex1 = lt1[, "ex"],
      ex2 = lt2[, "ex"],
      lx2 = lt2[, "lx"]
    )
    comp$dex = comp$ex2 - comp$ex1
    comp$ex12 = NA
    for (i in 1:nrow(comp)) {
      if (i == nrow(comp)) {
        comp[i, ]$ex12 = comp[i, ]$lx2 * comp[i, ]$dex
      } else {
        comp[i, ]$ex12 = comp[i, ]$lx2 * comp[i, ]$dex - comp[i + 1, ]$lx2 * comp[i +
                                                                                    1, ]$dex
      }
    }
    comp$ex12 <- round(comp$ex12, 2)
    comp$ex12_prc = 100 * comp$ex12 / sum(comp$ex12)
  }
  else {
    comp = data.frame(
      age = age,
      l1 = lt1[, "lx"],
      L1 = lt1[, "Lx"],
      T1 = lt1[, "Tx"],
      l2 = lt2[, "lx"],
      L2 = lt2[, "Lx"],
      T2 = lt2[, "Tx"]
    )
    comp$ex12 = NA
    for (i in 1:nrow(comp)) {
      if (i == nrow(comp)) {
        comp[i, ]$ex12 = comp[i, ]$l1 * ( (comp[i, ]$T2 / comp[i, ]$l2) - (comp[i, ]$T1 /comp[i, ]$l1) )
      } else{
        comp[i, ]$ex12 = comp[i, ]$l1 * ( (comp[i, ]$L2 / comp[i, ]$l2) - (comp[i, ]$L1 / comp[i, ]$l1) ) +
          comp[i + 1, ]$T2 * ( (comp[i, ]$l1 / comp[i, ]$l2) - (comp[i+1, ]$l1 / comp[i+1, ]$l2) )
      }
    }
    comp$ex12 <- round(comp$ex12, 2)
    comp$ex12_prc = 100 * comp$ex12 / sum(comp$ex12)
  }
  return(comp)
}
