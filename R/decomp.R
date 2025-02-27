
#' Age decomposition of mortality: single decrement process
#'
#' @param mx1 Numeric array with age specific mortality rates of population 1 (base population).
#' @param mx2 Numeric array with age specific mortality rates of population 2 (compared population).
#' @param sex Character. Sex. "m" for males and "f" for females. By default, `sex = "m"`.
#' @param age Numeric array of age intervals; for full life table = `0:100`; for concise life table = `c(0:1, seq(5,85,5))`
#' @param method Character. Decomposition method. "andreev" (1982), "arriaga" (1984) or "pollard" (1982) - slightly different in their results. By default, `method = "andreev"`.
#' @param ax1 Optional. Numeric array with ax for the 1st population. By default, it is a the middle of the interval, while ax for age 0 is modeled as in Andreev & Kingkade (2015).
#' @param ax2 Optional. Numeric array with ax for the 2nd population. By default, it is a the middle of the interval, while ax for age 0 is modeled as in Andreev & Kingkade (2015).
#' @references
#' 1. Arriaga, E. E. (1984). Measuring and explaining the change in life expectancies. *Demography*, *21*, 83-96.
#' 2. Андреев Е.М. (1982). Метод компонент в анализе продолжительности жизни. *Вестник статистики*, *9*, 42-47.
#' 3. Pollard, J. H. (1982). The expectation of life and its relationship to mortality. *Journal of the Institute of Actuaries*, *109(2)*, 225–240.
#' @import dplyr
#' @return dataframe with parameters of decomposition (depends on method) and decomposition in years (ex12) and percents (ex12_prc).
#' @export
decomp <- function(mx1, mx2, sex = "m", age, method = "andreev", ax1 = NULL, ax2 = NULL) {
  if (!(method %in% c("andreev", "arriaga", "pollard"))){stop("Choose method (misspelling?)")}
  lt1 <- LT(age = age, sex = sex, mx = mx1, ax = ax1)
  lt2 <- LT(age = age, sex = sex, mx = mx2, ax = ax2)
  if (method == "andreev") {
    comp = data.frame(
      age = age,
      ex1 = lt1[, "ex"],
      ex2 = lt2[, "ex"],
      lx2 = lt2[, "lx"]
    )
    comp$dex = dex_1 = comp$ex2 - comp$ex1
    c1_1 = lt2[,"lx"] * dex_1
    ex12_1 = round(c(rev(diff(rev(c1_1))), c1_1[length(c1_1)]), 2)

    dex_2 = comp$ex1 - comp$ex2
    c1_2 = lt1[,"lx"] * dex_2
    ex12_2 = round(c(rev(diff(rev(c1_2))), c1_2[length(c1_2)]), 2)

    comp$ex12 = round(0.5*(ex12_1 - ex12_2), 2)
    comp$ex12_prc = 100 * comp$ex12 / sum(comp$ex12)

  } else if (method == "arriaga"){
    comp = data.frame(
      age = age,
      l1 = lt1[, "lx"],
      L1 = lt1[, "Lx"],
      T1 = lt1[, "Tx"],
      l2 = lt2[, "lx"],
      L2 = lt2[, "Lx"],
      T2 = lt2[, "Tx"]
    )
    comp$ex12 = ex12_1 = ex12_2 = NA
    ex12_1 = ( lt1[,"lx"] / lt1[1,"lx"] ) * ( lt2[,"Lx"] / lt2[,"lx"] - lt1[,"Lx"] / lt1[,"lx"] ) +
      ( dplyr::lead(lt2[,"Tx"]) / lt1[1,"lx"] ) * ( lt1[,"lx"] / lt2[,"lx"] - dplyr::lead(lt1[,"lx"] / lt2[,"lx"]) )
    ex12_1[length(ex12_1)] = (lt1[nrow(lt1),"lx"] / lt1[1,"lx"]) * ( lt2[nrow(lt1),"Lx"] / lt2[nrow(lt1),"lx"] - lt1[nrow(lt1),"Lx"] / lt1[nrow(lt1),"lx"] )

    lt10 <- lt2
    lt2 <- lt1
    lt1 <- lt10
    ex12_2 = ( lt1[,"lx"] / lt1[1,"lx"] ) * ( lt2[,"Lx"] / lt2[,"lx"] - lt1[,"Lx"] / lt1[,"lx"] ) +
      ( dplyr::lead(lt2[,"Tx"]) / lt1[1,"lx"] ) * ( lt1[,"lx"] / lt2[,"lx"] - dplyr::lead(lt1[,"lx"] / lt2[,"lx"]) )
    ex12_2[length(ex12_2)] = (lt1[nrow(lt1),"lx"] / lt1[1,"lx"]) * ( lt2[nrow(lt1),"Lx"] / lt2[nrow(lt1),"lx"] - lt1[nrow(lt1),"Lx"] / lt1[nrow(lt1),"lx"] )

    comp$ex12 <- round(0.5*(ex12_1 - ex12_2), 2)
    comp$ex12_prc = 100 * comp$ex12 / sum(comp$ex12)

  } else if(method == "pollard"){
    comp = data.frame(
      age = age,
      ex1 = lt1[, "ex"],
      ex2 = lt2[, "ex"]
    )
    comp$wx <- 0.5 * ( cumprod(1 - lt2[,"qx"]) * lt1[,"ex"] + cumprod(1 - lt1[,"qx"]) * lt2[,"ex"])
    Qx1 <- -log(dplyr::lead(lt1[,"lx"]) / lt1[,"lx"])
    Qx2 <- -log(dplyr::lead(lt2[,"lx"]) / lt2[,"lx"])
    Qx1[length(Qx1)] = Qx2[length(Qx1)] = 0
    comp$Qx1 = Qx1
    comp$Qx2 = Qx2
    comp$ex12 = round( (Qx1 - Qx2) * comp$wx, 2)
    comp$ex12_prc = 100 * comp$ex12 / sum(comp$ex12)
  }

  return(comp)
}
