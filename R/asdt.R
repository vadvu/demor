#' Associated single decrement life table (ASDT) for causes of death (cause-deleted life table)
#'
#' @param age Numeric array of age intervals; for full life table = `0:100`; for concise life table = `c(0:1, seq(5,85,5))`
#' @param sex Character. Sex. "m" for males or "f" for females.
#' @param m_all Numeric array with age specific mortality rates of all causes of death (usual mx).
#' @param m_i Numeric array with age specific mortality rates of some cause of death (i)
#' @param full Logical. Is full table needed? TRUE = full, FALSE = concise
#' @param method Character. The method of ASDT construction to use. Now just "chiang1968" is supported.
#' @param ax Optional. Numeric array with ax. By default, it is a the middle of the interval, while ax for age 0 is modeled as in Andreev & Kingkade (2015).
#' @references Chiang, L. (1968). *Introduction to Stochastic Processes in Biostatistics*. New York: John Wiley and Sons.
#' @return dataframe.
#' @export
asdt <- function(age, sex, m_all, m_i, full = F, method = "chiang1968", ax = NULL){
  lt <- as.data.frame(LT(age = age, sex = sex, mx = m_all, ax = ax))
  lt$r_not_i <- 1-m_i/m_all
  lt$p_not_i <- (1-lt$qx)^lt$r_not_i
  lt$l_not_i <- 1
  for (i in 2:nrow(lt)){
    lt[i,]$l_not_i <- lt[i-1,]$l_not_i*lt[i-1,]$p_not_i
  }
  lt$n = c(diff(lt$age,1),NA)
  lt$a_not_i <- NA
  for(i in 1:nrow(lt)){
    if(i==nrow(lt)){
      lt[i,]$a_not_i <- lt[i,]$ex/lt[i,]$r_not_i
    } else {
      lt[i,]$a_not_i <- lt[i,]$n + lt[i,]$r_not_i * (lt[i,]$qx/(1-lt[i,]$p_not_i))*(lt[i,]$ax - lt[i,]$n )
    }
  }
  lt[i,]$a_not_i <- round(lt[i,]$a_not_i,3)
  lt$d_not_i <- lt$l_not_i * (1-lt$p_not_i)
  lt$L_not_i = lt$n * lt$l_not_i - lt$n * (1 - lt$a_not_i) * lt$d_not_i
  lt[nrow(lt),]$L_not_i = lt[nrow(lt),]$l_not_i * lt[nrow(lt),]$a_not_i
  lt$T_not_i = rev(cumsum(rev(lt$L_not_i)))
  lt$ex_without_i <- round(lt$T_not_i / lt$l_not_i, 2)
  if (full == T){
    return(lt)
  }else{
    return(lt[,c("age", "r_not_i", "lx", "qx", "ax", "ex", "p_not_i", "l_not_i", "a_not_i", "ex_without_i")])
  }
}
