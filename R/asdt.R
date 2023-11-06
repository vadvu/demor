#' (IN DEMO!) Associated single decrement life table (ASDT) for causes of death (cause-deleted life table)
#'
#' @param age Array of age intervals; for full life table = `0:100`; for concise life table = `c(0:1, seq(5,85,5))`
#' @param sex Sex. "m" for males or "f" for females.
#' @param m_all Age specific mortality rates of all causes of death
#' @param m_i Age specific mortality rates of some cause of death (i)
#' @param full Logical. Is full table needed? TRUE = full, FALSE = concise
#' @param method The method of ASDT construction to use. Now just "chiang1968" is supported.
#' @return dataframe
#' @export
asdt <- function(age, sex, m_all, m_i, full = F, method = "chiang1968"){
  lt <- as.data.frame(LT(age = age, sex = sex, mx = m_all))
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
