#' Gini coefficient of a life table
#'
#' @param age Numeric array of age intervals
#' @param mx Numeric array with age specific mortality rates.
#' @param ... Optional. Additional arguments for [LT()] function.
#' @return list with G0 (Gini coefficient), G0_abs (absolute Gini coefficient) and data for the Lorenz curve ($plot)
#' @export
gini <- function(age, mx, ...){
  lt <- LT(age = age, mx = mx, ...)
  Fx = rep(0, nrow(lt))
  Phix = rep(0, nrow(lt))
  for(i in 1:(nrow(lt) - 1)){
    Fx[i] = 1 - lt[i+1,"lx"]/lt[1,"lx"]
    Phix[i] = ( lt[1,"Tx"] - (lt[i,"Tx"] + lt[i,"age"]*lt[i,"lx"]) )/lt[1,"Tx"]
  }
  Fx[nrow(lt)] = Phix[nrow(lt)] = 1
  Fx[1] = Phix[1] = 0
  G0 = 0
  for(i in  1:(nrow(lt))){
    for(j in 1:(nrow(lt))){
      G0 = G0 + (1/(2*lt[1,"ex"]*lt[1,"lx"]^2)) *
        (lt[i,"dx"] * lt[j,"dx"]) *
        abs(lt[i,"age"] + lt[i,"ax"] - lt[j,"age"] - lt[j,"ax"])
    }
  }
  G0_abs = G0 * lt[1,"ex"]
  return(list(Gini = list(G0 = G0, G0_abs = G0_abs), plot = data.frame(Fx, Phix)))
}
