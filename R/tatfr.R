#' Tempo-adjusted total fertility rate (TFR')
#'
#' @param past_fx List with numeric arrays of age specific fertility rates for period t-1 by parity
#' @param present_fx List with numeric arrays of age specific fertility rates for period t by parity (it is period of interest)
#' @param post_fx List with numeric arrays of age specific fertility rates for period t+1 by parity
#' @param age Array with numeric values age
#'
#' @return list with TFR' (tatfr) and TFR' by parity (tatfr_i, in user-specific order as in lists), TFR (tfr) and TFR by parity (tfr_i)
#' @export
tatfr <- function(past_fx, present_fx, post_fx, age){
  parity = length(past_fx)
  present_tfr = rt = tai = rep(NA, parity)

  for(i in 1:parity){
    present_tfr[i] <- demor::tfr(fx = present_fx[[i]], age.int = median(diff(age)))

    rt[i] <- demor::mac(fx = post_fx[[i]], age = age) - demor::mac(fx = past_fx[[i]], age = age)
    rt[i] <- rt[i]/2

    tai[i] = present_tfr[i] / (1-rt[i])
  }

  return(list(tatfr = sum(tai), tatfr_i = tai, tfr = sum(present_tfr), tfr_i = present_tfr))
}
