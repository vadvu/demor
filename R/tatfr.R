#' Tempo-adjusted total fertility rate (TFR')
#'
#' @param past_fx List with numeric arrays of age specific fertility rates for period t-1 by parity
#' @param present_fx List with numeric arrays of age specific fertility rates for period t by parity (it is period of interest)
#' @param post_fx List with numeric arrays of age specific fertility rates for period t+1 by parity
#' @param age Array with numeric values age
#' @details
#' This indicator is calculated as follows
#' \deqn{TFR_{i,t}' = \frac{TFR_{i,t}}{1-(M_{i,t+1} - M_{i,t-1}) / 2}}
#' where \eqn{TFR_{i,t}', TFR_{i,t}} are tempo-adjusted and usual total fertility rate for parity \eqn{i} and time \eqn{t} respectively, \eqn{M_{i,t}} is mean age at childbearing for parity \eqn{i} and time \eqn{t}. The tempo-adjusted total fertility rate is a sum of parity-specific \eqn{TFR_i'}.
#'
#' Note, the calculation are done as in footnote 1 in (Bongaarts & Feeney, 2000, p. 563). Unfortunately, the original 1998 article does not provide the exact formula, which has caused some confusion in academic circles.
#'
#' @seealso [tfr()] for TFR and [mac()] for mean age at childbearing calculation.
#'
#' @references
#' Bongaarts, J., & Feeney, G. (1998). On the Quantum and Tempo of Fertility. Population and Development Review, 24(2), 271–291. https://doi.org/10.2307/2807974
#'
#' Bongaarts, J., & Feeney, G. (2000). On the Quantum and Tempo of Fertility: Reply. Population and Development Review, 26(3), 560–564. https://doi.org/10.1111/j.1728-4457.2000.00560.x
#'
#' @return list with TFR' (tatfr) and TFR' by parity (tatfr_i, in user-specific order as in lists), TFR (tfr) and TFR by parity (tfr_i)
#' @export
tatfr <- function(past_fx, present_fx, post_fx, age){
  parity = length(past_fx)
  present_tfr = rt = tai = rep(NA, parity)

  for(i in 1:parity){
    present_tfr[i] <- demor::tfr(fx = present_fx[[i]], age.int = stats::median(diff(age)))

    rt[i] <- demor::mac(fx = post_fx[[i]], age = age) - demor::mac(fx = past_fx[[i]], age = age)
    rt[i] <- rt[i]/2

    tai[i] = present_tfr[i] / (1-rt[i])
  }

  return(list(tatfr = sum(tai), tatfr_i = tai, tfr = sum(present_tfr), tfr_i = present_tfr))
}
