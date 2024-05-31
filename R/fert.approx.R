#' Fertility models for ASFR approximation
#'
#' @param fx Numeric vector of age specific fertility rates.
#' @param age Numeric vector of ages.
#' @param model Character. Model name to be estimated. Now "Hadwiger" and "Gamma" are supported.
#' @param boot Logical. Should bootstrapped 95% confidence intervals for ASFR approximation be calculated. Default is `FALSE` for no bootstrap.
#' @import dplyr
#' @return list with estimated model (parameters, R-squred, variance-covariance matrix of parameters) and dataframe with predicted ASFR.
#'
#' @details
#' This function runs least squares optimization of the selected fertility function using Port algorithm with 2000 maximum iterations
#' and 1e-07 as tolerance parameter.
#' ## Hadwiger model
#' The model is as follows: \deqn{f(age) = \frac{ab}{c} \frac{c}{age}^{3/2} exp[-b^2(\frac{c}{age}+\frac{age}{c}-2)]}
#' ## Gamma model
#' The model is as follows: \deqn{f(age) = \frac{R}{\Gamma(b)c^b}(age-d)^{b-1} exp[-(\frac{age-d}{c})]}
#' @references
#' Peristera, P., & Kostaki, A. (2007). Modeling fertility in modern populations. *Demographic Research*, *16*, 141-194.
#' @export
#'
#' @examples
#'
#' # fert.approx(fx = ASFR. age = 15:55, model = "Hadwiger", boot= F)
#'
fert.approx <- function(fx, age, model, boot = F){

  if(length(age) != length(fx)){
    stop("age and fx do not have the same length")
  }
  if(!(model %in% c("Hadwiger", "Gamma"))){
    stop("model can be only 'Hadwiger' or 'Gamma'")
  }

  if(model == "Hadwiger"){
    form <- as.formula(paste0("fx ~ (a*b/c)*(c/age)^(1.5) * exp(-b^2*(c/age + age/c - 2))"))
    start.val = list(a = 0.1, b = 0.1, c = 0.1)
    lower.val = c(0,0,0)
    upper.val = c(999,999,999)
  }
  if(model == "Gamma"){
    form <- as.formula(paste0("fx ~ R*( 1/( gamma(b)*c^b ) ) * (age-d)^(b-1) * exp( -1*( (age-d)/c ) )"))

    mygamma2 <- function(pars){
      R = pars[1]; b = pars[2]; c = pars[3]; d = pars[4];
      fx.pred = R*( 1/( gamma(b)*c^b ) ) * (age-d)^(b-1) * exp( -1*( (age-d)/c ) )
      sum((fx-fx.pred)^2)
    }
    m1 <- optim(par = c(tfr(fx, unique(diff(age))), 1, 1, min(age)), fn = mygamma2)
    start.val = list(R = m1$par[1], b = m1$par[2], c = m1$par[3], d = m1$par[4])
    lower.val = c(0,0,0,0)
    upper.val = c(999,999,999,999)
  }

  est <- nls(formula = form,
             start=start.val,
             lower = lower.val,
             upper = upper.val,
             algorithm = "port",
             control = list(maxiter = 2000, tol = 1e-07)
  )
  predicted = predict(est)
  Rsq = cor(fx, predict(est))

  if(boot){
    for(i in 1:999){
      bs = sample(resid(est), size = length(age), replace = T)
      bs = ifelse(predicted + bs < 0, 0, predicted + bs)
      if(i == 1){
        pred.boot = data.frame(age = age, pred = bs, i = i)
      }else{
        pred.boot = rbind(pred.boot, data.frame(age = age, pred = bs, i = i))
      }
    }

    ci <- pred.boot %>% dplyr::group_by(age) %>%
      dplyr::mutate(low_2.5prc = quantile(pred, 0.025),
                    high_97.5prc = quantile(pred, 0.975)) %>%
      as.data.frame() %>%
      dplyr::distinct(age, .keep_all = T) %>%
      dplyr::mutate(predicted = predicted) %>%
      dplyr::select(age, predicted, low_2.5prc, high_97.5prc)

    predicted = ci

  }else{
    predicted = data.frame(age = age, predicted = predicted)
  }

  return(list(
    model = list(
      type = model,
      params = coef(est),
      Rsq = Rsq,
      covmat = vcov(est)
    ),
    predicted = predicted
  )
  )
}
