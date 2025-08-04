#' Fertility models for ASFR approximation
#'
#' @param fx Numeric vector of age specific fertility rates.
#' @param age Numeric vector of ages.
#' @param model Character. Model name to be estimated. Now "Hadwiger", "Gamma", "Brass" and "Beta" are supported.
#' @param se Logical. Should bootstrapped variance for ASFR approximation be calculated. Default is `FALSE` for no bootstrap.
#' @param start Numeric vector with user-specific values of parameters for optimization. Default is `NULL` (choose automatically)
#' @param alpha Numeric. Used if `se = TRUE`, the level of uncertainty. By default, `alpha = 0.05` for 95% CI.
#' @param bn Numeric. Used if `se = TRUE`, number of bootstrap samples. By default, `bn = 1000`.
#' @return list with estimated model (parameters, variance-covariance matrix, percentiles of parameters) and dataframe with predicted and observed ASFR as well as SE and percentile of predictions
#'
#' @details
#' This function runs least squares optimization (using default `optim`) of the selected fertility function with 1e-06 as tolerance parameter.
#'
#' \eqn{f_x} is age-specific fertility rate for age \eqn{x}.
#'
#' ## Hadwiger model
#' The model is as follows: \deqn{f_x = \frac{ab}{c} \frac{c}{x}^{3/2} exp[-b^2(\frac{c}{x}+\frac{x}{c}-2)]}
#' where \eqn{a,b,c} are estimated parameters that do not have demographic interpretation. Sometimes \eqn{c} is interpreted as mean age at childbearing.
#'
#' ## Gamma model
#' The model is as follows: \deqn{f_x = \frac{R}{\Gamma(b)c^b}(x-d)^{b-1} exp[-(\frac{x-d}{c})]}
#' where \eqn{R,b,c,d} are estimated parameters. \eqn{\Gamma} is gamma function. \eqn{R} can be interpreted as fertility level (TFR) and \eqn{d} as mean age at childbearing.
#'
#' ## Brass model
#' The model is as follows: \deqn{f_x = c(x-d)(d+w-x)}
#' where \eqn{c,d,w} are estimated parameters.
#'
#' ## Beta model
#' The model is as follows: \deqn{f_x = \frac{R}{\Beta(A,C)}(\beta - \alpha)^{-(A+C-1)}(x-\alpha)^{(A-1)}(\beta-x)^{(B-1)}}
#' where \eqn{\Beta} is beta function, \eqn{R, \beta, \alpha} are estimated parameters, which can be interpreted as fertility level (TFR) and max and min age of childbearing respectively.
#' \eqn{A,C} are
#' \deqn{C = (\frac{(v - \alpha)(\beta - v)}{\tau^2} - 1)\frac{\beta - v}{\beta - \alpha}}
#' \deqn{A = C\frac{v-\alpha}{v - \beta}}
#' where \eqn{v, \tau^2} are estimated parameters, where \eqn{v} can be interpreted as mean age at childbearing. Thus, Beta model uses 5 parameters \eqn{R, \beta, \alpha, v, \tau^2}, where only \eqn{\tau^2} has no demographic interpretation.
#'
#' @references
#' Peristera, P., & Kostaki, A. (2007). Modeling fertility in modern populations. *Demographic Research*, *16*, 141-194.
#'
#' @export
#'
#' @examples
#'
#' # fert.approx(fx = ASFR, age = 15:55, model = "Hadwiger", se = FALSE)
#'
fert.approx <- function(fx, age, model, start = NULL, se = FALSE, alpha = 0.05, bn = 1000){

  ##### Checks

  if(length(age) != length(fx)){
    stop("age and fx do not have the same length")
  }
  if(!(model %in% c("Hadwiger", "Gamma", "Brass", "Beta"))){
    stop("model can be only 'Hadwiger', 'Gamma', 'Brass' or 'Beta'")
  }

  ##### Models

  hadw <- function(pars, age){
    a = pars[1]; b = pars[2]; c = pars[3]
    fx.pred = (a*b/c)*(c/age)^(1.5) * exp(-b^2*(c/age + age/c - 2))
    fx.pred
  }
  mygamma <- function(pars, age){
    R = pars[1]; b = pars[2]; c = pars[3]; d = pars[4]
    fx.pred = R*( 1/( gamma(b)*c^b ) ) * (age-d)^(b-1) * exp( -1*( (age-d)/c ) )
    fx.pred
  }
  brass <- function(pars, age){
    c = pars[1]; d = pars[2]; w = pars[3]
    fx.pred = c*(age - d)*(d+w-age)^2
    fx.pred
  }
  mybeta <- function(pars, age){
    a = pars[1]; b = pars[2]; v = pars[3]; tau.sq = pars[4]; R = pars[5]
    B = ((v-a)*(b-v)/tau.sq - 1)*((b-v)/(b-a))
    A = B*(v - a)/(b-v)
    fx.pred =  R*(1/beta(A, B)) * (b - a)^-(A+B-1) * (age-a)^(A-1) * (b-age)^(B-1)
    fx.pred
  }

  ##### Starting parameters

  if(!is.null(start)){
    first.pars <- start
  }else if (model == "Hadwiger"){
    first.pars <- c(tfr(fx, unique(diff(age))),
                    (max(fx)*mac(fx, age))/tfr(fx, unique(diff(age))),
                    mac(fx, age)) #a, b, c
  }else if(model == "Gamma"){
    first.pars <- c(tfr(fx, unique(diff(age))), 1, 1, min(age)) #R, b, c ,d
  }else if(model == "Brass"){
    first.pars <- c(1, min(age), max(age)-min(age)) #c, d, w
  }else if(model == "Beta"){
    first.pars <- c(min(age), max(age), mac(fx, age), mac(fx, age)+2, tfr(fx, unique(diff(age)))) #a, b, v, tau.sq, R
  }

  ##### Estimator

  estimate <- function(mf, pars, fx, age){

    objective <- function(pars, fx, age) {
      fx.pred <- mf(pars, age = age)
      sum((fx-fx.pred)^2)
    }

    m1 <- optim(par = pars,
                fn = objective, fx = fx, age = age,
                control =  list(maxit = 1e6))
    m2 <- optim(par = m1$par,
                fn = objective, fx = fx, age = age,
                control =  list(maxit = 1e6))

    while(m1$value - m2$value > 1e-06){
      m1 <- m2
      m2 <- optim(par = m1$par,
                  fn = objective, fx = fx, age = age,
                  control =  list(maxit = 1e6))
    }
    m2
  }


  ##### Results
  if(model == "Brass"){
    ffn <- brass
    nms <- c("c", "d", "w")
  }else if(model == "Hadwiger"){
    ffn <- hadw
    nms <- c("a", "b", "c")
  }else if(model == "Gamma"){
    ffn <- mygamma
    nms <- c("R", "b", "c" ,"d")
  }else if(model == "Beta"){
    ffn <- mybeta
    nms <- c("a", "b", "v", "tau.sq", "R")
  }
  est <- estimate(mf = ffn, first.pars, fx = fx, age = age)
  names(est$par) <- nms

  #### SE
  if(se == TRUE){
    for(i in 1:bn){
      samplei <- sample(1:length(fx), length(fx), replace = T)
      esti = estimate(mf = ffn, first.pars, fx = fx[samplei], age = age[samplei])$par
      if(i == 1){
        bootpars = matrix(esti, nrow = 1)
        bootpred = matrix(ffn(esti, age = age), ncol = 1)
      }else{
        bootpars = rbind(bootpars, esti)
        bootpred = cbind(bootpred, ffn(esti, age = age))
      }
    }
    covmat = cov(bootpars)
    prc.pars = apply(bootpars, 2, quantile, probs = alpha/2, na.rm = TRUE)
    prc.pars = t(rbind(prc.pars, apply(bootpars, 2, quantile, probs = 1-alpha/2, na.rm = TRUE)))
    rownames(prc.pars) <- nms
    colnames(prc.pars) <- c("prc.low", "prc.high")
    se.pred = diag(var(t(bootpred), na.rm = T))^0.5

    return(list(
      model = list(
        type = model,
        params = est$par,
        covmat = covmat,
        prc = prc.pars,
        rmse = sqrt(sum((ffn(est$par, age = age) - fx)^2) / (length(age) - length(est$par)))
      ),
      predicted = data.frame(age = age, fx.model = ffn(est$par, age = age), fx = fx, se = se.pred,
                             prc.low = apply(t(bootpred), 2, quantile, probs = alpha/2, na.rm = TRUE),
                             prc.high = apply(t(bootpred), 2, quantile, probs = 1-alpha/2, na.rm = TRUE)
      )
    )
    )
  }else{
    return(list(
      model = list(
        type = model,
        params = est$par,
        rmse = sqrt(sum((ffn(est$par, age = age) - fx)^2) / (length(age) - length(est$par)))
      ),
      predicted = data.frame(age = age, fx.model = ffn(est$par, age = age), fx = fx)
    )
    )
  }
}
