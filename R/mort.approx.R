#' Mortality models for mx approximation
#'
#' @param mx Numeric vector of age specific mortality rates.
#' @param age Numeric vector of ages.
#' @param model Character. Model name to be estimated. Now "Gompertz" and "Brass" are supported.
#' @param standard.mx Numeric vector of age specific mortality rates for standard population. Default is `NULL`.
#' @param boot Logical. Should bootstrapped 95% confidence intervals for mx approximation be calculated. Default is `FALSE` for no bootstrap. Do not work for `Gompertz` model.
#'
#' @return list with estimated model (parameters, R-squred, variance-covariance matrix of parameters) and dataframe with predicted mortality rates.
#' @import dplyr
#'
#' @details
#' This function runs least squares optimization of the selected mortality function using Gauss-Newton algorithm algorithm with 2000 maximum iterations
#' and 1e-07 as tolerance parameter. For "Gompertz" usual OLS estimator is used.
#' ## Gompertz model
#' The model is as follows: \deqn{m(age) = \alpha e^{\beta age}}
#' ## Gamma model
#' The model is as follows: \deqn{y(age) = \alpha + \beta y^{S}(age)}
#' where \deqn{y(age) = \frac{1}{2} ln[\frac{q(age)}{1-q(age)}]}
#' and subscript S defines that function is for standard population. To get mx from qx usual formula is used:
#' \deqn{m(age) = \frac{q(age)}{n-q(age)(n-a(age))}} where n is the size of age interval and a(x) is a parameter from life table.
#'
#' @references Preston, S. H., Heuveline, P., & Guillot, M. (2001). Demography: Measuring and modeling population processes. Blackwell Publishers. ([pdf](https://gwern.net/doc/statistics/2001-preston-demography.pdf))
#' @export
#'
#' @examples
#'
#' # mort.approx(mx = mx, age = 0:100, model = "Brass", standard.mx = standard.mx, boot = T)
#'
mort.approx <- function(mx, age, model, standard.mx = NULL, boot = F){

  if(length(age) != length(mx)){
    stop("age and mx do not have the same length")
  }
  if(!(model %in% c("Gompertz", "Brass"))){
    stop("model can be only 'Gompertz' or 'Brass'")
  }

  if(model == "Gompertz"){
    mod <- lm(log(mx) ~ age)
    par <- coef(mod)
    names(par) <- c("ln(a)", "b")
    predicted = predict(mod)

    return(list(
      model = list(
        type = model,
        params = par,
        covmat = vcov(mod)
      ),
      predicted = data.frame(age = age, mx.pred_ln = exp(predicted))
    ))

  }

  if(model == "Brass"){
    if(is.null(standard.mx)){
      stop("For Brass model standard mx are required")
    }
    if(length(standard.mx) != length(mx)){
      stop("Standard mx and mx do not have the same length")
    }
    qx <- demor::LT(age, sex = "m", mx)[,"qx"]
    qx <- qx[-length(qx)]
    qx.stand = demor::LT(age, sex = "m", standard.mx)[,"qx"]
    ax.stand =
      qx.stand <- qx.stand[-length(qx.stand)]
    stand.logit = 0.5*log(qx.stand/(1-qx.stand))

    est <- nls( 0.5*log((qx/(1-qx))) ~ a + b*stand.logit, start = list(a = 0, b = 1))
    qx.pred = c(exp(2*predict(est))/(1+exp(2*predict(est))),1)
    ax.stand = demor::LT(age, sex = "m", standard.mx)[,"ax"]
    n = diff(age)
    n = c(n, mean(n))
    mx.pred = qx.pred/(n - qx.pred*(n - ax.stand))


    if(boot){
      for(i in 1:999){
        bs = predict(est) + sample(resid(est), size = length(age)-1, replace = T)
        bs = c(exp(2*bs)/(1+exp(2*bs)),1)
        bs = bs/(n - bs*(n - ax.stand))
        if(i == 1){
          pred.boot = data.frame(age = age,
                                 pred = bs,
                                 i = i)
        } else {
          pred.boot = rbind(pred.boot, data.frame(age = age, pred = bs, i = i))
        }
      }
      ci <- pred.boot %>% dplyr::group_by(age) %>%
        dplyr::mutate(low_2.5prc = quantile(pred, 0.025),
                      high_97.5prc = quantile(pred, 0.975)) %>%
        as.data.frame() %>%
        dplyr::distinct(age, .keep_all = T) %>%
        dplyr::mutate(mx.pred = mx.pred) %>%
        dplyr::select(age, mx.pred, low_2.5prc, high_97.5prc)

      predicted = ci

    }else{
      predicted = data.frame(age = age, mx.pred = mx.pred)
    }

    return(list(
      model = list(
        type = model,
        params = coef(est),
        covmat = vcov(est)
      ),
      predicted = predicted
    )
    )
  }
}
