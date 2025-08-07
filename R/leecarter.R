#' Lee-Carter model
#'
#' @param data Dataframe in the long format with the following columns: `age`, `year`, `mx` (age specific mortality rates). For some types of `ktadj` argument `N` (population at age x) and `Dx` (number of deaths at age x) columns should also be presented.
#' @param n Numeric. Forecasted horizon
#' @param alpha Numeric. The level of uncertainty. By default, `alpha = 0.05` for 95% CI.
#' @param model Character. Model type for kt forecasting. Can be "RWwD" for random walk with drift (by default, for original Lee-Carter model) or "ARIMA" for ARIMA model which parameters are chosen automatically by `forecast::auto.arima()`.
#' @param ax_method Character. Method for ax calculation. Can be "classic" from original Lee-Carter model (by default), "last" or "last_smooth". See details.
#' @param bx_method Character. Method for bx calculation. Can be "classic" from original Lee-Carter model (by default) and "rotate" for rotating bx (Li et al., 2013).
#' @param boot Logical. Should bootstrap estimates for uncertainty be used? `FALSE` by default.
#' @param bn Numeric. Used if `boot = TRUE`, number of bootstrap samples. By default, `bn = 1000`.
#' @param ktadj Character. Type of `kt` adjustment. It can be set to 'none' (defaukt, no adjustment), 'Dmin', 'e0min', 'poisson' or 'edaggermin' (see Details). Note that 'Dmin' and 'poisson' require data on the age-specific number of deaths (`Dx` column in the data) and the age-specific population (`N` column in the data).
#' @param ... Optional. Additional arguments for [LT()] function.
#' @details
#' The \strong{`model`} argument specifies the forecasting method.
#' \itemize{
#'   \item `model ="RWwD"` – classic random walk option
#'   \item `model = "ARIMA"` for selecting a more complex time series model
#' }
#'
#' The \strong{`ax_method`} argument allows to control how `a_x` is calculated.
#' \itemize{
#'   \item `ax_method = "classic"` – classic option with the average of the logarithm of mortality rates (but there is so-called "jump-off bias").
#'   \item `ax_method = "last"` uses the logarithm of mortality for the last available year (as proposed in Lee & Miller, 2001).
#'   \item `ax_method = "last_smooth"` uses data for the last year with smoothing (see Ševčíková et al., 2016, p. 288).
#' }
#'
#' The \strong{`bx_method`} argument allows to control how `b_x` is calculated.
#' \itemize{
#'   \item `bx_method = "classic"` for the original method.
#'   \item `bx_method = "rotate"` for the rotational variant (see Li et al., 2013).
#' }
#'
#' The \strong{`ktadj`} argument allows to control how `k_t` is calculated.
#' \itemize{
#'   \item `ktadj = "none"` for no adjustment.
#'   \item `ktadj = "Dmin"` for minimizing the deviance of predicted/actual annual deaths (as proposed in the original Lee-Carter paper). This method requires data on the age-specific number of deaths (`Dx` column in the data) and the age-specific population (`N` column in the data).
#'   \item `ktadj = "e0min"` for minimizing the deviance of predicted/actual life expectancy (as proposed in Lee & Miller, 2001).
#'   \item `ktadj = "poisson"` for minimizing the deviance from a Poisson model, where the dependent variable is the age-specific annual number of deaths (as proposed in Booth et al., 2002). This method requires data on the age-specific number of deaths (`Dx` column in the data) and the age-specific population (`N` column in the data).
#'   \item `ktadj = "edaggermin"` for minimizing the deviance of predicted/actual edagger (see [edagger()]) as proposed in Rabbi & Mazzuco, 2021.
#' }
#'
#' @references
#'
#' Booth, H., Maindonald, J., & Smith, L. (2002). Applying Lee-Carter under conditions of variable mortality decline. Population studies, 56(3), 325-336. \href{https://doi.org/10.1080/00324720215935}{https://doi.org/10.1080/00324720215935}
#'
#' Lee, R. D., & Carter, L. R. (1992). Modeling and forecasting US mortality. Journal of the American Statistical Association, 87(419), 659–671. \href{https://doi.org/10.1080/01621459.1992.10475265}{https://doi.org/10.1080/01621459.1992.10475265}
#'
#' Lee, R., & Miller, T. (2001). Evaluating the performance of the lee-carter method for forecasting mortality. Demography, 38(4), 537–549. \href{https://doi.org/10.1353/dem.2001.0036}{https://doi.org/10.1353/dem.2001.0036}
#'
#' Li, N., Lee, R., & Gerland, P. (2013). Extending the Lee-Carter Method to Model the Rotation of Age Patterns of Mortality Decline for Long-Term Projections. Demography, 50(6), 2037–2051. \href{https://doi.org/10.1007/s13524-013-0232-2}{https://doi.org/10.1007/s13524-013-0232-2}
#'
#' Rabbi, A. M. F., & Mazzuco, S. (2021). Mortality forecasting with the lee–carter method: Adjusting for smoothing and lifespan disparity. European Journal of Population, 37(1), 97-120. \href{https://doi.org/10.1007/s10680-020-09559-9}{https://doi.org/10.1007/s10680-020-09559-9}
#'
#' Ševčíková, H., Li, N., Kantorová, V., Gerland, P., & Raftery, A. E. (2016). Age-Specific Mortality and Fertility Rates for Probabilistic Population Projections. In R. Schoen (Ed.), Dynamic Demographic Analysis (Vol. 39, pp. 285–310). Springer International Publishing. \href{https://doi.org/10.1007/978-3-319-26603-9_15}{https://doi.org/10.1007/978-3-319-26603-9_15}
#'
#' @return Dataframe with the projected mx and ex for t+n periods with mean, low95 and high 95 values
#' @import forecast dplyr tidyr splines
#' @export
leecart <- function(data, n = 10, alpha = 0.05, model = "RWwD", ax_method = "classic", bx_method = "classic", boot = FALSE, bn = 1000, ktadj = "none", ...){

  ##### Checks
  `%notin%` <- Negate(`%in%`)
  if("year" %notin% colnames(data) | "age" %notin% colnames(data) | "mx"  %notin% colnames(data)){
    stop("Please, prepare your data carefully")
  }else{
    data <- data %>% arrange(year, age)
  }
  if(sum(data$mx == 0)>0){
    warning("0 are in mx vector, linear interpolation is done")
    mxtoextr <- which(data$mx == 0)
    for(j in mxtoextr){
      data1 <- data[data$year == data[j,]$year,]
      lh <- data1[c(j-1, j+1),]$mx
      data[j,]$mx <- mean(lh)
      data[j,]$mx <- ifelse(is.na(data[j,]$mx), 1e-5, 1e-5)
    }
  }

  ##### Model
  Mx <- data %>% select(age, year, mx) %>% pivot_wider(names_from = year, values_from = mx) %>% select(-age) %>% as.matrix() %>% log()

  if(ax_method == "classic"){
    ax <- rowMeans(Mx)
    Mx <- Mx - ax
    svd_result <- svd(Mx)
    U <- svd_result$u # left singular vectors - age
    D <- svd_result$d # singular values
    V <- svd_result$v # right singular vectors - years

    bx <- U[,1]
    kt <- D[1] * V[,1]

    # Normalization
    bx.sum = sum(bx)
    kt <- kt * bx.sum
    kt <- kt - mean(kt) # kt sum = 0
    bx <- bx / bx.sum # bx sum = 1
  }else if (ax_method == "last"){
    ax = Mx[,ncol(Mx)]
    Mx <- Mx - ax
    svd_result <- svd(Mx)
    U <- svd_result$u # left singular vectors - age
    D <- svd_result$d # singular values
    V <- svd_result$v # right singular vectors - years

    bx <- U[,1]
    kt <- D[1] * V[,1]

    # Normalization
    bx.sum = sum(bx)
    kt <- kt * bx.sum
    kt <- kt - kt[length(kt)] # kt = 0 in the last year
    bx <- bx / bx.sum # bx sum = 1
  }else if (ax_method == "last_smooth"){
    ax = Mx[,ncol(Mx)]
    ax[-1] <- predict(lm(Mx[-1,ncol(Mx)] ~ splines::ns(unique(data$age)[-1], df = round(length(unique(data$age))/5))))
    Mx <- Mx - ax
    svd_result <- svd(Mx)
    U <- svd_result$u # left singular vectors - age
    D <- svd_result$d # singular values
    V <- svd_result$v # right singular vectors - years

    bx <- U[,1]
    kt <- D[1] * V[,1]

    # Normalization
    bx.sum = sum(bx)
    kt <- kt * bx.sum
    kt <- kt - kt[length(kt)] # kt = 0 in the last year
    bx <- bx / bx.sum # bx sum = 1
  } else {
    stop("Choose `ax_method` arguement carefully")
  }

  if(ktadj == "Dmin"){

    if("Dx" %notin% colnames(data) | "N" %notin% colnames(data)){
      stop("for `ktadj = 'Dx'` adjustment in the data should be deaths (`Dx` column) and population (`N` column) by age")
    }

    Dx <- data %>% select(age, year, Dx) %>% pivot_wider(names_from = year, values_from = Dx) %>% select(-age) %>% as.matrix()
    Nx <- data %>% select(age, year, N) %>% pivot_wider(names_from = year, values_from = N) %>% select(-age) %>% as.matrix()

    f <- function(k, ax, bx, Dx, Nx){
      sum(Dx) - sum(exp(ax + bx * k) * Nx)
    }
    kDx <- kt
    for(t in 1:ncol(Mx)){
      kDx[t] <- uniroot(f = f, interval = c(kt[t]-50, kt[t]+50), ax = ax, bx = bx, Dx = Dx[,t], Nx = Nx[,t], tol = 1e-6)$root[1]
    }
    kt <- kDx
  }else if (ktadj == "e0min"){

    e0 <- apply(exp(Mx + ax), 2, FUN = function(x) LT(age = unique(data$age), mx = x, ...)[1,"ex"])

    f <- function(k, ax, bx, e0, ...){
      mx.hat <- exp(ax + bx * k)
      e0 - LT(age = unique(data$age), mx = mx.hat, ...)[1,"ex"]
    }
    kex <- kt
    for(t in 1:ncol(Mx)){
      kex[t] <- uniroot(f = f, interval = c(kt[t]-50, kt[t]+50), ax = ax, bx = bx, e0 = e0[t], tol = 1e-6)$root[1]
    }
    kt <- kex
  } else if (ktadj == "poisson"){

    if("Dx" %notin% colnames(data) | "N" %notin% colnames(data)){
      stop("for `ktadj = 'poisson'` adjustment in the data should be deaths (`Dx` column) and population (`N` column) by age")
    }

    Dx <- data %>% select(age, year, Dx) %>% pivot_wider(names_from = year, values_from = Dx) %>% select(-age) %>% as.matrix()
    Nx <- data %>% select(age, year, N) %>% pivot_wider(names_from = year, values_from = N) %>% select(-age) %>% as.matrix()
    kpois <- kt
    for(t in 1:ncol(Mx)){
      kpois[t] <- coef(glm(round(Dx[,t]) ~ -1 + offset(log(Nx[,t]) + ax) + bx, family = poisson(link = "log")))[1]
    }
    kt <- kpois
  } else if (ktadj == "edaggermin"){

    edag <- apply(exp(Mx + ax), 2, FUN = function(x) edagger(age = unique(data$age), mx = x)[1])

    f <- function(k, ax, bx, edag, ...){
      mx.hat <- exp(ax + bx * k)
      (edag - edagger(age = unique(data$age), mx = mx.hat, ...)[1])^2
    }
    kedag <- kt
    for(t in 1:ncol(Mx)){
      kedag[t] <- optimise(f = f1, interval = c(kt[t]-50, kt[t]+50), ax = ax, bx = bx, edag = edag[t], tol = 1e-6)$minimum
    }
    kt <- kedag
  } else if (ktadj != "none"){
    stop("Choose `ktadj` arguement carefully")
  }

  ##### Fit
  mx.hat <- exp(ax + bx %o% kt)
  e0.hat <- apply(mx.hat, 2, FUN = function(x) LT(age = unique(data$age), mx = x, ...)[1,"ex"])

  ##### Forecast
  forecaster <- function(y = kt, mod = model){
    if(mod == "RWwD"){
      model <- forecast::Arima(y = y, order = c(0,1,0), include.drift = T)
    }else if(mod == "ARIMA"){
      model <- forecast::auto.arima(y = y, seasonal = F)
    } else{
      stop("Choose `model` arguement carefully")
    }
    model
  }

  formodel <- forecaster()
  kt.for = forecast::forecast(formodel, bootstrap = boot, npaths = bn, h = n, level = 1-alpha) %>%
    as.data.frame()
  mx.for <- lapply(kt.for[,1], FUN = function(x) exp(ax + bx %o% x))
  mx.low <- lapply(kt.for[,3], FUN = function(x) exp(ax + bx %o% x))
  mx.high <- lapply(kt.for[,2], FUN = function(x) exp(ax + bx %o% x))

  e0.for <- sapply(mx.for, FUN = function(x) LT(age = unique(data$age), mx = x, ...)[1,"ex"])
  e0.low <- sapply(mx.low, FUN = function(x) LT(age = unique(data$age), mx = x, ...)[1,"ex"])
  e0.high <- sapply(mx.high, FUN = function(x) LT(age = unique(data$age), mx = x, ...)[1,"ex"])


  if(bx_method == "rotate"){
    bx2 = bx
    bx.m = mean(bx2[unique(data$age) %in% 15:64])
    bxu = ifelse(unique(data$age) <=64, bx.m, NA)
    bxu[is.na(bxu)] <- bx2[which(is.na(bxu))] * bx.m /  bx2[which(is.na(bxu))][1]
    bxu = bxu / sum(bxu)

    wt0.hat = (e0.hat - 80)/(102 - e0.hat)
    wt.hat = sqrt( 0.5 * ( 1 + sin( 0.5*pi*(2*wt0.hat - 1) ) ) )

    wt0.for = (e0.for - 80)/(102 - e0.for)
    wt.for = sqrt( 0.5 * ( 1 + sin( 0.5*pi*(2*wt0.for - 1) ) ) )

    wt0.low = (e0.low - 80)/(102 - e0.low)
    wt.low = sqrt( 0.5 * ( 1 + sin( 0.5*pi*(2*wt0.low - 1) ) ) )

    wt0.high = (e0.high - 80)/(102 - e0.high)
    wt.high = sqrt( 0.5 * ( 1 + sin( 0.5*pi*(2*wt0.high - 1) ) ) )

    for(t in 1:length(kt)){
      if(e0.hat[t] < 80){
        bxi = bx
      }else if(e0.hat[t] >= 80 & e0.hat[t] < 102){
        bxi = (1-wt.hat[t]) * bx + wt.hat[t] * bxu
      }else if(e0.hat[t] >= 102){
        bxi = bxu
      }
      mx.hat[,t] <- exp(ax + bxi %o% kt[t])
    }

    for(t in 1:n){
      if(e0.for[t] < 80){
        bxi = bx
      }else if(e0.for[t] >= 80 & e0.for[t] < 102){
        bxi = (1-wt.for[t]) * bx + wt.for[t] * bxu
      }else if(e0.for[t] >= 102){
        bxi = bxu
      }
      mx.for[[t]] <- exp(ax + bxi %o% kt.for[t,1])

      if(e0.low[t] < 80){
        bxi = bx
      }else if(e0.low[t] >= 80 & e0.low[t] < 102){
        bxi = (1-wt.low[t]) * bx + wt.low[t] * bxu
      }else if(e0.low[t] >= 102){
        bxi = bxu
      }
      mx.low[[t]] <- exp(ax + bxi %o% kt.for[t,3])

      if(e0.high[t] < 80){
        bxi = bx
      }else if(e0.high[t] >= 80 & e0.high[t] < 102){
        bxi = (1-wt.high[t]) * bx + wt.high[t] * bxu
      }else if(e0.high[t] >= 102){
        bxi = bxu
      }
      mx.high[[t]] <- exp(ax + bxi %o% kt.for[t,2])
    }

    e0.hat <- apply(mx.hat, 2, FUN = function(x) LT(age = unique(data$age), mx = x, ...)[1,"ex"])
    e0.for <- sapply(mx.for, FUN = function(x) LT(age = unique(data$age), mx = x, ...)[1,"ex"])
    e0.low <- sapply(mx.low, FUN = function(x) LT(age = unique(data$age), mx = x, ...)[1,"ex"])
    e0.high <- sapply(mx.high, FUN = function(x) LT(age = unique(data$age), mx = x, ...)[1,"ex"])
  }

  ##### Merging
  kt.findat <- data.frame(year = unique(data$year), kt = kt, conf.low = NA, conf.high = NA) %>%
    rbind(x = ., y = data.frame(year = (max(data$year) + 1):(max(data$year) + n),
                                kt = kt.for[,1],
                                conf.low = kt.for[,2],
                                conf.high = kt.for[,3])
    )

  e0.obs <- apply(exp(Mx + ax), 2, FUN = function(x) LT(age = unique(data$age), mx = x, ...)[1,"ex"])
  e0.findat <- data.frame(year = unique(data$year), e0.obs = e0.obs, e0.hat = e0.hat, conf.low = NA, conf.high = NA) %>%
    rbind(x = ., y = data.frame(year = (max(data$year) + 1):(max(data$year) + n),
                                e0.obs = NA,
                                e0.hat = e0.for,
                                conf.low = e0.low,
                                conf.high = e0.high)
    )


  mx.findat <- data.frame(year = lapply((max(data$year) + 1):(max(data$year) + n), FUN = function(x) rep(x, length(unique(data$age)))) %>% unlist(),
                          age = rep(unique(data$age), n),
                          mx.obs = NA,
                          mx.hat = unlist(mx.for),
                          conf.low = unlist(mx.high),
                          conf.high = unlist(mx.low)
  )

  mx.hat <- mx.hat %>% as.data.frame()
  colnames(mx.hat) = unique(data$year)
  mx.hat <- mx.hat %>%
    mutate(age = unique(data$age)) %>%
    pivot_longer(cols = !age,
                 names_to = "year",
                 values_to = "mx.hat"
    ) %>%
    arrange(year, age)
  mx.findat <- left_join(mx.hat %>% mutate(year = as.numeric(year)),
                         data %>% select(year, age, mx.obs = mx), by = c("year", "age")
  ) %>%
    mutate(conf.low = NA,
           conf.high = NA) %>%
    select(everything(colnames(mx.findat))) %>%
    rbind(x = ., y = mx.findat) %>%
    as.data.frame()


  return(
    list(
      model = formodel,
      kt = kt.findat %>% mutate(conf.low = ifelse(year == max(data$year), kt, conf.low), conf.high = ifelse(year == max(data$year), kt, conf.high)),
      ex0 = e0.findat %>% mutate(conf.low = ifelse(year == max(data$year), e0.hat, conf.low), conf.high = ifelse(year == max(data$year), e0.hat, conf.high)),
      mx = mx.findat %>% mutate(conf.low = ifelse(year == max(data$year), mx.hat, conf.low), conf.high = ifelse(year == max(data$year), mx.hat, conf.high))
    )
  )
}
