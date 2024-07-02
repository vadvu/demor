#' Probabilistic Projections of Population
#'
#' @param mort.m Data.frame with columns year, age, mx for males.
#' @param mort.f Data.frame with columns year, age, mx for females
#' @param fert Data.frame with columns year, age, fx.
#' @param time.n Numeric value of for how many years projection should be done.
#' @param sims Numeric values of number of simulations.
#' @param popf Data.frame with columns year, age, pop for females
#' @param popm Data.frame with columns year, age, pop for females
#' @import vars MASS stats
#' @return list with data.frame with projections and VAR model
#' @export
prob.proj <- function(mort.m, mort.f, fert, time.n, sims, popf, popm){
  #mortality
  lee.m <- leecart(data = mort.m, concise = F, n = 1, sex = "m")
  lee.f <- leecart(data = mort.f, concise = F, n = 1, sex = "f")
  km <- lee.m$kdata[lee.m$kdata$year <=max(mort.m$year),]$`Point Forecast`
  kf <- lee.f$kdata[lee.f$kdata$year <=max(mort.m$year),]$`Point Forecast`

  all.data <- data.frame(year = min(mort.m$year):max(mort.m$year),
                         km = km,
                         kf = kf,
                         a = NA,
                         b = NA,
                         c = NA)
  #fertility
  for(i in min(mort.m$year):max(mort.m$year)){
    fxi <- fert[fert$year == i,]$fx
    agei <- fert[fert$year == i,]$age
    pars <- fert.approx(fxi, agei, model = "Hadwiger", boot = F)
    all.data[all.data$year == i,4:6] <- pars$model$params
  }

  #model
  full.model <- vars::VAR(all.data[,2:6], type = "trend", p = 1)
  V_e <- summary(full.model)$covres
  V_x <- vcov(full.model)
  betas <- sapply(1:5, FUN = function(x){full.model$varresult[[x]]$coefficients}, simplify = "array")
  betas <- c(betas[,1], betas[,2], betas[,3], betas[,4], betas[,5])
  mydat.sims <- data.frame()

  #simulations
  for(i in 1:sims){
    pred.i <- matrix(0, nrow = time.n+10, ncol = 6)
    pred.i[1:10,] <- t(sapply(10:1, FUN = function(x){
      c(as.numeric(all.data[nrow(all.data)-x,-1]), nrow(all.data)-x)
    }))
    pred.i[10:nrow(pred.i),6] <- (nrow(all.data)-1):(nrow(all.data)+time.n-1)
    for(t in 1:time.n){
      betas.i <- MASS::mvrnorm(n = 1, mu = betas, Sigma = V_x)
      error.i <- MASS::mvrnorm(n = 1, mu =rep(0, 5), Sigma = V_e)
      betas.mat <- matrix(betas.i, ncol = 5)
      pred.i[t+10,1:5] <- t(betas.mat) %*% pred.i[t+9,] + error.i
    }
    if(i == 1){
      all.sims = data.frame(pred.i, i = i)
    } else {
      all.sims = rbind(all.sims, data.frame(pred.i, i = i))
    }
  }

  #aggregation
  new.names = c("km", "kf", "a", "b", "c")
  for(variable in 1:5){
    for(i in c(seq(0.025, 0.975, 0.1), 0.5)){
      colonka <- colnames(all.sims)[variable]
      agri <- aggregate(data = all.sims, get(colonka) ~ X6, FUN = quantile, probs = i)
      colnames(agri)[2] <- paste0(i,new.names[variable])

      if(variable == 1 & i == 0.025){
        all.aggr <- agri
      }else{
        all.aggr <- merge(all.aggr, agri, by = "X6")
      }
    }
  }

  hadw.tfr <- function(X6, prc, what){
    a = all.aggr[all.aggr$X6 == X6,paste0(prc, "a")]
    b = all.aggr[all.aggr$X6 == X6,paste0(prc, "b")]
    c = all.aggr[all.aggr$X6 == X6,paste0(prc, "c")]
    age = unique(fert$age)
    pred.fx = (a*b/c)*(c/age)^(1.5) * exp(-b^2*(c/age + age/c - 2))
    if(what == "tfr"){
      return(tfr(pred.fx, unique(diff(unique(fert$age)))))
    }else{
      return(pred.fx)
    }
  }

  for(i in c(seq(0.025, 0.975, 0.1), 0.5)){
    all.aggr[paste0(i, "tfr")] <- NA
    all.aggr[paste0(i, "tfr")] <- sapply(all.aggr$X6, FUN = function(x){hadw.tfr(x,i,"tfr")})
  }

  le.m <- function(X6, sex, a, b, prc, what){
    if(what == "e0"){
      mx <- exp(a+b*all.aggr[all.aggr$X6 == X6,paste0(prc, "k", sex)])
      LT(age, sex = sex, mx = mx)[1,"ex"]
    }else{
      exp(a+b*all.aggr[all.aggr$X6 == X6,paste0(prc, "k", sex)])
    }
  }

  for(i in c("m","f")){
    age <- lee.m$param$`a, b`[,1]
    if(i == "m"){
      ai <- lee.m$param$`a, b`[,2]
      bi <- lee.m$param$`a, b`[,3]
    }else{
      ai <- lee.f$param$`a, b`[,2]
      bi <- lee.f$param$`a, b`[,3]
    }
    for(j in c(seq(0.025, 0.975, 0.1), 0.5)){
      all.aggr[paste0(j, "e0", i)] <- NA
      all.aggr[paste0(j, "e0", i)] <-  sapply(all.aggr$X6,
                                              FUN = function(x){le.m(X6 = x,
                                                                     sex = i,
                                                                     prc = j,
                                                                     a = ai,
                                                                     b = bi,
                                                                     what = "e0")})
    }
  }

  #projections
  each.proj <- function(sex, prc){
    population <- matrix(data = 0,
                         nrow = length(lee.m$param$`a, b`[,1]),
                         ncol = length(unique(all.aggr$X6)))
    newcols <- c()
    for(i in unique(all.aggr$X6)){
      newcols[i - min(unique(all.aggr$X6))+1] <- ifelse(!is.na(all.data[i,]$year),
                                                        all.data[i,]$year,
                                                        newcols[i - min(unique(all.aggr$X6))]+1
      )
    }
    colnames(population) <- newcols
    rownames(population) <- lee.m$param$`a, b`[,1]
    if(sex == "m"){
      pop = popm
      ai <- lee.m$param$`a, b`[,2]
      bi <- lee.m$param$`a, b`[,3]
    } else {
      pop = popf
      ai <- lee.f$param$`a, b`[,2]
      bi <- lee.f$param$`a, b`[,3]
    }
    for(i in colnames(population)){
      if(as.numeric(i) %in% all.data$year){
        population[,i] <- pop[pop$year == as.numeric(i),3]
      } else {
        next
      }
    }
    trends <- unique(all.aggr$X6)
    names(trends) <- newcols

    for(i in colnames(population)){
      if(as.numeric(i) %in% all.data$year){
        next
      } else{
        les.i <- leslie(
          mx = le.m(X6 = trends[i], sex = sex, a = ai, b = bi, prc = prc, what = "mx"),
          fx = hadw.tfr(X6 = trends[i], prc = prc, what = "fx"),
          age1 = unique(mort.m$age),
          age2 = unique(fert$age),
          sex = sex
        )
        population[,paste0(i)] <- les.i %*% population[,paste0(as.numeric(i)-1)]
      }
    }
    return(population)
  }

  for(sex in c("m","f")){
    for(i in c(seq(0.025, 0.975, 0.1), 0.5)){
      pred.i <- each.proj(sex,i)

      all.aggr[paste0(i, "pop", sex)] <- NA
      all.aggr[paste0(i, "pop", sex)] <- colSums(pred.i)

      if(sex == "f"){
        mka <- each.proj("m",i)
        all.aggr[paste0(i, "med.age")] <- sapply(1:ncol(pred.i),
                                                 FUN = function(x){
                                                   med.age(N = pred.i[,x] + mka[,x],
                                                           age = as.numeric(rownames(pred.i)),
                                                           int = median(diff(as.numeric(rownames(pred.i)))))
                                                 })
        all.aggr[paste0(i, "net")] <- NA
        all.aggr[-1, paste0(i, "net")] <- diff(colSums(pred.i + mka))

        all.aggr[paste0(i, "dependency")] <- NA
        all.aggr[paste0(i, "dependency")] <- sapply(1:ncol(pred.i),
                                                    FUN = function(x){
                                                      whc.dep = which(
                                                        as.numeric(rownames(pred.i)) %in% c(0:20, 65:150)
                                                      )
                                                      N = pred.i[,x] + mka[,x]
                                                      100*(sum(N[whc.dep]))/(sum(N[-whc.dep]))
                                                    })
        all.aggr[paste0(i, "old.dependency")] <- NA
        all.aggr[paste0(i, "old.dependency")] <- sapply(1:ncol(pred.i),
                                                        FUN = function(x){
                                                          whc.dep = which(
                                                            as.numeric(rownames(pred.i)) %in% c(65:150)
                                                          )
                                                          N = pred.i[,x] + mka[,x]
                                                          100*(sum(N[whc.dep]))/(sum(N[-whc.dep]))
                                                        })
      }

      if(sex == "m" & i == 0.025){
        proj.age <- data.frame(age = as.numeric(rownames(pred.i)))
      }else{
        proj.age.i <- as.data.frame(cbind(age = as.numeric(rownames(pred.i)), pred.i))
        colnames(proj.age.i)[-1] <- paste0(colnames(proj.age.i)[-1], sex, i)
        proj.age <- cbind(proj.age, proj.age.i[,-1])
      }
    }
  }

  all.aggr$year <- (max(mort.m$year)-10):(max(mort.m$year)+time.n-1)
  return(list(data = all.aggr,
              age.structure = proj.age,
              model = full.model,
              tests = list("H0: NOT serially correlated errors" = vars::serial.test(full.model))
  )
  )
}
