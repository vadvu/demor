#' Probabilistic Projections of Population
#'
#' @param mort.m Data.frame with columns year, age, mx for males.
#' @param mort.f Data.frame with columns year, age, mx for females
#' @param fert Data.frame with columns year, age, fx.
#' @param popf Data.frame with columns year, age, pop for females
#' @param popm Data.frame with columns year, age, pop for females
#' @param time.n Numeric value of for how many years projection should be done.
#' @param sims Numeric values of number of simulations.
#' @param tfr.boot Logical. Should bootstrap be used to estimate TFR? Default is `FALSE` The number of replications is `sims`
#' @param clusters Numeric. Number of engines for parallel computations
#' @param pp lag size
#' @param migr migration data or `NULL` for closed population
#' @import vars MASS stats dplyr parallel foreach doParallel
#' @return list with data.frame with projections, VAR model, projected age structure and some tests.
#' @export
prob.proj <- function(mort.m, mort.f, fert, popf, popm,
                      migr = NULL,
                      time.n = 25,
                      sims = 500,
                      tfr.boot = F,
                      clusters = 6,
                      pp = 1){
  #mortality
  lee.m <- leecart(data = mort.m, concise = F, n = 1, sex = "m")
  lee.f <- leecart(data = mort.f, concise = F, n = 1, sex = "f")
  km <- lee.m$kdata[lee.m$kdata$year <=max(mort.m$year),]$k
  kf <- lee.f$kdata[lee.f$kdata$year <=max(mort.m$year),]$k

  all.data <- data.frame(year = min(mort.m$year):max(mort.m$year),
                         km = km,
                         kf = kf,
                         a = NA,
                         b = NA,
                         c = NA)
  #fertility
  if(tfr.boot == T){
    sims.tfr <- ifelse(sims>1000, 1000, sims)
    cl <- parallel::makeCluster(clusters)
    doParallel::registerDoParallel(cl)
    for(i in min(mort.m$year):max(mort.m$year)){
      fxi <- fert[fert$year == i,]$fx
      agei <- fert[fert$year == i,]$age
      start.point = c(tfr(fxi, unique(diff(agei))),
                      (max(fxi)*mac(fxi, agei))/tfr(fxi, unique(diff(agei))),
                      mac(fxi, agei))
      est.out <- foreach::foreach(i = 1:sims.tfr, .packages = "demor", .combine = rbind)%dopar%{
        pars.b <- tryCatch({
          b.samp = sample(1:length(fxi), replace = T)
          demor::fert.approx(fxi[b.samp], agei[b.samp], model = "Hadwiger", boot = F,
                             start = start.point)$model$params
        },
        error = function(cond){
          c(NA, NA, NA)
        })
        pars.b
      }
      all.data[all.data$year == i,4:6] <- c(median(est.out[,1], na.rm = T),
                                            median(est.out[,2], na.rm = T),
                                            median(est.out[,3], na.rm = T)
      )

    }
    parallel::stopCluster(cl)
  }else{
    for(i in min(mort.m$year):max(mort.m$year)){
      fxi <- fert[fert$year == i,]$fx
      agei <- fert[fert$year == i,]$age
      pars <- demor::fert.approx(fxi, agei, model = "Hadwiger", boot = F, start = NULL)
      all.data[all.data$year == i,4:6] <- pars$model$params
    }
  }

  all.data[,4:6] <- log(all.data[,4:6])

  #model
  if(is.null(pp)){
    pp <- vars::VARselect(all.data[,2:6], type = "trend", lag.max = 3)$selection
    pp <- median(pp)
  }
  full.model <- vars::VAR(all.data[,2:6], type = "trend", p = pp)
  V_e <- summary(full.model)$covres
  V_x <- vcov(full.model)
  betas <- sapply(1:5, FUN = function(x){full.model$varresult[[x]]$coefficients}, simplify = "array")
  betas <- c(betas[,1], betas[,2], betas[,3], betas[,4], betas[,5])
  mydat.sims <- data.frame()

  #simulations
  cl <- parallel::makeCluster(clusters)
  doParallel::registerDoParallel(cl)
  all.sims <- foreach(i = 1:sims,.packages = c("MASS", "demor"), .combine = rbind) %dopar% {
    pred.i <- matrix(0, nrow = time.n+nrow(all.data), ncol = 6)
    pred.i[1:nrow(all.data),1:5] <- as.matrix(full.model$y)
    pred.i[1:nrow(pred.i),6] <- 1:nrow(pred.i)
    betas.mat <- matrix(MASS::mvrnorm(n = 1, mu = betas, Sigma = V_x), ncol = 5)

    for(t in 1:time.n){
      error.i <- MASS::mvrnorm(n = 1, mu = rep(0, 5), Sigma = V_e)
      mat.c <- sapply(1:pp, FUN = function(x){
        if(x != pp){
          pred.i[t+nrow(full.model$y)-x,-ncol(pred.i)]
        }else{
          pred.i[t+nrow(full.model$y)-x,]
        }
      })
      mat.c <- unlist(mat.c)
      pred.i[t+nrow(full.model$y),1:5] <- t(betas.mat) %*% mat.c + error.i
    }
    data.frame(pred.i, i = i)
  }
  parallel::stopCluster(cl)
  all.sims[,3:5] <- exp(all.sims[,3:5])

  #aggregation
  new.names = c("km", "kf", "a", "b", "c")
  un.prc <- unlist(lapply(c(seq(0.5, 0.9, 0.1), 0.95, 0.99), FUN = function(x){c(1-(1-x)/2, (1-x)/2)}))
  un.prc <- sort(c(un.prc, 0.5))


  for(variable in 1:5){
    for(i in un.prc){
      colonka <- colnames(all.sims)[variable]
      agri <- aggregate(data = all.sims, get(colonka) ~ X6, FUN = quantile, probs = i)
      colnames(agri)[2] <- paste0(i,new.names[variable])

      if(variable == 1 & i == un.prc[1]){
        all.aggr <- agri
      }else{
        all.aggr <- merge(all.aggr, agri, by = "X6")
      }
    }
  }
  colnames(all.sims)[1:5] <- new.names

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

  for(i in un.prc){
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
    for(j in un.prc){
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

  migration.prob <- function(migr, year, sex){
    num <- migr[migr$year == year,2]
    ver <- dbinom(x = 1:length(16:40), size = length(16:40), prob = 0.5)
    fin = c()
    for(j in ver){
      fin <- c(fin, rbinom(1, size = abs(num), prob = j))
    }
    names(fin) <- 16:40
    new.mig <- lee.m$param$`a, b`[,1]
    new.mig[17:41] <- fin*(num/abs(num))
    new.mig[c(0:16, 42:101)] <- 0
    sex.factor <- ifelse(sex == "m",
                         runif(1, min = 0.5, max = 0.7),
                         1-runif(1, min = 0.5, max = 0.7))
    round(new.mig*sex.factor)
  }




  #projections
  each.proj <- function(sex, ii){
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
        mx.i <- exp(ai+bi*all.sims[all.sims$i == ii & all.sims$X6 == trends[i],paste0("k", sex)])
        a.i = all.sims[all.sims$X6 == trends[i] & all.sims$i == ii,]$a
        b.i = all.sims[all.sims$X6 == trends[i] & all.sims$i == ii,]$b
        c.i = all.sims[all.sims$X6 == trends[i] & all.sims$i == ii,]$c
        age.i = unique(fert$age)
        fx.i = (a.i*b.i/c.i)*(c.i/age.i)^(1.5) * exp(-b.i^2*(c.i/age.i + age.i/c.i - 2))
        les.i <- leslie(
          mx = mx.i,
          fx = fx.i,
          age1 = unique(mort.m$age),
          age2 = unique(fert$age),
          sex = sex
        )
        population[,paste0(i)] <- round(les.i %*% population[,paste0(as.numeric(i)-1)])
        if(!is.null(migr)){
          population[,paste0(i)] <- population[,paste0(i)] + migration.prob(migr, year = as.numeric(i), sex = sex)
        }
      }
    }
    return(population)
  }

  cl <- parallel::makeCluster(clusters)
  doParallel::registerDoParallel(cl)
  out2 <- foreach(i = 1:sims, .packages = c("demor", "dplyr"))%dopar%{
    big.list <- list()
    for(sex in c("m","f")){
      big.list[[sex]] <- each.proj(sex,i)
    }
    big.list[["all"]] <- big.list[["m"]]+big.list[["f"]]
    popm.i <- colSums(big.list[["m"]])
    popf.i <- colSums(big.list[["f"]])
    all.i <- colSums(big.list[["all"]])
    net <- c(NA,diff(all.i))
    dep <- sapply(1:length(all.i),FUN = function(x){
      whc.dep = which(
        as.numeric(rownames(big.list[["all"]])) %in% c(0:20, 65:150)
      )
      N = big.list[["all"]][,x]
      100*(sum(N[whc.dep]))/(sum(N[-whc.dep]))
    })
    old.dep <- sapply(1:length(all.i),FUN = function(x){
      whc.dep = which(
        as.numeric(rownames(big.list[["all"]])) %in% c(65:150)
      )
      N = big.list[["all"]][,x]
      100*(sum(N[whc.dep]))/(sum(N[-whc.dep]))
    })
    # med.age <- sapply(1:length(all.i),
    #                   FUN = function(x){
    #                     med.age(N = big.list[["all"]][,x],
    #                             age = as.numeric(rownames(big.list[["all"]])),
    #                             int = median(diff(as.numeric(rownames(big.list[["all"]])))))
    #                     })
    # med.age <- round(med.age,2)
    dd <- data.frame(popm = popm.i, popf = popf.i, pop = all.i,
                     net = net,
                     dependency = dep,
                     old.dependency = old.dep,
                     # med.age = med.age,
                     i = i
    )
    dd$X6 <- 1:nrow(dd)
    for(j in names(big.list)){
      big.list[[j]] <- as.data.frame(big.list[[j]])
      big.list[[j]]$age <- as.numeric(rownames(big.list[[j]]))
      big.list[[j]]$i <- i
    }
    list(aggr = dd, all = big.list)

  }

  out2.1 <- foreach(i = 1:sims, .combine = rbind)%dopar%{
    out2[[i]]$aggr
  }
  out2.struct <- foreach(i = 1:sims, .combine = rbind)%dopar%{
    out2[[i]]$all$all
  }
  parallel::stopCluster(cl)
  all.sims <- left_join(all.sims, out2.1, by = c("X6", "i"))

  cl <- parallel::makeCluster(clusters)
  doParallel::registerDoParallel(cl)
  out2.2.m <- foreach(i = 1:sims, .combine = rbind, .packages = c("dplyr")) %dopar% {
    out2[[i]]$all$m %>%
      as.data.frame() %>%
      mutate(age = as.numeric(rownames(.))) %>%
      mutate(i = i)
  }
  out2.2.f <- foreach(i = 1:sims, .combine = rbind, .packages = c("dplyr")) %dopar% {
    out2[[i]]$all$f %>%
      as.data.frame() %>%
      mutate(age = as.numeric(rownames(.))) %>%
      mutate(i = i)
  }
  out2.2.all <- foreach(i = 1:sims, .combine = rbind, .packages = c("dplyr")) %dopar% {
    out2[[i]]$all$all %>%
      as.data.frame() %>%
      mutate(age = as.numeric(rownames(.))) %>%
      mutate(i = i)
  }
  parallel::stopCluster(cl)

  out2.2.m <- out2.2.m %>% pivot_longer(cols = 1:(ncol(out2.2.m)-2),
                                        names_to = "year",
                                        values_to = "pop") %>%
    as.data.frame()
  out2.2.f <- out2.2.f %>% pivot_longer(cols = 1:(ncol(out2.2.f)-2),
                                        names_to = "year",
                                        values_to = "pop") %>%
    as.data.frame()
  out2.2.all <- out2.2.all %>% pivot_longer(cols = 1:(ncol(out2.2.all)-2),
                                            names_to = "year",
                                            values_to = "pop") %>%
    as.data.frame()

  cl <- parallel::makeCluster(clusters)
  doParallel::registerDoParallel(cl)
  agri.all <- foreach(i = un.prc, .combine = cbind)%dopar%{
    agri.mi <- aggregate(data = out2.2.m, pop ~ age + year, FUN = quantile, probs = i)
    agri.fi <- aggregate(data = out2.2.f, pop ~ age + year, FUN = quantile, probs = i)
    agri.alli <- aggregate(data = out2.2.all, pop ~ age + year, FUN = quantile, probs = i)
    agri.alli <- cbind(agri.alli, agri.mi[,3], agri.fi[,3])
    colnames(agri.alli) <- c("age", "year", paste0("pop", i), paste0("popm", i), paste0("popf", i))
    if(i == un.prc[1]){
      agri.alli
    }else{
      agri.alli[,-c(1:2)]
    }
  }
  parallel::stopCluster(cl)

  for(i in un.prc){
    agri <- aggregate(data = all.sims, pop ~ X6, FUN = quantile, probs = i)
    colnames(agri)[2] <- paste0(i,"pop")
    all.aggr <- left_join(all.aggr, agri, by = "X6")

    agri <- aggregate(data = all.sims, popm ~ X6, FUN = quantile, probs = i)
    colnames(agri)[2] <- paste0(i,"popm")
    all.aggr <- left_join(all.aggr, agri, by = "X6")

    agri <- aggregate(data = all.sims, popf ~ X6, FUN = quantile, probs = i)
    colnames(agri)[2] <- paste0(i,"popf")
    all.aggr <- left_join(all.aggr, agri, by = "X6")

    # agri <- aggregate(data = all.sims, med.age ~ X6, FUN = quantile, probs = i)
    # colnames(agri)[2] <- paste0(i,"med.age")
    # all.aggr <- left_join(all.aggr, agri, by = "X6")

    agri <- aggregate(data = all.sims, net ~ X6, FUN = quantile, probs = i)
    colnames(agri)[2] <- paste0(i,"net")
    all.aggr <- left_join(all.aggr, agri, by = "X6")

    agri <- aggregate(data = all.sims, dependency ~ X6, FUN = quantile, probs = i)
    colnames(agri)[2] <- paste0(i,"dependency")
    all.aggr <- left_join(all.aggr, agri, by = "X6")

    agri <- aggregate(data = all.sims, old.dependency ~ X6, FUN = quantile, probs = i)
    colnames(agri)[2] <- paste0(i,"old.dependency")
    all.aggr <- left_join(all.aggr, agri, by = "X6")
  }

  all.aggr$year <- NA
  all.aggr[1:nrow(all.data),]$year <- all.data$year
  all.aggr <- all.aggr[,c("year", colnames(all.aggr)[-ncol(all.aggr)])]
  all.aggr[(nrow(all.data)+1):nrow(all.aggr),]$year <- max(all.data$year+1):(max(all.data$year)+time.n)
  return(list(data = all.aggr,
              age = agri.all,
              sims.age = out2.struct,
              all.sims = all.sims,
              model = full.model,
              tests = list("H0: NOT serially correlated errors" = vars::serial.test(full.model))
  )
  )
}
