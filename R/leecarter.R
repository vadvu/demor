#' Basic Lee-Carter model
#'
#' @param data Dataframe with the following columns: age, year, mx
#' @param n Numeric. Forecasted horizon
#' @param sex Character. Sex. "m" for males or "f" for females.
#' @param concise Logical. Should results be restricted? `TRUE` for just forecast, `FALSE` for full data
#' @references Lee, R. D., & Carter, L. R. (1992). Modeling and forecasting US mortality. *Journal of the American statistical association*, *87*(*419*), 659-671.
#' @return Dataframe with the projected mx and ex for t+n periods with mean, low95 and high 95 values
#' @import forecast dplyr tidyr
#' @export
leecart <- function(data, n=10, sex = "m", concise = TRUE){
  #data: year, age, mx
  `%notin%` <- Negate(`%in%`)
  if("year" %notin% colnames(data) & "age" %notin% colnames(data) & "mx"  %notin% colnames(data)){
    stop("Please, prepare your data carefully")
  }else{
    data <- data %>% arrange(year, age)
  }
  if(sum(data$mx == 0)>0){
    data$mx <- ifelse(data$mx == 0, 1e-5, data$mx)
    warning("0 are in mx vector, transform them to 1e-5")
  }
  #1 step
  ax <- rep(0, length(unique(data$age)))
  names(ax)<-unique(data$age)
  for(i in 1:length(ax)){
    ax[i]<- mean(log(data[data$age == as.numeric(names(ax)[i]), ]$mx))
  }
  #2 step
  data$Ax <- NA
  for(i in unique(data$age)){
    data[data$age == i,]$Ax <- log(data[data$age == i,]$mx) - ax[paste0(i)]
  }
  ma <- data.frame(k = 1:length(unique(data$age)))
  for(i in unique(data$year)){
    ma[paste0(i)]<-data[data$year==i,]$Ax
  }
  ma <- as.matrix(ma[,-1])
  #3 step
  svd <- svd(t(ma),1,1)
  k <- svd$u * sum(svd$v) * svd$d[1]
  b <- svd$v/sum(svd$v)
  svd <- data.frame(age = unique(data$age), ax = ax, b = b)
  #4 step
  #k forecast
  ar <- forecast::auto.arima(k, seasonal = F, stationary = F, allowdrift = T)
  fk <- as.data.frame(forecast::forecast(ar, h = n))
  fk$year <- (max(unique(data$year))+1):(max(unique(data$year))+n)
  kh <- fk[1:length(unique(data$year)),]
  kh$year <- unique(data$year)
  kh[,-6]<-NA
  kh[,1]<-k
  allk <- rbind(kh,fk)[,c(6,1:5)]
  colnames(fk)[2]<-"k"
  colnames(allk)[2]<-"k"
  #m forecast
  for (i in c(1,4,5)){
    if(i==1){
      mf <- matrix(NA, n, length(unique(data$age)), byrow = TRUE)
      for(z in 1:n){
        mf[z,]<-exp(ax+b*fk[z,i])
      }
      mf <- as.data.frame(mf)
      colnames(mf)<-unique(data$age)
      mf$year <- (max(unique(data$year))+1):(max(unique(data$year))+n)
      mf1 <- as.data.frame(tidyr::pivot_longer(mf, cols = 1:length(unique(data$age)), names_to = "age",
                                               values_to = "mx"))
    }else{
      mf <- matrix(NA, n, length(unique(data$age)), byrow = TRUE)
      for(z in 1:n){
        mf[z,]<-exp(ax+b*fk[z,i])
      }
      mf <- as.data.frame(mf)
      colnames(mf)<-unique(data$age)
      mf$year <- (max(unique(data$year))+1):(max(unique(data$year))+n)
      mf <- as.data.frame(tidyr::pivot_longer(mf, cols = 1:length(unique(data$age)), names_to = "age",
                                              values_to = paste0("mx", ifelse(i==4,"_low95","_high95"))))
      mf1 <- merge(mf1,mf,by = c('year', 'age'))
    }
  }
  mf1 <- mf1 %>% mutate(age = as.numeric(age), year = as.numeric(year)) %>% arrange(year, age)
  ledata <- data.frame(year = rep(unique(mf1$year), length(unique(data$age))),
                       age = rep(unique(data$age), length(unique(mf1$year))))
  ledata <- ledata %>% dplyr::mutate(age = as.numeric(age)) %>%  dplyr::arrange(year, age)
  ledata$ex = NA
  ledata$ex_low95 = NA
  ledata$ex_high95 = NA
  for (i in unique(ledata$year)){
    ledata[ledata$year==i,"ex"] <- LT(unique(ledata$age), sex = sex, mx = mf1[mf1$year==i,3])[,"ex"]
    ledata[ledata$year==i,"ex_high95"] <- LT(unique(ledata$age), sex = sex, mx = mf1[mf1$year==i,4])[,"ex"]
    ledata[ledata$year==i,"ex_low95"] <- LT(unique(ledata$age), sex = sex, mx = mf1[mf1$year==i,5])[,"ex"]
  }
  allf <- cbind(mf1, ledata[,-c(1:2)])
  if(concise){
    return(allf)
  }else{
    return(
      list(param = list("a, b" = as.matrix(svd), "k" = k),
           model = ar,
           kdata = allk,
           mxdata = mf1,
           ledata = ledata,
           all_forecast = allf)
    )
  }
}
