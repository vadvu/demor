#' (IN DEMO!) Basic Lee-Carter model
#'
#' @param data Dataframe with following columns: age, year, mx
#' @param n Numeric. Forecasted horizon
#'
#' @return Dataframe with projected mx for t+n periods with mean, low95 and high 95 values (group column)
#' @import forecast
#' @export
leecart <- function(data, n=10){
  #data = year, age, mx
  `%notin%` <- Negate(`%in%`)
  if("year" %notin% colnames(data) & "age" %notin% colnames(data) & "mx"  %notin% colnames(data)){
    stop("Prepare data carefully")
  }
  #1 step
  ax <- rep(0, length(unique(data$age)))
  names(ax)<-unique(data$age)
  for(i in 1:length(ax)){
    ax[i]<- mean(log(data[data$age== as.numeric(names(ax)[i]), ]$mx))
  }
  #2 step
  data$Ax <- NA
  for(i in unique(data$age)){
    data[data$age == i,]$Ax <- log(data[data$age == i,]$mx) - ax[paste0(i)]
  }

  ma <- data.frame(k = 1:length(unique(data$age)))
  for(i in min(data$year):max(data$year)){
    ma[paste0(i)]<-NA
    ma[,paste0(i)] <- data[data$year==i,]$Ax
  }
  ma <- as.matrix(ma[,-1])
  #3 step
  svd <- svd(ma)
  k <- svd$v[,1]
  s <- svd$d[1]
  b <- svd$u[,1]
  svd <- data.frame(age = unique(data$age), ax = ax, b = b, s = s)
  #4 step
  ar <- forecast::auto.arima(k, seasonal = F, stationary = F, allowdrift = T)
  fk <- as.data.frame(forecast(ar, h = n))
  pred_m <- data.frame(age = unique(data$age), group = "mean")
  for (i in 1:n){
    pred_m[paste0("t+",i)] <- NA
    pred_m[,paste0("t+",i)] <- exp(svd$ax + svd$s*fk[i,1]*svd$b)
  }
  pred_l <- data.frame(age = unique(data$age), group = "low95")
  for (i in 1:n){
    pred_l[paste0("t+",i)] <- NA
    pred_l[,paste0("t+",i)] <- exp(svd$ax + svd$s*fk[i,4]*svd$b)
  }
  pred_h <- data.frame(age = unique(data$age), group = "high95")
  for (i in 1:n){
    pred_h[paste0("t+",i)] <- NA
    pred_h[,paste0("t+",i)] <- exp(svd$ax + svd$s*fk[i,5]*svd$b)
  }
  pred <- rbind(pred_m, pred_h, pred_l)
  return(pred)
}
