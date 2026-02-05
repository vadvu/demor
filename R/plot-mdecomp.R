#' Plot for mdecomp function
#'
#' @param x A result of age and cause decomposition from [mdecomp()].
#' @param return.data Should the data be returned (`TRUE`) or the ggplot2 plot (default, `FALSE`).
#' @param ... Ignored.
#'
#' @returns ggplot2 plot
#' @seealso [mdecomp()]
#' @method plot mdecomp
#' @export
plot.mdecomp <- function(x, return.data = FALSE, ...){
  decm <- x
  decm_plot <- decm[,c(1,3)]
  decm_plot$group = colnames(decm)[3]
  colnames(decm_plot)[2]<-"ex12"
  for(i in 4:ncol(decm)){
    decm_plot_i <- decm[,c(1,i)]
    decm_plot_i$group = colnames(decm)[i]
    colnames(decm_plot_i)[2]<-"ex12"
    decm_plot <- rbind(decm_plot,decm_plot_i)
    rm(decm_plot_i)
  }
  for (i in unique(decm_plot$group)){
    decm_plot[decm_plot$group==i,]$group <- paste0(i, " (", round(sum(decm_plot[decm_plot$group==i,]$ex12),2), ")")
  }
  finplot <- decm_plot %>%
    group_by(group) %>%
    mutate(age = ages(x = age, groups =  age, char = T)) %>%
    as.data.frame()
  if(return.data){
    return(finplot)
  }else{
    finplot <- finplot %>%
      ggplot(aes(x = age, y = ex12, fill = group))+
      geom_bar(stat="identity", colour = "black")
    return(finplot)
  }
  return(finplot)
}
