#' Plot population pyramid
#'
#' @param popm Numeric vector of male population
#' @param popf Numeric vector of female population
#' @param ages Numeric vector of unique ages
#'
#' @return plot as `ggplot2` object
#' @import ggplot2
#' @export
plot_pyr <- function(popm, popf, ages){
  pyr = data.frame(age = rep(ages,2),
                   pop = c(popm,popf),
                   sex = c(rep("Males", length(ages)), rep("Females", length(ages)))
                   )
  pyr$pop = ifelse(pyr$sex=="Males", -1*pyr$pop, pyr$pop)
  pl <- ggplot(pyr, aes(as.factor(age), pop, fill = sex))+
    geom_bar(stat = "identity", alpha = 0.8)+
    coord_flip()+
    scale_y_continuous(labels = abs)+
    xlab("Age")+ylab("")+theme_classic()+
    scale_fill_manual(name = "Sex:", values=c("#ED0000B2","#00468BB2"))
  return(pl)
}
