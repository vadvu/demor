#' Plot population pyramid
#'
#' @param popm Numeric vector of male population
#' @param popf Numeric vector of female population
#' @param age Numeric vector of ages
#' @param sexn Character array of labels for the sexes. By default = `c("Males", "Females")`
#' @param sexc Character array of colors for the sexes. By default = `c("#ED0000B2","#00468BB2")`, which are red and blue
#' @param age.cont Logical. Should the age axis be considered as continious scale? Recommend to switch to `TRUE` if the age intervals are small (1 year) and `FALSE` otherwise (for ex., when age interval is 5 years). By default = `FALSE`
#'
#' @return `ggplot2` object
#' @import ggplot2 scales
#' @export
plot_pyr <- function(popm,
                      popf,
                      age,
                      sexn = c("Males", "Females"),
                      sexc = c("#ED0000B2","#00468BB2"),
                      age.cont = FALSE
){
  if (length(popm) != length(popf) || length(popm) != length(age)) {
    stop("popm, popf, and age must have the same length.")
  }
  pyr = data.frame(age = rep(age,2),
                   pop = c(popm,popf),
                   sex = c(rep(sexn[1], length(age)), rep(sexn[2], length(age))),
                   ex = NA
  )
  pyr[pyr$sex == sexn[1],]$ex <- ifelse(popm - popf > 0, popf - popm, 0)
  pyr[pyr$sex == sexn[2],]$ex <- ifelse(popm - popf < 0, abs(popm - popf), 0)

  pyr$pop = ifelse(pyr$sex==sexn[1], -1*pyr$pop, pyr$pop)
  if(age.cont){
    pl <- ggplot(pyr %>% arrange(sex, age), aes(age, pop, fill = sex))+
      geom_bar(stat = "identity", alpha = 0.5, color = "black")+
      geom_bar(aes(y = pop - ex, fill = sex), stat = "identity", color = "black", alpha = 1)+
      coord_flip()+
      scale_x_continuous(breaks = seq(min(age), max(age), 5))+
      scale_y_continuous(sec.axis = dup_axis(),
                         breaks = scales::breaks_extended(n = 10),
                         labels = scales::label_number(scale_cut = scales::cut_short_scale(), style_negative = "parens")
      )+
      labs(x = "Age", y = "")+
      theme_linedraw()+
      scale_fill_manual(name = "", values = sexc)
  }else{
    pl <- ggplot(pyr %>% arrange(sex, age), aes(as.factor(age), pop, fill = sex))+
      geom_bar(stat = "identity", alpha = 0.5, color = "black")+
      geom_bar(aes(y = pop - ex, fill = sex), stat = "identity", color = "black", alpha = 1)+
      coord_flip()+
      scale_y_continuous(sec.axis = dup_axis(),
                         breaks = scales::breaks_extended(n = 10),
                         labels = scales::label_number(scale_cut = scales::cut_short_scale(), style_negative = "parens")
      )+
      labs(x = "Age", y = "")+
      theme_linedraw()+
      scale_fill_manual(name = "", values = sexc)
  }

  return(pl)
}
