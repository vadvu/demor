#' Plot population pyramid
#'
#' @param popm Numeric vector of male population
#' @param popf Numeric vector of female population
#' @param popm2 Optional. Numeric vector of second male population to be plotted as line.
#' @param popf2 Optional. Numeric vector of second female population to be plotted as line.
#' @param age Numeric vector of ages
#' @param prc Should the population be shown as a percentage rather than in absolute terms? By default, it is `FALSE` and the plot shows user-specific values in `popm` and `popf`.
#' @param sexn Character array of labels for the sexes. By default = `c("Males", "Females")`
#' @param sexc Character array of colors for the sexes. By default = `c("#ED0000B2","#00468BB2")`, which are red and blue
#' @param age.cont Optional. Logical. Should the age axis be considered as continuous scale? Recommend to switch to `TRUE` if the age intervals are small (1 year) and `FALSE` otherwise (for ex., when age interval is 5 years). By default function chooses the value by itself.
#' @param un.intervals Logical. Should the age groups be unified? For example, if the first and second groups are 0-1, 1-4, and all other groups are five-year, the function automatically makes the group 1-4 to make all the intervals the same. By default, `TRUE`.
#'
#' @return `ggplot2` object
#' @import ggplot2 scales
#' @export
plot_pyr <- function(popm, popf, popm2 = NULL, popf2 = NULL, age, prc = FALSE, sexn = c("Males", "Females"), sexc = c("#ED0000B2","#00468BB2"), age.cont = NULL, un.intervals = TRUE){

  if (length(popm) != length(popf) || length(popm) != length(age)) {
    stop("popm, popf, and age must have the same length.")
  }
  if(is.null(age.cont)){
    age.cont <- if (length(age) > 50) TRUE else FALSE
  }
  if(is.null(popm2)){
    popm2 <- popf2 <- rep(0, length(age))
  }
  pyr = data.frame(age = rep(age,2),
                   pop = c(popm, popf),
                   sex = c(rep(sexn[1], length(age)), rep(sexn[2], length(age))),
                   pop2 = c(popm2, popf2)
  )

  if(prc){
    pyr <- pyr %>%
      group_by(sex) %>%
      mutate(pop = pop / sum(pop),
             pop2 = pop2 / sum(pop2)
      ) %>%
      as.data.frame()
    pyr[is.na(pyr)] <- 0
  }

  if(un.intervals){
    if(length(unique(diff(age))) > 1){
      ux <- unique(diff(age))
      agegroup <- ux[which.max(tabulate(match(diff(age), ux)))]
      newage <- seq(0, max(age), agegroup)
      newage0 <- newage
      newage0[1] = newage0[2] - 1
      pyr$age <- demor::ages(x = age, groups = newage0, char = FALSE)
      pyr[which(pyr[,"age"] == newage0[1]), "age"] <- newage[1]
      pyr <- stats::aggregate(data = pyr, cbind(pop, pop2) ~ sex + age, FUN = sum)
    }
  }

  pyr$ex <- NA
  pyr[pyr$sex == sexn[1],]$ex <- ifelse(pyr[pyr$sex == sexn[1],]$pop - pyr[pyr$sex == sexn[2],]$pop > 0, pyr[pyr$sex == sexn[2],]$pop - pyr[pyr$sex == sexn[1],]$pop, 0)
  pyr[pyr$sex == sexn[2],]$ex <- ifelse(pyr[pyr$sex == sexn[1],]$pop - pyr[pyr$sex == sexn[2],]$pop < 0, abs(pyr[pyr$sex == sexn[1],]$pop - pyr[pyr$sex == sexn[2],]$pop), 0)

  pyr$pop = ifelse(pyr$sex==sexn[1], -1*pyr$pop, pyr$pop)
  pyr$pop2 = ifelse(pyr$sex==sexn[1], -1*pyr$pop2, pyr$pop2)
  if(age.cont){
    pl <- ggplot(pyr %>% arrange(sex, age), aes(age, pop, fill = sex))+
      geom_bar(stat = "identity", alpha = 0.25, color = "black", linewidth = 0.1)+
      geom_bar(aes(y = pop - ex, fill = sex), stat = "identity", color = "black", alpha = 1, linewidth = 0.1)+
      coord_flip()+
      scale_x_continuous(breaks = seq(min(age), max(age), 5))+
      scale_y_continuous(sec.axis = dup_axis(),
                         breaks = scales::breaks_extended(n = 10),
                         labels = scales::label_number(scale_cut = scales::cut_short_scale(), style_negative = "parens")
      )+
      labs(x = "", y = "")+
      theme_linedraw()+
      theme(legend.position = "bottom")+
      geom_line(aes(x = age, y = pop2), linetype = "dashed")+
      scale_fill_manual(name = "", values = sexc)
  }else{
    pl <- ggplot(pyr %>% arrange(sex, age) %>% mutate(age = as.factor(age)) %>% mutate(age2 = as.numeric(age)), aes(age, pop, fill = sex))+
      geom_bar(stat = "identity", alpha = 0.25, color = "black", linewidth = 0.1, width = 1)+
      geom_bar(aes(y = pop - ex, fill = sex), stat = "identity", color = "black", alpha = 1, linewidth = 0.1, width = 1)+
      coord_flip()+
      scale_y_continuous(sec.axis = dup_axis(),
                         breaks = scales::breaks_extended(n = 10),
                         labels = scales::label_number(scale_cut = scales::cut_short_scale(), style_negative = "parens")
      )+
      labs(x = "", y = "")+
      theme_linedraw()+
      theme(legend.position = "bottom")+
      geom_line(aes(x = age2, y = pop2), linetype = "dashed")+
      scale_fill_manual(name = "", values = sexc)
  }

  return(pl)
}
