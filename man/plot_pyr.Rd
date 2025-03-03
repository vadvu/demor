% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_pyr.R
\name{plot_pyr}
\alias{plot_pyr}
\title{Plot population pyramid}
\usage{
plot_pyr(
  popm,
  popf,
  age,
  sexn = c("Males", "Females"),
  sexc = c("#ED0000B2", "#00468BB2"),
  age.cont = FALSE
)
}
\arguments{
\item{popm}{Numeric vector of male population}

\item{popf}{Numeric vector of female population}

\item{age}{Numeric vector of ages}

\item{sexn}{Character array of labels for the sexes. By default = \code{c("Males", "Females")}}

\item{sexc}{Character array of colors for the sexes. By default = \code{c("#ED0000B2","#00468BB2")}, which are red and blue}

\item{age.cont}{Logical. Should the age axis be considered as continious scale? Recommend to switch to \code{TRUE} if the age intervals are small (1 year) and \code{FALSE} otherwise (for ex., when age interval is 5 years). By default = \code{FALSE}}
}
\value{
\code{ggplot2} object
}
\description{
Plot population pyramid
}
