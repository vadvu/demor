% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mdecomp.R
\name{mdecomp}
\alias{mdecomp}
\title{Age and cause decomposition of differences in life expectancies}
\usage{
mdecomp(mx1, mx2, age, sex, ax1 = NULL, ax2 = NULL)
}
\arguments{
\item{mx1}{List of numeric arrays. 1st array should be all-cause nmx in the 1st population, other arrays are cause-specific nmx in the 1st population}

\item{mx2}{List of numeric arrays. 1st array should be all-cause nmx in the 2nd population, other arrays are cause-specific nmx in the 2nd population}

\item{age}{Numeric array of age intervals; for full life table = \code{0:100}; for concise life table = \code{c(0:1, seq(5,85,5))}}

\item{sex}{Character. Sex. "m" for males and "f" for females}

\item{ax1}{Optional. Numeric array with ax for the 1st population. By default, it is a the middle of the interval, while ax for age 0 is modeled as in Andreev & Kingkade (2015).}

\item{ax2}{Optional. Numeric array with ax for the 2nd population. By default, it is a the middle of the interval, while ax for age 0 is modeled as in Andreev & Kingkade (2015).}
}
\value{
Dataframe with 1st column as overall decomposition (\code{ex12}), and other columns are decompositions by causes (\code{cause(i)})
}
\description{
Age and cause decomposition of differences in life expectancies
}
