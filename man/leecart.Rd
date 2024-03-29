% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/leecarter.R
\name{leecart}
\alias{leecart}
\title{Basic Lee-Carter model}
\usage{
leecart(data, n = 10, sex = "m", concise = TRUE)
}
\arguments{
\item{data}{Dataframe with following columns: age, year, mx}

\item{n}{Numeric. Forecasted horizon}

\item{sex}{Sex. "m" for males or "f" for females.}

\item{concise}{Should results be restricted? \code{TRUE} for just forecast, \code{FALSE} for full data}
}
\value{
Dataframe with projected mx and ex for t+n periods with mean, low95 and high 95 values
}
\description{
Basic Lee-Carter model
}
\references{
Lee, R. D., & Carter, L. R. (1992). Modeling and forecasting US mortality. \emph{Journal of the American statistical association}, \emph{87}(\emph{419}), 659-671.
}
