% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rosbris.R
\name{get_rosbris}
\alias{get_rosbris}
\title{Get data from RosBris (DEPRECATED)}
\usage{
get_rosbris(type, age = 1, initial = FALSE, lastyear = 2022)
}
\arguments{
\item{type}{Character. Type of data for downloading. "m" = mortality, f = "ferility"}

\item{age}{Numeric. Age group. \code{1} for one-year data, \code{5} for five-year data}

\item{initial}{Logical. Do you want initial rosbris data? \code{T} for initial, \code{F} for 'long' format (default).}

\item{lastyear}{Numeric. The last available year in rosbris
(can be seen on the \href{http://demogr.nes.ru/index.php/ru/demogr_indicat/data}{website}). Default is \code{2022} (in 2023).}
}
\value{
List (if \code{initial = TRUE}) of dataframes: population and mortality/fertility data. Dataframe (if \code{initial = FALSE}) in long format.
}
\description{
Get data from RosBris (DEPRECATED)
}
\references{
Center for Demographic Research (2023). \emph{Russian Fertility and Mortality Database (Rosbris)}. \href{http://www.demogr.nes.ru/en/demogr_indicat/data}{URL}
}
