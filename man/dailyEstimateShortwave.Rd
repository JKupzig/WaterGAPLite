% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{dailyEstimateShortwave}
\alias{dailyEstimateShortwave}
\title{Calculate shortwave radiation}
\usage{
dailyEstimateShortwave(SimDates, TempC, Sunshine, GR, cor_row)
}
\arguments{
\item{SimDates}{Datevector of Simulation period}

\item{TempC}{Temperatur as NumericMatrix in degree}

\item{Sunshine}{Sunshine duration as NumericMatrix  in hours}

\item{GR}{information of row for cells}

\item{cor_row}{information of correction of rows for continental grid}
}
\value{
ShortwaveDownMatrix Matrix with estimated shortwave radiation in W/m²
}
\description{
rcpp function to estimate Shortwave radiation when not given as measured input
}
