% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{data.incidence_ui}
\alias{data.incidence_ui}
\title{Incidence of cervical cancer with uncertainty intervals, all ages and by country}
\format{
A data table with 185 observations of 5 variables.
\describe{
  \item{Country}{Country name}
  \item{iso3}{ISO3 country code}
  \item{Mid}{Estimated number of new cases in 2018, cervix uteri, females, all ages (mean)}
  \item{Low}{Estimated number of new cases in 2018, cervix uteri, females, all ages (lower bound of 95\% uncertainty interval)}
  \item{High}{Estimated number of new cases in 2018, cervix uteri, females, all ages (upper bound of 95\% uncertainty interval)}
  \item{Source}{Data source}
  \item{Year}{Data source (year)}
}
}
\source{
\url{https://gco.iarc.fr/today/online-analysis-table}
}
\usage{
data.incidence_ui
}
\description{
A dataset containing the incidence of cervical cancer with uncertainty
    intervals in 185 countries, as reported by IARC's Globocan 2018 database.
    Estimated number of new cases in 2018, cervix uteri, females, all ages.
}
\keyword{datasets}
