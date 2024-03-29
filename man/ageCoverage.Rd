% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{ageCoverage}
\alias{ageCoverage}
\title{Get age-specific coverage-rates}
\usage{
ageCoverage(
  ages,
  routine_coverage,
  vaccine_efficacy_nosexdebut,
  vaccine_efficacy_sexdebut,
  campaigns,
  lifetab,
  cohort,
  agevac,
  country_iso3 = NULL
)
}
\arguments{
\item{ages}{Numeric vector (required): ages in model}

\item{routine_coverage}{Number (required): proportion of population that
receives routine vaccination}

\item{campaigns}{List or number (required): if a list, applies MAC
vaccination (needs to change)}

\item{lifetab}{Data.table (required): lifetable generated with lifeTable()}

\item{cohort}{Number (required): cohort-size (only used in MAC campaigns)}

\item{agevac}{Number (required): target age for vaccination}

\item{vaccine_efficacy}{Number (required): proportion indicating
vaccine-efficacy}
}
\value{
Data.table with coverage and effective coverage by age. Used in RunCohort()
}
\description{
Get age-specific coverage-rates
}
\examples{
ages <- c(0:100)
routine_coverage <- 0.75
vaccine_efficacy <- 0.8
lifetab <- lifeTable(unlist(data.mortall[iso3=="AFG", as.character(0:100),
  with=FALSE], use.names=FALSE), 9)
agevac <- 9
ageCoverage (ages, routine_coverage, vaccine_efficacy, -1,
  lifetab, cohort, agevac)
}
