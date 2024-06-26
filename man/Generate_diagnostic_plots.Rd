% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{Generate_diagnostic_plots}
\alias{Generate_diagnostic_plots}
\title{Generate diagnostic plots of vaccine coverage and burden estimates}
\usage{
Generate_diagnostic_plots(
  vaccine_coverage_folder,
  coverage_prefix,
  touchstone,
  scenarios,
  no_vaccine_scenario,
  plot_no_vaccine_scenario = TRUE,
  burden_estimate_folder,
  plot_folder,
  plot_file,
  plot_type,
  countries,
  start_year = -1,
  end_year = -1,
  compare_plots = FALSE,
  vaccine_prefix,
  no_vaccine_prefix
)
}
\arguments{
\item{vaccine_coverage_folder}{character, folder to read vaccine coverage of
different scenarios}

\item{coverage_prefix}{character, prefix of coverage file}

\item{touchstone}{character, touchstone (VIMC)}

\item{scenarios}{character, names of vaccination scenarios}

\item{no_vaccine_scenario}{character, name of no vaccination scenario}

\item{plot_no_vaccine_scenario, }{logical, if TRUE then also generate plots
for no vaccine scenario}

\item{burden_estimate_folder}{character, folder to read burden estimates of
different scenarios}

\item{plot_folder}{character, diagnostic plot folder}

\item{plot_file}{character, diagnostic plot file}

\item{plot_type}{character, diagnostic plot type (central or stochastic)}

\item{countries, }{character, "all" countries or specific countries (iso3 codes)}

\item{start_year}{numeric, start year of plot}

\item{end_year}{numeric, end year of plot}

\item{compare_plots}{logical, if TRUE then generate comparative plots}

\item{vaccine_prefix}{character, prefix of burden estimates file (vaccination scenarios)}

\item{no_vaccine_prefix}{character, prefix of burden estimates file (no vaccination scenario)}
}
\value{
Null return value; diagnostic plots are saved to file
}
\description{
Generate diagnostic plots of vaccine coverage and
  burden estimates (cases, deaths, dalys)
}
\details{
Diagnostic plots of vaccine coverage and burden estimates (cases, deaths, dalys)
  are generated for each country and scenario. Optionally, comparative plots
  across all scenarios can be generated.
}
\examples{
Generate_diagnostic_plots (
    vaccine_coverage_folder    = "vaccine_coverage",
    coverage_prefix            = "coverage_",
    touchstone                 = "touchstone",
    scenarios                  = c ("hpv-routine-default", "hpv-routine-best"),
    no_vaccine_scenario        = "hpv-no-vaccination",
    plot_no_vaccine_scenario   = TRUE,
    burden_estimate_folder     = "output_all",
    plot_folder                = "plots"
    plot_file                  = "plot_file.pdf",
    plot_type                  = "central"
    countries                  = "all",
    start_year                 = 2000,
    end_year                   = 2100,
    compare_plots              = TRUE,
    vaccine_prefix             = "central-burden-vaccination_all_",
    no_vaccine_prefix          = "central-burden-novaccination_all_" )

}
