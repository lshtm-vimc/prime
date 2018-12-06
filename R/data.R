# Data.R - This file includes documentation of datasets.

#' 1-year prevalence of cervical cancer
#'
#' A dataset containing the 1-year prevalence (proportion) of cervical cancer in 185 countries, as reported by
#'     IARC's Globocan 2018 database.
#'
#' As per IARC definition -- The (1-year) prevalence of a given cancer
#'     is the number of individuals within a defined population who have been diagnosed with that
#'     cancer (within 1 year) and who are still alive at a given point in time (i.e. the survivors).
#'
#' @format A data table containing 185 observations of 103 variables.
#' \describe{
#'   \item{Country}{Country name}
#'   \item{0..100}{Age 0-100}
#'   \item{iso3}{ISO3 country code}
#' }
#' @source \url{https://gco.iarc.fr/today/online-analysis-table}
"data.cecx_1y_prevalence"


#' 3-year prevalence of cervical cancer
#'
#' A dataset containing the 3-year prevalence (proportion) of cervical cancer in 185 countries, as reported by
#'     IARC's Globocan 2018 database.
#'
#' As per IARC definition -- The (3-year) prevalence of a given cancer
#'     is the number of individuals within a defined population who have been diagnosed with that
#'     cancer (within 3 years) and who are still alive at a given point in time (i.e. the survivors).
#'
#' @format A data table containing 185 observations of 103 variables.
#' \describe{
#'   \item{Country}{Country name}
#'   \item{0..100}{Age 0-100}
#'   \item{iso3}{ISO3 country code}
#' }
#' @source \url{https://gco.iarc.fr/today/online-analysis-table}
"data.cecx_3y_prevalence"


#' 5-year prevalence of cervical cancer
#'
#' A dataset containing the 5-year prevalence (proportion) of cervical cancer in 185 countries, as reported by
#'     IARC's Globocan 2018 database.
#'
#' As per IARC definition -- The (5-year) prevalence of a given cancer
#'     is the number of individuals within a defined population who have been diagnosed with that
#'     cancer (within 5 years) and who are still alive at a given point in time (i.e. the survivors).
#'
#' @format A data table with 185 observations of 103 variables.
#' \describe{
#'   \item{Country}{Country name}
#'   \item{0..100}{Age 0-100}
#'   \item{iso3}{ISO3 country code}
#' }
#' @source \url{https://gco.iarc.fr/today/online-analysis-table}
"data.cecx_5y_prevalence"


#' Cost of cervical cancer treatment
#'
#' A dataset containing the cost of cervical cancer treatment.
#'
#' @format A data table with 194 observations of 4 variables.
#' \describe{
#'   \item{country}{Country name}
#'   \item{cancer_cost}{cost per cancer episode, in $US}
#'   \item{cancer_cost_adj}{cost per cancer episode -- adjusted, in international/PPP $}
#'   \item{iso3}{ISO3 country code}
#' }
#' @source \url{}
"data.costcecx"


#' Country names and codes
#'
#' A dataset containing the country names and codes (ISO/WHO/WB/UN).
#'
#' @format A data table with 251 observations of 13 variables.
#' \describe{
#'   \item{name1, name2, name3, name4}{Country names}
#'   \item{iso2, iso3}{ISO2 and ISO3 country codes}
#'   \item{isonum}{ISO number}
#'   \item{WHOcode}{WHO country code}
#'   \item{who_region}{WHO region}
#'   \item{who_mort}{WHO mortality stratum - A/B/C/D/E}
#'   \item{WBincome}{WB income classification of countries}
#'   \item{UNgroup}{UN group classification of countries}
#'   \item{GDPpc2011id}{GDP per capita (2011)}
#' }
#' @source \url{}
"data.countryname"


#' Global data table
#'
#' A dataset containing a global range of variables.
#'
#' @format A data table with 194 observations of 35 variables.
#' \describe{
#'   \item{Country}{Country name}
#'   \item{iso2}{ISO2 country code}
#'   \item{WHO Region}{WHO regions}
#'   \item{WHO Mortality Stratum}{WHO mortality stratum}
#'   \item{World Bank Income Group (2011)}{World Bank income group levels}
#'   \item{GAVI Eligibility}{GAVI eligibility status}
#'   \item{PAHO Revolving Fund}{PAHO revolving fund status}
#'   \item{Cohort size (2010) [1]}{Cohort size}
#'   \item{Coverage (3 doses at year 10) [2]}{Vaccination coverage at age 10}
#'   \item{Vaccine efficacy vs vaccine type infection [2]}{Vaccine efficacy}
#'   \item{Duration of protection [2]}{Duration of vaccine protection}
#'   \item{Age group [3]}{Age group}
#'   \item{Vaccine price [4]}{Vaccine price}
#'   \item{Vaccine delivery/ operational/ admin costs [5]}{Vaccine delivery, operational and administration costs}
#'   \item{Cancer treatment costs - primary level hospital i$ (per episode, over lifetime) [6]}{Cancer treatment costs - primary level hospital i$ (per episode, over lifetime)}
#'   \item{Cancer treatment costs  - primary level hospital US$ (per episode, over lifetime) [6]}{Cancer treatment costs  - primary level hospital US$ (per episode, over lifetime)}
#'   \item{Cancer treatment costs - secondary level hospital i$ (per episode, over lifetime) [6]}{Cancer treatment costs - secondary level hospital i$ (per episode, over lifetime)}
#'   \item{Cancer treatment costs - secondary level hospital US$ (per episode, over lifetime) [6]}{Cancer treatment costs - secondary level hospital US$ (per episode, over lifetime)}
#'   \item{Cancer treatment costs - teaching hospital i$ (per episode, over lifetime) [6]}{Cancer treatment costs - teaching hospital i$ (per episode, over lifetime)}
#'   \item{Cancer treatment costs - teaching hospital US$ (per episode, over lifetime) [6]}{Cancer treatment costs - teaching hospital US$ (per episode, over lifetime)}
#'   \item{Discount rate [2]}{Disount rate}
#'   \item{Perspective [2]}{Perspective}
#'   \item{Costs [2]}{}
#'   \item{Time horizon [2]}{Time horizon}
#'   \item{"Percent" CeCx due to 16/18}{Percentage of cervical cancer due to HPV strains 16 and 18}
#'   \item{Vaccine programme}{Vaccine programme}
#'   \item{Vaccine programme Oct2013}{Vaccine programme Oct2013}
#'   \item{Econ evaluation}{Economic evaluation}
#'   \item{GDP per capita (2011 US$) [7]}{GDP per capita (2011 US$)}
#'   \item{GDP per capita (2011 i$) [7]}{GDP per capita (2011 i$)}
#'   \item{GNI per capita (2011 i$) [7]}{GNI per capita (2011 i$)}
#'   \item{GNI per capita (2011 US$) [7]}{GNI per capita (2011 US$)}
#'   \item{V33}{International$}
#'   \item{V34}{US$}
#'   \item{iso3}{ISO3 country code}
#' }
"data.global"


#' WHO life table
#'
#' A dataset containing the WHO life table.
#'
#' @format A data table with 196 observations of 107 variables.
#' \describe{
#'   \item{Country ¦ Age [12]}{Country name}
#'   \item{0..100}{Age 0-100}
#'   \item{cancer_cost}{cost per cancer episode, in $US}
#'   \item{cancer_cost_adj}{cost per cancer episode -- adjusted, in international/PPP $}
#'   \item{iso3}{ISO3 country code}
#' }
#' @source \url{}
"data.mortall"


#' Cost of cervical cancer treatment
#'
#' A dataset containing the cost of cervical cancer treatment.
#'
#' @format A data table with 194 observations of 4 variables.
#' \describe{
#'   \item{country}{Country name}
#'   \item{cancer_cost}{cost per cancer episode, in $US}
#'   \item{cancer_cost_adj}{cost per cancer episode -- adjusted, in international/PPP $}
#'   \item{iso3}{ISO3 country code}
#' }
#' @source \url{}
"data.mortall.unwpp.mx"


#' Cost of cervical cancer treatment
#'
#' A dataset containing the cost of cervical cancer treatment.
#'
#' @format A data table with 194 observations of 4 variables.
#' \describe{
#'   \item{country}{Country name}
#'   \item{cancer_cost}{cost per cancer episode, in $US}
#'   \item{cancer_cost_adj}{cost per cancer episode -- adjusted, in international/PPP $}
#'   \item{iso3}{ISO3 country code}
#' }
#' @source \url{}
"data.mortcecx"


#' Cost of cervical cancer treatment
#'
#' A dataset containing the cost of cervical cancer treatment.
#'
#' @format A data table with 194 observations of 4 variables.
#' \describe{
#'   \item{country}{Country name}
#'   \item{cancer_cost}{cost per cancer episode, in $US}
#'   \item{cancer_cost_adj}{cost per cancer episode -- adjusted, in international/PPP $}
#'   \item{iso3}{ISO3 country code}
#' }
#' @source \url{}
"data.pop"


#' Cost of cervical cancer treatment
#'
#' A dataset containing the cost of cervical cancer treatment.
#'
#' @format A data table with 194 observations of 4 variables.
#' \describe{
#'   \item{country}{Country name}
#'   \item{cancer_cost}{cost per cancer episode, in $US}
#'   \item{cancer_cost_adj}{cost per cancer episode -- adjusted, in international/PPP $}
#'   \item{iso3}{ISO3 country code}
#' }
#' @source \url{}
"data.popproj"


#' Cost of cervical cancer treatment
#'
#' A dataset containing the cost of cervical cancer treatment.
#'
#' @format A data table with 194 observations of 4 variables.
#' \describe{
#'   \item{country}{Country name}
#'   \item{cancer_cost}{cost per cancer episode, in $US}
#'   \item{cancer_cost_adj}{cost per cancer episode -- adjusted, in international/PPP $}
#'   \item{iso3}{ISO3 country code}
#' }
#' @source \url{}
"data.quality"


#' Cost of cervical cancer treatment
#'
#' A dataset containing the cost of cervical cancer treatment.
#'
#' @format A data table with 194 observations of 4 variables.
#' \describe{
#'   \item{country}{Country name}
#'   \item{cancer_cost}{cost per cancer episode, in $US}
#'   \item{cancer_cost_adj}{cost per cancer episode -- adjusted, in international/PPP $}
#'   \item{iso3}{ISO3 country code}
#' }
#' @source \url{}
"data.sexual_debut"


#' Global data table
#'
#' A dataset containing a global range of variables.
#'
#' @format A data table with 194 observations of 35 variables.
#' \describe{
#'   \item{Country}{Country name}
#'   \item{iso2}{ISO2 country code}
#' }
#' @source \url{}
"data.valid"
