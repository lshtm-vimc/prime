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


#' Incidence of cervical cancer
#'
#' A dataset containing the incidence of cervical cancer in 185 countries, as reported by
#'     IARC's Globocan 2018 database.
#'
#' @format A data table with 185 observations of 103 variables.
#' \describe{
#'   \item{Country}{Country name}
#'   \item{0..100}{Age 0-100}
#'   \item{iso3}{ISO3 country code}
#' }
#' @source \url{https://gco.iarc.fr/today/online-analysis-table}
"data.incidence"


#' WHO life table
#'
#' A dataset containing the WHO life table.
#'
#' @format A data table with 196 observations of 107 variables.
#' \describe{
#'   \item{Country ¦ Age [12]}{Country name}
#'   \item{0..100}{Age 0-100}
#'   \item{V103..V106}{na}
#'   \item{iso3}{ISO3 country code}
#' }
#' @source \url{https://www.who.int/gho/mortality_burden_disease/life_tables/life_tables/en/}
"data.mortall"


#' UNWPP life table
#'
#' A dataset containing the UNWPP life table (World Population Prospects 2017).
#'
#' @format A data table with 115710 observations of 8 variables.
#' \describe{
#'   \item{country_code_numeric}{Country code numeric}
#'   \item{country_code}{ISO3 country code}
#'   \item{country}{Country name}
#'   \item{age_from}{age from (start-age)}
#'   \item{age_to}{age to (end-age)}
#'   \item{year}{Year}
#'   \item{gender}{Gender}
#'   \item{value}{mx mortality rate in year = proportion of individuals of age x dying by age x+1}
#' }
#' @source \url{https://population.un.org/wpp/}
"data.mortall.unwpp.mx"


#' Mortality from cervical cancer
#'
#' A dataset containing the mortality from cervical cancer in 185 countries, as reported by
#'     IARC's Globocan 2018 database.
#'
#' @format A data table with 185 observations of 103 variables.
#' \describe{
#'   \item{Country}{Country name}
#'   \item{0..100}{Age 0-100}
#'   \item{iso3}{ISO3 country code}
#' }
#' @source \url{https://gco.iarc.fr/today/online-analysis-table}
"data.mortcecx"


#' UNWPP population estimates
#'
#' A dataset containing the UNWPP population estimates -- World Population Prospects 2017.
#'
#' @format A data table with 3306163 observations of 8 variables.
#' \describe{
#'   \item{country_code_numeric}{Country code numeric}
#'   \item{country_code}{ISO3 country code}
#'   \item{country}{Country name}
#'   \item{age_from}{age from (start-age)}
#'   \item{age_to}{age to (end-age)}
#'   \item{year}{Year}
#'   \item{gender}{Gender}
#'   \item{value}{Population size}
#' }
#' @source \url{https://population.un.org/wpp/Download/Standard/Population/}
#' @source {VIMC}
"data.pop"


#' Population projections of 5-year old girls
#'
#' A dataset containing population projections of 5-year old girls.
#'
#' @format A data table with 98 observations of 91 variables.
#' \describe{
#'   \item{iso3}{ISO3 country code}
#'   \item{2011..2100}{Year -- 2011..2100}
#' }
"data.popproj"


#' Data quality of incidence and mortality
#'
#' A dataset indicating data quality of cervical cancer incidence and mortality.
#'
#' @format A data table with 186 observations of 4 variables.
#' \describe{
#'   \item{Country}{Country name}
#'   \item{Incidence}{Quality of cervical cancer incidence data}
#'   \item{Mortality}{Quality of cervical cancer mortality data}
#'   \item{iso3}{ISO3 country code}
#' }
"data.quality"


#' Sexual debut data
#'
#' A dataset containing sexual debut data and (2) parameters for the sexual debut curve (logistic model).
#'
#' @format A data table with 94 observations of 14 variables.
#' \describe{
#'   \item{V1}{Row number}
#'   \item{iso2}{ISO2 country code}
#'   \item{country}{Country name}
#'   \item{iso3}{ISO3 country code}
#'   \item{who}{WHO region}
#'   \item{X15}{Proportion of people who have sexually debuted at age 15}
#'   \item{X18}{Proportion of people who have sexually debuted at age 18}
#'   \item{X20}{Proportion of people who have sexually debuted at age 20}
#'   \item{X22}{Proportion of people who have sexually debuted at age 22}
#'   \item{X25}{Proportion of people who have sexually debuted at age 25}
#'   \item{Never}{Proportion of people who had not sexually debuted}
#'   \item{cluster.id}{Clustering countries with similar characteristics}
#'   \item{a}{Parameter for sexual debut curve (logistic model)}
#'   \item{b}{Parameter for sexual debut curve (logistic model)}
#' }
"data.sexual_debut"


#' Model validation
#'
#' A dataset containing data for validation.
#'
#' @format A data table with 26 observations of 49 variables.
#' \describe{
#'   \item{Country}{Country name}
#'   \item{iso2}{ISO2 country code}
#'   \item{WHO Region}{WHO region}
#'   \item{World Bank Income Group (2011)}{World Bank income group classification (2011)}
#'   \item{Author}{Author}
#'   \item{Year}{Year of publication}
#'   \item{Title}{Title of publication}
#'   \item{Currency}{Currency}
#'   \item{Currency year}{Currency year}
#'   \item{Conversion to I$2011}{International dollar (I$2011)}
#'   \item{ICER vs no prevention}{Incremental cost-effectiveness ratio of vaccination versus no prevention (CHECK)}
#'   \item{ICER vs screen}{Incremental cost-effectiveness ratio of vaccination versus no prevention (CHECK)}
#'   \item{Denominator}{Denominator for health impact}
#'   \item{Vaccine total costs}{Vaccine total costs}
#'   \item{Vaccine coverage}{Vaccine coverage}
#'   \item{Vaccine efficacy vs vaccine type infection}{Vaccine efficacy versus vaccine type infection}
#'   \item{Duration of protection}{Duration of protective immunity from vaccination}
#'   \item{Cohort size}{Cohort size}
#'   \item{Age at vaccination}{Age at vaccination, years}
#'   \item{Cancer treatment cost per episode}{Cancer treatment cost per episode, dollars}
#'   \item{Discount rate: costs}{Discount rate for costs}
#'   \item{Discount rate: benefits}{Discount rate for benefits}
#'   \item{Perspective}{Perspective of economic evaluation}
#'   \item{Comparator is no screening}{Comparator refers to no scereening, logical (Y/N)}
#'   \item{Time horizon}{Time horizon of analysis}
#'   \item{GDP per capita}{GDP per capita}
#'   \item{Cervical cancer due to 16/18}{Proportion of cervical cancer due to HPV types 16 and 18}
#'   \item{CeCx cost low original}{Cervical cancer cost / low / original}
#'   \item{CeCx cost high original}{Cervical cancer cost / high / original}
#'   \item{CeCx cost low}{Cervical cancer cost / low}
#'   \item{CeCx cost high}{Cervical cancer cost / high}
#'   \item{CeCx data available}{Cervical cancer data available, logical (Y/N)}
#'   \item{0-4}{0-4 years}
#'   \item{5-9}{5-9 years}
#'   \item{9-14}{9-14 years}
#'   \item{15-19}{15-19 years}
#'   \item{20-24}{20-24 years}
#'   \item{25-29}{25-29 years}
#'   \item{30-34}{30-34 years}
#'   \item{35-39}{35-39 years}
#'   \item{40-44}{40-44 years}
#'   \item{45-49}{45-49 years}
#'   \item{50-54}{50-54 years}
#'   \item{55-59}{55-59 years}
#'   \item{60-64}{60-64 years}
#'   \item{65-69}{65-69 years}
#'   \item{70-74}{70-74 years}
#'   \item{75-79}{75-79 years}
#'   \item{80+}{80+ years}
#' }
"data.valid"


#' Disability weights and duration of cervical cancer stages
#'
#' A dataset containing disability weights and duration of different phases of cervical cancer.
#'
#' @format A data table with 13 observations of 8 variables.
#' \describe{
#'   \item{Source}{Source of disability weights - IHME / WHO}
#'   \item{Sequela}{Sequelae / stage / phase of cervical cancer}
#'   \item{Duration}{Duration of cervical cancer phase}
#'   \item{WHO_MortalityStratum}{WHO moratlity stratum -- applicable only for long term sequelae from WHO source}
#'   \item{Mid}{Disability weight (mid)}
#'   \item{Low}{Disability weight (low)}
#'   \item{High}{Disability weight (high)}
#'   \item{Description}{Description of cervical cancer phase}
#' }
"data.disability_weights"
