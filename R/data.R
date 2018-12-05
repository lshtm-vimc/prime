# Data.R - This script is for documenting datasets.


#' 1-year prevalence of cervical cancer
#'
#' A dataset containing the 1-year prevalence (proportion) of cervical cancer in 185 countries, as reported by
#'     IARC's Globocan 2018 database.
#'
#' As per IARC definition -- The (1-year) prevalence of a given cancer
#'     is the number of individuals within a defined population who have been diagnosed with that
#'     cancer (within 1 year) and who are still alive at a given point in time (i.e. the survivors).
#'
#' @format A data table containing 185 observations of 103 variables (185 row * 103 col).
#' \describe{
#'   \item{Country}{Country name}
#'   \item{Age}{Age 0-100}
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
#' @format A data table containing 185 observations of 103 variables (185 row * 103 col).
#' \describe{
#'   \item{Country}{Country name}
#'   \item{Age}{Age 0-100}
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
#' @format A data table with 185 observations of 103 variables (185 row * 103 col).
#' \describe{
#'   \item{Country}{Country name}
#'   \item{Age}{Age 0-100}
#'   \item{iso3}{ISO3 country code}
#' }
#' @source \url{https://gco.iarc.fr/today/online-analysis-table}
"data.cecx_5y_prevalence"


#' Cost of cervical cancer treatment
#'
#' A dataset containing the cost of cervical cancer treatment.
#'
#' @format A data table with 194 observations of 4 variables (194 row * 4 col).
#' \describe{
#'   \item{country}{Country name}
#'   \item{cancer_cost}{cost per cancer episode, in $US}
#'   \item{cancer_cost_adj}{cost per cancer episode -- adjusted, in international/PPP $}
#'   \item{iso3}{ISO3 country code}
#' }
#' @source \url{}
"data.costcecx"


#' Country names and codes.
#'
#' A dataset containing the country names and codes (ISO/WHO/WB/UN).
#'
#' @format A data table with 251 observations of 13 variables (251 row * 13 col).
#' \describe{
#'   \item{name1, name2, name3, name4}{country names}
#'   \item{name2}{}
#'   ...
#' }
#' @source \url{http://www.diamondse.info/}
"data.countryname"


#' Prices of 50,000 round cut diamonds.
#'
#' A dataset containing the prices and other attributes of almost 54,000
#' diamonds.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{price}{price, in US dollars}
#'   \item{carat}{weight of the diamond, in carats}
#'   ...
#' }
#' @source \url{http://www.diamondse.info/}
"data.global"


#' Prices of 50,000 round cut diamonds.
#'
#' A dataset containing the prices and other attributes of almost 54,000
#' diamonds.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{price}{price, in US dollars}
#'   \item{carat}{weight of the diamond, in carats}
#'   ...
#' }
#' @source \url{http://www.diamondse.info/}
"data.incidence"


#' Prices of 50,000 round cut diamonds.
#'
#' A dataset containing the prices and other attributes of almost 54,000
#' diamonds.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{price}{price, in US dollars}
#'   \item{carat}{weight of the diamond, in carats}
#'   ...
#' }
#' @source \url{http://www.diamondse.info/}
"data.mortall"


#' Prices of 50,000 round cut diamonds.
#'
#' A dataset containing the prices and other attributes of almost 54,000
#' diamonds.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{price}{price, in US dollars}
#'   \item{carat}{weight of the diamond, in carats}
#'   ...
#' }
#' @source \url{http://www.diamondse.info/}
"data.mortall.unwpp.mx"


#' Prices of 50,000 round cut diamonds.
#'
#' A dataset containing the prices and other attributes of almost 54,000
#' diamonds.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{price}{price, in US dollars}
#'   \item{carat}{weight of the diamond, in carats}
#'   ...
#' }
#' @source \url{http://www.diamondse.info/}
"data.mortcecx"


#' Prices of 50,000 round cut diamonds.
#'
#' A dataset containing the prices and other attributes of almost 54,000
#' diamonds.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{price}{price, in US dollars}
#'   \item{carat}{weight of the diamond, in carats}
#'   ...
#' }
#' @source \url{http://www.diamondse.info/}
"data.pop"


#' Prices of 50,000 round cut diamonds.
#'
#' A dataset containing the prices and other attributes of almost 54,000
#' diamonds.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{price}{price, in US dollars}
#'   \item{carat}{weight of the diamond, in carats}
#'   ...
#' }
#' @source \url{http://www.diamondse.info/}
"data.popproj"


#' Prices of 50,000 round cut diamonds.
#'
#' A dataset containing the prices and other attributes of almost 54,000
#' diamonds.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{price}{price, in US dollars}
#'   \item{carat}{weight of the diamond, in carats}
#'   ...
#' }
#' @source \url{http://www.diamondse.info/}
"data.quality"


#' Prices of 50,000 round cut diamonds.
#'
#' A dataset containing the prices and other attributes of almost 54,000
#' diamonds.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{price}{price, in US dollars}
#'   \item{carat}{weight of the diamond, in carats}
#'   ...
#' }
#' @source \url{http://www.diamondse.info/}
"data.sexual_debut"


#' Prices of 50,000 round cut diamonds.
#'
#' A dataset containing the prices and other attributes of almost 54,000
#' diamonds.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{price}{price, in US dollars}
#'   \item{carat}{weight of the diamond, in carats}
#'   ...
#' }
#' @source \url{http://www.diamondse.info/}
"data.valid"
