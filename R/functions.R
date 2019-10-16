# functions.R - This program includes the functions used in the PRIME model.


#' Simulation log reporting
#'
#' Appends message of simulation run (\code{x}) to log file (\code{logname}).
#'
#' @param logname log filename
#' @param x message of simulation run
#'
#' @return None
#' @export
#'
#' @examples #
#'
writelog <- function (logname,
                      x) {

  # wait until log file is yours
  Sys.sleep (0.02)

  while (file.exists (paste0 (logname, "_locked") ) ) {
    Sys.sleep (0.02)
  }

  # lock log file
  file.create (paste0 (logname,"_locked"))

  # write to log file
  write ( paste0 (format (Sys.time(), "%Y/%m/%d %H:%M:%S"), " ", x),
          file   = logname,
          append = TRUE )

  # unlock log file
  file.remove (paste0 (logname,"_locked") )

} # end of function -- writelog


#' Creates .data.batch for running multiple birth cohorts
#'
#' Creates .data.batch which is used when running/looping over multiple birth
#'   cohorts ( runCohort() ) at once.
#'
#' .data.batch is based on the data.table (DT) coverage_data, which is a DT with
#'   columns country_code (ISO3), year (of vaccination),
#'   age_first (age at vaccination), age_last (age at vaccination),
#'   coverage (in proportion, for all the agegroups specified).
#'
#' If you only want to run 1 age in this country/coverage combination,
#'   age_first==age_last
#'
#' @param coverage_data Data table with columns country_code,
#'        year (of vaccination), age_first, age_last, coverage.
#' @param reporting_years Numeric_vector, years that should be reported
#'        (parameter: not required)
#' @param force Logical, whether .data.batch should be overwritten if it already
#'        exists (parameter: not required)
#'
#' @return None
#' @export
#'
#' @examples #
RegisterBatchData <- function (coverage_data,
                               reporting_years = -1,
                               force           = FALSE) {

  if (exists(".data.batch") & !force) {
    stop ("'.data.batch' already exists.")
  }

  macs   <- coverage_data[age_first != age_last]
  nomacs <- coverage_data[age_first == coverage_data$age_last]

  if (nrow(macs) > 0) {
    for(m in 1:nrow(macs)) {
      ages <- macs[m,age_first]:macs[m,age_last]
      for(a in ages){
        nomacs <- rbindlist(
          list(
            nomacs,
            macs[m,]
          )
        )
        nomacs[nrow(nomacs),"age_first"] <- a
        nomacs[nrow(nomacs),"age_last"]  <- a
      }
    }
    coverage_data <- nomacs
  }

  coverage_data [, "birthcohort"] <-
    coverage_data [, year] - coverage_data [, age_first]

  setorder (coverage_data, country_code, birthcohort, age_first)

  demographic_years <- sort (as.numeric (
    unique (data.pop [country_code %in%
                        unique (coverage_data [, country_code]), year])
    ))
  coverage_years <- sort (as.numeric (
    unique (coverage_data$birthcohort)
    ))

  if (length(reporting_years) > 1 && reporting_years != -1) {
    reporting_years <- sort (
      unique (
        c (
          min (coverage_data [, year] - coverage_data [, age_last]):
            max ((coverage_data [, year] - coverage_data [, age_first]) + 100)
        )
      )
    )
    min_year <- min (reporting_years)
    max_year <- max (reporting_years)
  } else {
    min_year <- min (coverage_years)
    max_year <- max (coverage_years)
  }

  if (min(coverage_years) > min_year) {
    year_first <- min (coverage_years)
  } else {
    year_first <- min_year
  }
  if (max(demographic_years) < max_year) {
    year_last <- max (demographic_years)
  } else {
    year_last <- max_year
  }

  birthcohorts <- year_first:year_last
  template     <- coverage_data [birthcohort==coverage_years[1]]

  # remove birthcohorts which should not be modelled
  coverage_data <- coverage_data [birthcohort %in% birthcohorts]

  # select birthcohorts which are not yet in the coverage-data
  birthcohorts <- birthcohorts [!(birthcohorts %in% coverage_data[,birthcohort])]

  # add birthcohorts to coverage data
  for (b in birthcohorts) {
    t_coverage_data <- template
    t_coverage_data [, "birthcohort"] <- b
    t_coverage_data [, "coverage"]    <- 0
    coverage_data <- rbindlist (
      list(
        coverage_data,
        t_coverage_data
      )
    )
  }

  colnames (coverage_data) [which (colnames (coverage_data) == "age_first")] <-
    "agevac"

  coverage_data [, "activity_type"] <- "routine"
  coverage_data [, "target"] <- NA

  coverage_data <- coverage_data[ , c("country_code",
                                      "birthcohort",
                                      "coverage",
                                      "agevac",
                                      "activity_type",
                                      "target")
                                  ]

  setorder (coverage_data, country_code, birthcohort, agevac)

  .data.batch <<- coverage_data

} # end of function -- RegisterBatchData


#' Creates .data.batch for running multiple birth cohorts (VIMC runs)
#'
#' Creates .data.batch which is used when running/looping over multiple birth
#'   cohorts ( runCohort() ) at once. Similar to RegisterBatchData, but for when
#'   we make runs for VIMC.
#'
#' .data.batch is based on the data.table (DT) coverage_data, which is a DT with
#'   columns country_code (ISO3), year (of vaccination),
#'   age_first (age at vaccination), age_last (age at vaccination),
#'   coverage (in proportion, for all the age groups specified).
#'
#' @param vimc_coverage data table with coverage estimates as downloaded from
#'   VIMC montagu
#' @param vimc_template data table with reporting template as downloaded from
#'   VIMC montagu
#' @param use_campaigns logical, whether campaigns as stated in coverage files
#'   should be modelled
#' @param use_routine logical, whether routine vaccination as stated in coverage
#'   file should be modelled
#' @param restrict_to_coverage_data logical, whether the first birth-cohort
#'   should be the first cohort that is mentioned in the coverage data.
#'     If TRUE, restrict to coverage data.
#'     If FALSE, restrict to cohorts provided in vimc_template.
#' @param force logical, whether .data.batch should be overwritten if it already
#'   exists
#' @param psa integer, indicating how many runs for probabilistic sensitivity
#'   analysis (PSA). 0 to run no PSA.
#'
#' @return None
#' @export
#'
#' @examples #
RegisterBatchDataVimc <- function (vimc_coverage,
                                   vimc_template,
                                   use_campaigns,
                                   use_routine,
                                   restrict_to_coverage_data = FALSE,
                                   force                     = FALSE,
                                   psa                       = 0) {

  if (exists(".data.batch") & !force) {
    stop("'.data.batch' already exists.")
  }

  if (use_campaigns) {
    coverage_data <- vimc_coverage[activity_type=="routine" |
                                     (activity_type=="campaign" &
                                        coverage > 0)]
  } else {
    coverage_data <- vimc_coverage[activity_type=="routine"]
  }
  if (!use_routine) {
    coverage_data[activity_type=="routine" & coverage > 0, "coverage"] <- 0
  }

  coverage_data [target=="<NA>","target"] <- NA
  class (coverage_data$target) <- "numeric"
  coverage_data <- coverage_data [country_code %in% unique(vimc_template[,country])]
  macs <- coverage_data [age_first != age_last]
  nomacs <- coverage_data [age_first == coverage_data$age_last]

  if (nrow(macs) > 0) {
    for (m in 1:nrow(macs)) {
      ages <- macs[m,age_first]:macs[m,age_last]
      for (a in ages) {
        nomacs <- rbindlist(
          list(
            nomacs,
            macs[m,]
          )
        )
        if(a == ages[1]) {
          target <- as.numeric (nomacs[nrow(nomacs),target])
        }
        nomacs [nrow(nomacs),"age_first"] <- a
        nomacs [nrow(nomacs),"age_last"]  <- a
        #spread size of target evenly over age-strata targetted
        nomacs [nrow(nomacs),"target"] <- target/length(ages)
      }
    }
    coverage_data <- nomacs
  }
  coverage_data[,"birthcohort"] <- coverage_data[,year] - coverage_data[,age_first]
  setorder(coverage_data, country_code, birthcohort, age_first)

  # get years that should be reported in template
  reporting_years <- sort(as.numeric(unique(vimc_template$year)))

  # get years for which there is demographic data
  demographic_years <- sort(as.numeric(unique(data.pop[country_code %in% unique(coverage_data[, country_code]), year])))

  # get birthcohorts for which we have coverage data
  coverage_years <- sort(as.numeric(unique(coverage_data$birthcohort)))

  # get min and max birthcohorts that should be modelled, following template
  min_year <- min(reporting_years) -
    max(vimc_template[year==min(reporting_years),age])
  max_year <- max(reporting_years) -
    min(vimc_template[year==max(reporting_years),age])

  # restrict to years for which we have demographic data, or to years for which
  # we have coverage data
  if (!restrict_to_coverage_data) {
    if (min(demographic_years) > min_year) {
      year_first <- min(demographic_years)
    } else {
      year_first <- min_year
    }
  } else {
    if (min(coverage_years) > min_year) {
      year_first <- min(coverage_years)
    } else {
      year_first <- min_year
    }
  }

  # restrict to years for which we have demographic data
  if (max(demographic_years) < max_year) {
    year_last <- max(demographic_years)
  } else {
    year_last <- max_year
  }

  birthcohorts <- year_first:year_last

  # remove birthcohorts which should not be modelled
  coverage_data <- coverage_data [birthcohort %in% birthcohorts]

  # sort by country code
  countries <- sort ( unique( coverage_data[,country_code] ) )

  for(c in countries) {

    # create template for data not yet in coverage-data
    template <- coverage_data [
      country_code  == c &
      activity_type == "routine" &
      birthcohort   == min (coverage_data [country_code == c &
                                             activity_type == "routine",
                                           birthcohort] ) ]

    # select birthcohorts which are not yet in the coverage-data and add to data
    missing_birthcohorts <- birthcohorts[!(birthcohorts %in% coverage_data[country_code == c,birthcohort])]

    if (length(missing_birthcohorts) > 0 ) {
      for (b in missing_birthcohorts) {
        t_coverage_data <- template
        t_coverage_data [,"birthcohort"] <- b
        t_coverage_data [,"coverage"]    <- 0
        coverage_data <- rbindlist (
          list(
            coverage_data,
            t_coverage_data
          )
        )
      }
    }
  }
  colnames (coverage_data)[which(colnames(coverage_data)=="age_first")] <- "agevac"
  coverage_data <- coverage_data [, c("country_code",
                                      "birthcohort",
                                      "coverage",
                                      "agevac",
                                      "activity_type",
                                      "target")
                                  ]

  # model countries without coverage data but in template (with coverage-level of 0)
  countries <- sort (unique(vimc_template[, country]))
  countries <- countries [!(countries %in% unique(coverage_data[, country_code]))]

  if (length(countries) > 0) {
    for (c in countries) {
      t_coverage_data <- coverage_data[country_code == unique (coverage_data[,country_code])[1]]
      t_coverage_data [,"target"] <- 0
      t_coverage_data [,"coverage"] <- 0
      t_coverage_data [,"country_code"] <- c

      coverage_data <- rbindlist (
        list(
          t_coverage_data,
          coverage_data
        )
      )
    }
  }

  setorder (coverage_data, country_code, birthcohort, agevac)
  .data.batch <<- coverage_data

  # sort by country code
  countries <- sort (unique(.data.batch[,country_code]))

  # ----------------------------------------------------------------------------
  # ----------------------------------------------------------------------------
  # Note: The following code chunk for PSA is redundant -- grandfathered
  # from older version. The newer version does not use data.quality to conduct
  # psa. Instead it uses uncertainty intervals in burden estimates and
  # disability weights to conduct probabilistic sensitivity analysis.
  # ----------------------------------------------------------------------------
  # sensitivity analysis
  if (psa > 1) {
    psadat <- data.table(
      country = character(0),
      run_id = numeric(0),
      incidence = numeric(0),
      mortality = numeric(0)
    )
    for (c in countries){
      # select proxy country if data not available
      tc <- switch(
        c,
        "XK"  = "ALB",  # demography data available for XK but no burden & cost
        "PSE" = "JOR",  # burden & demography data available for PSE but no cost
        # "MHL"="KIR",
        # "TUV"="FJI",
        # "SSD"="SDN",
        c
      )
      inc_quality  <- data.quality [iso3==tc, Incidence]
      mort_quality <- data.quality [iso3==tc, Mortality]
      if (inc_quality == 0){
        inc_quality <- c(0.5,1.5)
      } else {
        inc_quality <- c(0.8,1.2)
      }
      if(mort_quality == 0){
        mort_quality <- c(0.5,1.5)
      } else {
        mort_quality <- c(0.8,1.2)
      }
      psadat <- rbindlist(
        list(
          psadat,
          data.table(
            country = rep(c, psa),
            psa = c(1:psa),
            incidence = runif (n=psa, min=inc_quality[1], max=inc_quality[2]),
            mortality = runif (n=psa, min=inc_quality[1], max=mort_quality[2])
          )
        )
      )
    }

    if (exists(".data.batch") & !force) {
      warning("'.data.batch.psa' already exists and is NOT overwritten.")
    } else {
      .data.batch.psa <<- psadat
    }
  }
  # ----------------------------------------------------------------------------
  # ----------------------------------------------------------------------------

} # end of function -- RegisterBatchDataVimc


#' Run multiple cohorts in a batch
#'
#' Runs multiple cohorts in one batch, based on the data in .data.batch
#'
#' @param countries ignore, read from .data.batch
#' @param coverage ignore, read from .data.batch
#' @param agevac ignore, read from .data.batch
#' @param agecohort ignore, read from .data.batch
#' @param canc.inc year from where incidence data is read
#'        (2018; old data: 2012) --
#'        with updated 2018 Globocan data,
#'        DALY weights from GBD,
#'        and DALY estimation based on prevalence instead of age of incidence
#' @param sens ignore, does not do anything anymore
#' @param unwpp_mortality logical, whether to create lifetables based on
#'        UNWPP mx estimates or WHO data
#' @param year_born ignore
#' @param year_vac ignore
#' @param runs ignore
#' @param vaccine_efficacy_beforesexdebut vaccine efficacy before sexual debut
#' @param vaccine_efficacy_aftersexdebut vaccine efficacy after sexual debut
#' @param log name of log file
#' @param by_calendaryear logical, output values by calendar year or
#'        by year of birth cohort
#' @param use_proportions logical, output data as rates per capita or in totals
#' @param analyseCosts logical, directly run cost-effectiveness analysis
#'        on output or not
#' @param canc.cost Character (optional): Is cost of cancer adjusted
#'        ("adj" for International $) or not ("unadj" for US$)
#' @param discounting Logical (optional): If TRUE, run cost-effectiveness analysis
#'        undiscounted and discounted. If FALSE, only uses undiscounted
#' @param disc.cost Number (optional): Discounting for health costs
#'        (only if discounting=TRUE)
#' @param disc.ben Number (optional): Discounting for health outcomes
#'        (only if discounting=TRUE)
#' @param psa integer, number of runs for probabilistic sensitivity analysis (PSA)
#' @param psa_vals data table with values to use in probabilistic sensitivity
#'        analysis, usually .data.batch.psa, generated by
#'        RegisterBatchData* functions (currently only RegisterBatchDataVimc)
#' @param disability.weights character, disability weights for cervical cancer
#'        from GBD 2017 or GBD 2001
#' @param wb.indicator character, World Bank indicator for GDP/GNI per capita
#'        in I$/US$ and current/constant data
#' @param wb.year numeric, year of the World Bank indicator value
#' @return Returns combined results
#' @export
#'
#' @examples #
BatchRun <- function (countries                       = -1,
                      coverage                        = -1,
                      agevac                          = -1,
                      agecohort                       = -1,
                      canc.inc                        = "2018",
                      sens                            = -1,
                      unwpp_mortality                 = TRUE,
                      year_born                       = -1,
                      year_vac                        = -1,
                      runs                            = 1,
                      vaccine_efficacy_beforesexdebut = 1,
                      vaccine_efficacy_aftersexdebut  = 0,
                      log                             = -1,
                      by_calendaryear                 = FALSE,
                      use_proportions                 = TRUE,
                      analyseCosts                    = FALSE,
                      canc.cost                       = "unadj",
                      discounting                     = FALSE,
                      disc.cost                       = 0.03,
                      disc.ben                        = 0.03,
                      psa                             = 0,
                      psa_vals                        = ".data.batch.psa",
                      disability.weights              = "gbd_2017",
                      wb.indicator                    = "NY.GDP.PCAP.PP.CD",
                      wb.year                         = 2017
                      ) {

  ages <- as.numeric(colnames(data.incidence)[!grepl("\\D",colnames(data.incidence))])
  ages <- ages[!is.na(ages)]

  if(countries==-1){
    countries <- sort(unique(.data.batch[,country_code]))
  }

  if(year_vac==-1 & year_born==-1){
    years <- sort(unique(.data.batch[,birthcohort]))
  }

  if(psa>1){
    psadat <- get(".data.batch.psa")
    if(length(unique(psadat[,run_id])) != psa){
      stop ("Number of specified PSA does not correspond to number of run ids in PSA file")
    } else {
      runs <- psa
      dopsa <- TRUE
    }
  } else {
    dopsa <- FALSE
    runs <- 1
    psadat <- -1
  }

  #create initial variables that won't be overwritten in foreach loops
  init_coverage <- coverage
  init_agevac <- agevac
  init_agecohort <- agecohort
  combine <- foreach(
    cn = 1:length(countries),
    .packages=c("data.table","prime"),
    # .errorhandling="pass",
    .export=c(".data.batch","data.pop", "writelog")
  ) %:% foreach(
    y=1:length(years)
  ) %:% foreach(
    r=1:runs
  ) %dopar% {
    #) %do% {
    .t_data.batch <- .data.batch [country_code == countries[cn] &
                                    birthcohort == years[y]]
    if(init_coverage==-1){
      coverage <- .t_data.batch[1, coverage]
    } else {
      coverage <- init_coverage
    }
    if(init_agevac==-1){
      agevac <- .t_data.batch[1, agevac]
    } else {
      agevac <- init_agevac
    }
    if(init_agecohort==-1 & agevac>1){
      agecohort <- agevac-1
      #agecohort <- agevac
    } else if(init_agecohort==-1){
      agecohort <- 1
    } else {
      agecohort <- init_agecohort
    }

    cohort <- data.pop [country_code == countries[cn] &
                          year       == (years[y] + agecohort) &
                          age_from   == agecohort,
                        value]

    if(is.character(log)){
      writelog(
        log,
        paste0(
          "country: '",
          countries[cn],
          "'; birthcohort: '",
          years[y],
          "'; run: '",
          r,
          "'; agecohort: '",
          agecohort,
          "'; agevac: '",
          agevac,
          "'; coverage: '",
          coverage,
          "'; cohort_size: '",
          cohort,
          "'; .t_data.batch (rows): '",
          nrow(.t_data.batch),
          "';"
        )
      )
    }

    if (nrow(.t_data.batch)>1) {
      campaigns <- list()
      for(cmp in 2:nrow(.t_data.batch)){
        campaigns[[length(campaigns)+1]] <- list(
          "year" = .t_data.batch[cmp,birthcohort],
          "ages" = .t_data.batch[cmp,agevac],
          # add type and coverage data -
          # if type is 'routine', coverage is already a proportion;
          # if type is 'campaign', proportion will be calculated from target population
          "type" = .t_data.batch[cmp,activity_type],
          #"coverage" = switch(
          #  .t_data.batch[cmp,activity_type],
          #  "routine" = .t_data.batch[cmp,coverage],
          #  "campaign" = .t_data.batch[cmp,coverage]*.t_data.batch[cmp,target]
          #)
          "coverage" = .t_data.batch[cmp,coverage]
        )
      }
    } else {
      campaigns <- -1
    }

    if (dopsa) {
      cpsadat <- list(
        incidence = psadat[country == countries[cn] & run_id == r, incidence],
        mortality = psadat[country == countries[cn] & run_id == r, mortality]
      )
    } else {
      cpsadat <- list(
        incidence = 1,
        mortality = 1
      )
    }

    data <- RunCountry (
      country_iso3          = countries[cn],
      vaceff_beforesexdebut = vaccine_efficacy_beforesexdebut,
      vaceff_aftersexdebut  = vaccine_efficacy_aftersexdebut,
      cov                   = coverage,
      agevac                = agevac,
      agecohort             = agecohort,
      cohort                = cohort,
      canc.inc              = canc.inc,
      unwpp_mortality       = unwpp_mortality,
      year_born             = years[y],
      campaigns             = campaigns,
      run_batch             = TRUE,
      analyseCosts          = analyseCosts,
      canc.cost             = canc.cost,
      discounting           = discounting,
      disc.cost             = disc.cost,
      disc.ben              = disc.ben,
      psadat                = cpsadat,
      disability.weights    = disability.weights,
      wb.indicator          = wb.indicator,
      wb.year               = wb.year
    )

    data [,"country"]     <- countries [cn]
    data [,"birthcohort"] <- years [y]

    if (dopsa) {
      data [,"run_id"] <- r
    }
    return(data)

  }  # end of foreach

  for (l in 1:length(combine)) {
    for (i in 1:length(combine[[l]])) {
      combine[[l]][[i]] <- rbindlist(combine[[l]][[i]])
    }
    combine[[l]] <- rbindlist(combine[[l]])
  }
  combine <- rbindlist(combine)

  if (by_calendaryear) {
    combine[, "birthcohort"] <- combine[,birthcohort] + combine[,age]
    colnames(combine)[colnames(combine) == "birthcohort"] <- "year"
  }
  if (!use_proportions) {
    combine[, "vaccinated"] <- combine[,vaccinated]*combine[,cohort_size]
    combine[, "immunized"]  <- combine[,immunized]*combine[,cohort_size]

    combine[, "inc.cecx"] <- combine[,inc.cecx]*combine[,cohort_size]
    colnames(combine)[colnames(combine) == "inc.cecx"] <- "cases"
    combine[, "mort.cecx"] <- combine[,mort.cecx]*combine[,cohort_size]
    colnames(combine)[colnames(combine) == "mort.cecx"] <- "deaths"

    combine[, "lifey"]      <- combine[,lifey]*combine[,cohort_size]
    combine[, "disability"] <- combine[,disability]*combine[,cohort_size]

    combine[, "cost.cecx"] <- combine[,cost.cecx]*combine[,cohort_size]
    colnames(combine)[colnames(combine) == "cost.cecx"] <- "costs"
  }

  return(combine)

} # end of function -- BatchRun


#' Formatting output for VIMC Montagu
#'
#' \code{OutputVimc} takes result of BatchRun and outputs it in format to be
#'   uploaded to VIMC Montagu.
#'
#' @param DT data table with results
#' @param age_stratified logical, whether output should be stratified by age
#' @param calendar_year logical, whether output should be given by calendar year
#'        of event OR by year of birth of cohort
#' @param vimc_template data table with template file downloaded from montagu
#'
#' @return #
#' @export
#'
#' @examples #
OutputVimc <- function (DT,
                        age_stratified = TRUE,
                        calendar_year  = FALSE,
                        vimc_template  = -1) {

  #check if data by calendar_year
  if ("year" %in% colnames(DT)) {
    is_by_calendar_year <- TRUE
  } else {
    is_by_calendar_year <- FALSE
    colnames(DT)[which(colnames(DT) == "birthcohort")] <- "year"
  }

  #check if values are proportions
  if (!("cases" %in% colnames(DT))) {
    DT [, "inc.cecx"]   <- DT [, cohort_size] * DT [, inc.cecx]
    DT [, "mort.cecx"]  <- DT [, cohort_size] * DT [, mort.cecx]
    DT [, "disability"] <- DT [, cohort_size] * DT [, disability] +
      DT [, cohort_size] * DT[, lifey]

    if ("run_id" %in% colnames(DT)) {
      DT <- DT[, c("scenario",
                   "run_id",
                   "country",
                   "year",
                   "age",
                   "cohort_size",
                   "inc.cecx",
                   "mort.cecx",
                   "disability")]

      colnames(DT) <- c("scenario",
                        "run_id",
                        "country",
                        "year",
                        "age",
                        "cohort_size",
                        "cases",
                        "deaths",
                        "dalys")
    } else {
      DT <- DT[, c("scenario",
                   "country",
                   "year",
                   "age",
                   "cohort_size",
                   "inc.cecx",
                   "mort.cecx",
                   "disability")
               ]

      colnames(DT) <- c("scenario",
                        "country",
                        "year",
                        "age",
                        "cohort_size",
                        "cases",
                        "deaths",
                        "dalys")
    }
  } else {
    DT[,"dalys"] <- DT[,lifey] + DT[,disability]
    if ("run_id" %in% colnames(DT)) {
      DT <- DT[, c("scenario",
                   "run_id",
                   "country",
                   "year",
                   "age",
                   "cohort_size",
                   "cases",
                   "deaths",
                   "dalys")]
    } else {
      DT <- DT[, c("scenario",
                   "country",
                   "year",
                   "age",
                   "cohort_size",
                   "cases",
                   "deaths",
                   "dalys")]
    }
  }

  if (is.data.table(vimc_template)) {
    #calendar year is needed to select data in vimc_template
    if (!is_by_calendar_year) {
      DT[,"year"] <- DT[,year] + DT[,age]
    }
    DT <- DT[year %in% vimc_template[,year]]
    for (y in sort(unique(DT[,year]))) {
      DT <- DT[year!=y | (year==y & age %in% vimc_template[year==y,age])]
    }
    for (c in unique(DT[,country])) {
      DT [country == c, "country_name"] <- unique (vimc_template[country == c,
                                                                 country_name])
    }
    DT[,"disease"] <- unique(vimc_template[,"disease"])

    #revert back to impact year
    if (!is_by_calendar_year) {
      DT[,"year"] <- DT[,year] - DT[,age]
    }
    if ("run_id" %in% colnames(DT)) {
      DT <- DT[,c("scenario","run_id",colnames(vimc_template)),with=F]
    } else {
      DT <- DT[,c("scenario",colnames(vimc_template)),with=F]
    }
  }

  #return correct year
  if (calendar_year & !is_by_calendar_year) {
    DT[,"year"] <- DT[,year] + DT[,age]
  } else if (!calendar_year & is_by_calendar_year) {
    DT[,"year"] <- DT[,year] - DT[,age]
  }

  if ("run_id" %in% colnames(DT)) {
    if (!age_stratified) {
      DT <- dtAggregate (DT, "age", c("cohort_size",
                                      "cases",
                                      "deaths",
                                      "dalys",
                                      "run_id"))
    }
    setorder (DT, scenario, run_id, country, year, age)
  } else {
    if (!age_stratified) {
      DT <- dtAggregate(DT, "age", c("cohort_size",
                                     "cases",
                                     "deaths",
                                     "dalys"))
    }
    setorder(DT, scenario, country, year, age)
  }

  return(DT)

} # end of function -- OutputVimc


#' Run PRIME for a single birth-cohort
#'
#' Runs PRIME for one birth-cohort. Usually called by another function such as
#'   RunCountry().
#'
#' @param lifetab Data.table: The life-table for this cohort. Can be created
#'        using the lifeTable() function.
#' @param cohort Number: The cohort-size of this birth-cohort at the time where
#'        the lifetable starts.
#' @param incidence Numeric vector: Age-specific CeCx(16/18) incidence-rates.
#' @param mortality_cecx Numeric vector: Age-specific CeCx(16/18) mortality-rates.
#' @param prevalence Numeric vector: Age-specific CeCx(16/18) prevalence rates
#'        (5-year prevalence) -- referring to people who are alive within
#'        5 years of diagnosis.
#' @param agevac Number: Age at which the cohort is vaccinated.
#' @param coverage Number: Proportion of the cohort that will receive a vaccination.
#' @param campaigns List or number: MAC cohort-vaccinations (needs to be changed).
#' @param vaccine_efficacy_nosexdebut Number: proportion indicating
#'        vaccine-efficacy before sexual debut.
#' @param vaccine_efficacy_sexdebut Number: proportion indicating
#'        vaccine-efficacy after sexual debut.
#' @param cost_cancer Number: total per capita cost of cancer.
#' @param discounting Logical (optional): If TRUE, run cost-effectiveness analysis
#'        undiscounted and discounted. If FALSE, only uses undiscounted
#' @param disc.cost Number (optional): Discounting for health costs
#'        (only if discounting=TRUE)
#' @param disc.ben Number (optional): Discounting for health outcomes
#'        (only if discounting=TRUE)
#'
#' @return Returns a data.table with size of the birth-cohort and age-specific
#'   incidence-rates, mortality-rates, years-of-life-lost, years-of-healthy-life-lost,
#'   and cancer-costs before and after vaccination.
#'   Also displays whether discounting has been used ("type" column).
#'
#' @examples
#' lifetab <- lifeTable(unlist(data.mortall[iso3=="AFG",
#'   as.character(0:100), with=F], use.names=F), 9)
#' cohort <- unlist(data.popproj[iso3=="AFG", "2020"], use.names=F)
#' incidence <- unlist(data.incidence[iso3=="AFG", as.character(0:100), with=F],
#'   use.names=F)
#' mortality_cecx <- unlist(data.mortall[iso3=="AFG", as.character(0:100), with=F],
#'   use.names=F)
#' prevalence <- unlist(data.cecx_5y_prevalence[iso3=="AFG",
#'   as.character(0:100), with=F], use.names=F)
#' agevac <- 9
#' coverage <- 0.8
#' campaigns <- -1
#' vaccine_efficacy_nosexdebut <- 0.95
#' vaccine_efficacy_sexdebut <- 0
#' cost_cancer <- 100
#'
#' RunCohort(lifetab, cohort, incidence, mortality_cecx, prevalence, agevac,
#'   coverage, campaigns, vaccine_efficacy_nosexdebut, vaccine_efficacy_sexdebut,
#'   cost_cancer, disc.cost=0.03, disc.ben=0.03,
#'   discounting=FALSE, country_iso3="AFG", run_country=FALSE)
#'
#' @export
#' @import data.table
#' @import foreach
#'
RunCohort <- function (lifetab,
                       cohort,
                       incidence,
                       mortality_cecx,
                       prevalence,
                       agevac,
                       coverage,
                       campaigns,
                       vaccine_efficacy_nosexdebut,
                       vaccine_efficacy_sexdebut,
                       cost_cancer,
                       discounting        = FALSE,
                       disc.cost          = 0.03,
                       disc.ben           = 0.03,
                       country_iso3       = NULL,
                       run_country        = FALSE,
                       disability.weights = "gbd_2017") {

  #check if required variables are present
  if (sum (!sapply (ls(), function(x) {checkSize(get(x))} ) ) > 0) {
    stop("Not all values have the required length")
  }

  ages <- lifetab [, age]
  lexp <- lifetab [, ex]

  ##############################################################################
  # Calculate weights for DALYs. This calculation of daly.canc.nonfatal and
  # daly.canc.fatal is specific for GBD 2001 disability weights.
  # daly.canc.nonfatal <- daly.canc.diag + daly.canc.seq * 4
  # daly.canc.fatal <- daly.canc.diag + daly.canc.terminal
  #
  ##############################################################################
  #
  # Calculate yld using GBD 2001 disability weights
  if (disability.weights == "gbd_2001") {

    ##############################################################################
    # daly.canc.seq <- switch(
    #   data.global[iso3==country_iso3,`WHO Mortality Stratum`],
    #   "A"=0.04,
    #   "B"=0.11,
    #   "C"=0.13,
    #   "D"=0.17,
    #   "E"=0.17
    # )
    ##############################################################################
    # disability weight for long term sequela based on WHO mortality stratum
    # this is specific for gbd_2001
    stratum       <- data.global [iso3==country_iso3, `WHO Mortality Stratum`]
    daly.canc.seq <- data.disability_weights [Source == "gbd_2001" &
                                                WHO_MortalityStratum == stratum,
                                              Mid]
    ##############################################################################

    # diagnosis, therapy and control over 1 year
    diag <- data.disability_weights [Source == disability.weights &
                                       Sequela == "diagnosis",
                                     Mid]

    # metastatic stage for 6 months and terminal stage for 6 months (for a total of 1 year)
    # note: disability weights for metastatic and terminals stages are equal
    terminal <- data.disability_weights [Source == disability.weights &
                                           Sequela == "terminal",
                                         Mid]

    # long term sequela (for 4 years)
    # note: duration of long-term sequela is same irrespective of
    # WHO mortality startum (A/B/C/D/C)
    control <- daly.canc.seq *
      data.disability_weights [Source == disability.weights &
                                 Sequela == "control" &
                                 WHO_MortalityStratum == "E",
                               Duration]

    # yld associated with a non fatal case & a fatal case
    daly.canc.nonfatal <- diag + control
    daly.canc.fatal    <- diag + terminal

    # combine yld contribution from non-fatal and fatal cases
    yld <- ((incidence - mortality_cecx) * daly.canc.nonfatal) +
      (mortality_cecx * daly.canc.fatal)

    if (discounting) {
      ############################################################################
      # estimate yld discounted (taking into account for morbidity in future years)
      # in gbd 2001, yld is attributed to age of incidence
      # diagnosis, therapy and control over 1 year + long term sequela for next 4 years
      daly.canc.nonfatal.disc <- diag +
        daly.canc.seq * ( 1/(1+disc.ben)   + 1/(1+disc.ben)^2 +
                          1/(1+disc.ben)^3 + 1/(1+disc.ben)^4 )

      # diagnosis, therapy and control in 1st year followed by
      # metastatic stage for 6 months and terminal stage for 6 months (in 2nd year)
      daly.canc.fatal.disc <- diag + terminal * 1/(1+disc.ben)

      yld.disc <- ((incidence - mortality_cecx) * daly.canc.nonfatal.disc) +
        (mortality_cecx * daly.canc.fatal.disc)

      # daly.canc.nonfatal.disc <- daly.canc.diag +
      #   daly.canc.seq * ( 1/(1+disc.ben) +
      #                       1/(1+disc.ben)^2 +
      #                       1/(1+disc.ben)^3 +
      #                       1/(1+disc.ben)^4 )
      #
      # daly.canc.fatal.disc <- daly.canc.diag + daly.canc.terminal * 1/(1+disc.ben)
      ############################################################################
    }

  } else if (disability.weights == "gbd_2017") {

    # calculate yld using GBD 2017 disability weights
    ##########################################################################
    # diability weights for different phases of cervical cancer
    # (diagnosis & therapy, controlled, metastatic, terminal)
    # dw = list (diag       = daly.canc.diag,
    #            control    = daly.canc.control,
    #            metastatic = daly.canc.metastatic,
    #            terminal   = daly.canc.terminal)
    #
    # duration of different phases of cervical cancer
    # (diagnosis & therapy, controlled, metastatic, terminal) -- unit in years
    # cecx_duration = list (diag = 4.8/12, metastatic = 9.21/12, terminal = 1/12)
    # duration of controlled phases is based on remainder of time after attributing to other phases
    #
    ##########################################################################

    # disability weights for different phases of cervical cancer
    # (diagnosis & therapy, controlled, metastatic, terminal)
    dw = list (diag       = data.disability_weights [Source == disability.weights &
                                                       Sequela == "diagnosis",
                                                     Mid],
               control    = data.disability_weights [Source == disability.weights &
                                                       Sequela == "control",
                                                     Mid],
               metastatic = data.disability_weights [Source == disability.weights &
                                                       Sequela == "metastatic",
                                                     Mid],
               terminal   = data.disability_weights [Source == disability.weights &
                                                       Sequela == "terminal",
                                                     Mid])

    # duration of different phases of cervical cancer
    # (diagnosis & therapy, controlled, metastatic, terminal) -- unit in years
    cecx_duration = list (diag       = data.disability_weights [Source == disability.weights &
                                                                  Sequela=="diagnosis",
                                                                Duration],
                          metastatic = data.disability_weights [Source == disability.weights &
                                                                  Sequela=="metastatic",
                                                                Duration],
                          terminal   = data.disability_weights [Source == disability.weights &
                                                                  Sequela=="terminal",
                                                                Duration])
    # duration of controlled phases is based on remainder of time after attributing to other phases

    # combine yld contribution from (incidence, prevalence and mortality) cases
    yld <-
      (incidence  * dw$diag * cecx_duration$diag) +
      (prevalence * dw$control) +
      (mortality_cecx * ( (dw$metastatic * cecx_duration$metastatic) +
                           (dw$terminal * cecx_duration$terminal) ) )

    if (discounting) {
      # estimate yld discounted (taking into account for morbidity in future years)
      # in gbd 2017, since yld is attributed to age of prevalence,
      # yld (discounted) = yld (undiscounted)
      yld.disc <- yld
    }

  }


  ##############################################################################

  # discounting
  if (discounting) {

    # daly.canc.nonfatal.disc <- daly.canc.diag +
    #   daly.canc.seq * ( 1/(1+disc.ben) +
    #                       1/(1+disc.ben)^2 +
    #                       1/(1+disc.ben)^3 +
    #                       1/(1+disc.ben)^4 )
    #
    # daly.canc.fatal.disc <- daly.canc.diag + daly.canc.terminal * 1/(1+disc.ben)

    disc.cost.yr <- rep(0, length(ages))
    disc.ben.yr  <- rep(0, length(ages))

    disc.cost.yr [1:(which(ages == agevac))] <- 1
    disc.ben.yr  [1:(which(ages == agevac))] <- 1

    # age of vaccination is base year for discounting of this cohort
    # estimate cumulative discount rates for each year beyond age of vaccination
    for (a in ages [which (ages > agevac) ]) {
      disc.cost.yr [which (ages == a)] <- 1 / (1 + disc.cost)^(a - agevac)
      disc.ben.yr  [which (ages == a)] <- 1 / (1 + disc.ben)^(a - agevac)
    }


    ##############################################################################
    # estimate remaining life expectancy (discounted)
    # lexp.disc <- rep(0,length(ages))
    #
    # for (a in ages[-which(ages==max(ages))]) {
    #   lexp.disc [which (ages==a)] <- sum(
    #     disc.ben.yr [1:floor (
    #       lexp [which (ages==a)]
    #     )]
    #   ) + (
    #     lexp [which (ages==a)]
    #     -floor(
    #       lexp [which (ages==a)]
    #     )
    #   ) * disc.ben.yr [floor(
    #     lexp [which (ages==a)]
    #   ) + 1]
    # }

  # YLL discounting based on formula = (1 - exp^( -r * L )) / r
  # https://www.who.int/quantifying_ehimpacts/publications/en/9241546204chap3.pdf

  lexp.disc <- (1 - exp (-1 * disc.ben * lexp)) / disc.ben

  }
  ##############################################################################


  coverage <- ageCoverage (ages,
                           coverage,
                           vaccine_efficacy_nosexdebut,
                           vaccine_efficacy_sexdebut,
                           campaigns,
                           lifetab,
                           cohort,
                           agevac,
                           country_iso3 = country_iso3)

  # In out.pre data table, 'lifey' refers to YLL and 'disability' refers to YLD
  # YLL - Years of Life Lost due to premature mortality
  # YLD - Years of Life lost due to Disability

  # expected number of cases, deaths, dalys, and costs (static)
  out.pre <- data.table (
    age         = ages,
    cohort_size = cohort * lifetab[,lx.adj],
    vaccinated  = rep(0,length(ages)),
    immunized   = rep(0,length(ages)),
    inc.cecx    = incidence,
    mort.cecx   = mortality_cecx,
    lifey       = mortality_cecx*lexp,
    # disability  = (incidence - mortality_cecx)*daly.canc.nonfatal + mortality_cecx*daly.canc.fatal,
    # disability  = (incidence  * daly.canc.diag * 4.8/12) + (prevalence * daly.canc.control) + (mortality_cecx * (daly.canc.metastatic * 9.21/12 + daly.canc.terminal * 1/12) ),
    # disability  = (incidence  * dw$diag * cecx_duration$diag) + (prevalence * dw$control) + (mortality_cecx * (dw$metastatic * cecx_duration$metastatic + dw$terminal * cecx_duration$terminal) ),
    disability  = yld,
    cost.cecx   = incidence * cost_cancer
  )

  out.post                   <- out.pre
  out.post                   <- out.post * (1 - coverage [, effective_coverage])
  out.post [, "age"]         <- ages
  out.post [, "cohort_size"] <- cohort * lifetab[,lx.adj]
  out.post [, "vaccinated"]  <- coverage [, coverage]
  out.post [, "immunized"]   <- coverage [, effective_coverage]

  # discounted
  if (discounting) {
    out.pre.disc                  <- out.pre
    out.pre.disc [, "inc.cecx"]   <- out.pre.disc [, inc.cecx]  * disc.ben.yr
    out.pre.disc [, "mort.cecx"]  <- out.pre.disc [, mort.cecx] * disc.ben.yr
    out.pre.disc [, "lifey"]      <- out.pre [,mort.cecx] * lexp.disc * disc.ben.yr

    ############################################################################
    # out.pre.disc [, "disability"] <- (
    #   (out.pre[, inc.cecx] -
    #      out.pre [, mort.cecx]) * daly.canc.nonfatal.disc +
    #      out.pre [, mort.cecx]  * daly.canc.fatal.disc
    # ) * disc.ben.yr

    out.pre.disc [, "disability"] <- yld.disc * disc.ben.yr
    ############################################################################

    out.pre.disc [, "cost.cecx"]    <- out.pre [, cost.cecx] * disc.cost.yr

    out.post.disc                   <- out.pre.disc
    out.post.disc                   <- out.pre.disc * (1-coverage[, effective_coverage])

    out.post.disc [, "age"]         <- ages
    out.post.disc [, "cohort_size"] <- cohort * lifetab[, lx.adj]
    out.post.disc [, "vaccinated"]  <- coverage [, coverage]
    out.post.disc [, "immunized"]   <- coverage [, effective_coverage]

    out.pre.disc  [, "scenario"]    <- "pre-vaccination"
    out.pre.disc  [, "type"]        <- "discounted"
    out.post.disc [, "scenario"]    <- "post-vaccination"
    out.post.disc [, "type"]        <- "discounted"
  }

  out.pre  [, "scenario"] <- "pre-vaccination"
  out.pre  [, "type"]     <- "undiscounted"
  out.post [, "scenario"] <- "post-vaccination"
  out.post [, "type"]     <- "undiscounted"

  # combine results
  if (discounting) {
    out.pre <- rbindlist(
      list(
        out.pre,
        out.pre.disc
      )
    )
    out.post <- rbindlist(
      list(
        out.post,
        out.post.disc
      )
    )
  }

  out <- rbindlist(
    list(
      out.pre,
      out.post
    )
  )

  out <- out [ , c ("scenario",
                    "type",
                    "age",
                    "cohort_size",
                    "vaccinated",
                    "immunized",
                    "inc.cecx",
                    "mort.cecx",
                    "lifey",
                    "disability",
                    "cost.cecx"),
               with = FALSE]

  return (out)

} # end of function -- RunCohort


#' Run PRIME for a specific country
#'
#' Runs RunCohort() using country-specific estimates.
#' If year_born and year_vac are not provided, assumes vaccination occurs in
#'   the current year.
#'
#' @param country_iso3 Character string (required): ISO3 code of the country
#' @param vaceff Number (optional): Proportion indicating vaccine-efficacy
#' @param cov Number (optional): Proportion with routine coverage
#' @param agevac Integer (optional): Target age for HPV vaccination
#' @param agecohort Integer (optional): Reference age for cohort-size
#'        (only used when 'cohort' is not provided)
#' @param cohort Integer (optional): Cohort-size. -1 if unknown
#' @param canc.inc Integer (optional): Reference year for cancer incidence rates
#'        (Globocan: 2018 or 2012)
#' @param sens Numeric-vector (optional): Specific values to be used in a PSA.
#'        -1 if PSA's are not used
#' @param unwpp_mortality Logical (optional): If TRUE, uses year-specific UNWPP
#'        mortality estimates to construct life-tables.
#'        If FALSE, use WHO based mortality estimates.
#' @param year_born Integer (optional): Year in which cohort is born
#' @param year_vac Integer (optional): Year in which cohort is vaccinated
#' @param campaigns List (optional): Multi-Age-Cohort campaigns
#'        (needs to be changed)
#' @param analyseCosts Logical (optional): If FALSE, returns result from
#'        RunCohort() function.
#'        If TRUE, runs analyseCosts() with country-specific results.
#' @param canc.cost Character (optional): Is cost of cancer adjusted
#'        ("adj" for International $) or not ("unadj" for US$)
#' @param discounting Logical (optional): If TRUE, run cost-effectiveness analysis
#'        undiscounted and discounted. If FALSE, only uses undiscounted
#' @param disc.cost Number (optional): Discounting for health costs
#'        (only if discounting=TRUE)
#' @param disc.ben Number (optional): Discounting for health outcomes
#'        (only if discounting=TRUE)
#' @param disability.weights character, disability weights for cervical cancer
#'        from GBD 2017 or GBD 2001
#' @param wb.indicator character, World Bank indicator for GDP/GNI per capita
#'        in I$/US$ and current/constant data
#' @param wb.year numeric, year of the World Bank indicator value
#'
#' @return data.table with country-specific results of HPV vaccination.
#'         Returns cost-analysis if analyseCosts=TRUE
#' @export
#' @importFrom wbstats wb
#'
#' @examples RunCountry("AFG")
#' @examples RunCountry("AFG", year_vac=2020, agevac=10, cov=0.75, vaceff=0.88)
#' @examples RunCountry("AFG", year_vac=2020, agevac=10, cov=0.75, vaceff=0.88,
#'           analyseCosts=TRUE)
RunCountry <- function (country_iso3,
                        vaceff_beforesexdebut = 1,
                        vaceff_aftersexdebut  = 0,
                        cov                   = 1,
                        agevac                = 10,
                        agecohort             = 10,
                        cohort                = -1,
                        canc.inc              = "2018",
                        sens                  = -1,
                        unwpp_mortality       = TRUE,
                        year_born             = -1,
                        year_vac              = -1,
                        campaigns             = -1,
                        analyseCosts          = FALSE,
                        canc.cost             = "unadj",
                        discounting           = FALSE,
                        disc.cost             = 0.03,
                        disc.ben              = 0.03,
                        run_batch             = FALSE,
                        psadat                = -1,
                        disability.weights    = "gbd_2017",
                        wb.indicator          = "NY.GDP.PCAP.PP.CD",
                        wb.year               = 2017) {

  ## check if all required data is present in the global environment
  # if(sum(!(c("data.incidence", "data.global", "data.costcecx", "data.popproj", "data.mortcecx", "data.mortall", "data.mortall.unwpp") %in% ls(name=.GlobalEnv, all.names=T))) > 0){
  #	stop("Not all required datafiles seem to be present in your environment. Please load all datafiles required.")
  # }

  # check if required variables are present (sanity check, sees if any variables
  # passed to function have a length of 0)
  if (sum (!sapply (ls(), function(x) {checkSize(get(x))} )) > 0) {
    stop ("Not all values have the required length")
  }

  # retrieve year of birthcohort
  if (year_vac!=-1 & year_born!=-1 ) {
    # check if year of vaccination corresponds with age of vaccination
    # for this birth-cohort
    if (year_vac-agevac != year_born) {
      stop (
        paste0 ("Year of vaccination (",
                year_vac,
                ") and age of vaccination (",
                agevac,
                ") do not correspond with chosen birthcohort (",
                year_born,
                "). Please change accordingly or omit 'year_born' or 'year_vac'."
        )
      )
    }
  } else if (year_vac==-1 & year_born!=-1) {
    year_vac <- year_born+agevac

  } else if (year_vac!=-1 & year_born==-1) {
    year_born <- year_vac-agevac

  } else if (year_vac==-1 & year_born==-1) {
    #assume vaccination in current year
    year_vac <- as.numeric(format(Sys.time(),format="%Y"))
    year_born <- year_vac-agevac
  }

  ages <- as.numeric (colnames (data.incidence) [!grepl ("\\D",
                                                    colnames (data.incidence))])
  ages <- ages[!is.na(ages)]

  # If no data available, use other country as proxy
  # if ( country_iso3 %in% c("XK","MHL","TUV","PSE") ) {
  if ( country_iso3 %in% c("XK", "PSE") ) {
    proxy <- TRUE
    country_iso3 <- switch (
      country_iso3,
      "XK"  = "ALB",  # demography data available for XK but no burden & cost
      "PSE" = "JOR",  # burden & demography data available for PSE but no cost
      # "MHL" = "KIR",
      # "TUV" = "FJI",
      country_iso3
    )
  } else {
    proxy <- FALSE
  }

  # set country specific variables
  # cost per FVG
  cost.vac <- monetary_to_number (
    data.global [iso3 == country_iso3,
                 `Vaccine price (USD) [4]`]
  ) + monetary_to_number (
    data.global [iso3 == country_iso3,
                 `Vaccine delivery/ operational/ admin costs (USD) [5]`]
  )

  # cost per cancer episode
  if (canc.cost == "unadj") {
    # cost.canc <- monetary_to_number(data.costcecx[iso3==country_iso3, cancer_cost])
    cost.canc <- data.costcecx [iso3==country_iso3, cancer_cost]
  } else if (canc.cost == "adj") {
    # cost.canc <- monetary_to_number(data.costcecx[iso3==country_iso3, cancer_cost_adj])
    cost.canc <- data.costcecx [iso3==country_iso3, cancer_cost_adj]
  }

  # % of CeCx due to 16/18
  p1618 <- data.global [iso3==country_iso3, `% CeCx due to 16/18`] / 100

  # age-dependent parameters
  if (canc.inc == "2018") {
    inc <- unlist (data.incidence [iso3==country_iso3,
                                   as.character(ages),
                                   with=F],
                   use.names=F) * p1618

    mort.cecx <- unlist (data.mortcecx [iso3==country_iso3,
                                        as.character(ages),
                                        with=F],
                         use.names=F) * p1618

    cecx_5y_prev <- unlist (data.cecx_5y_prevalence [iso3==country_iso3,
                                                     as.character(ages),
                                                     with=F],
                            use.names=F) * p1618
  }
  else if (canc.inc == "2012") {
    inc <- unlist (data.incidence2012 [iso3==country_iso3,
                                       as.character(ages),
                                       with=F],
                   use.names=F) * p1618

    mort.cecx    <- unlist (data.mortcecx2012 [iso3==country_iso3,
                                               as.character(ages),
                                               with=F],
                            use.names=F) * p1618

    cecx_5y_prev <- unlist (data.cecx_5y_prevalence [iso3==country_iso3,
                                                     as.character(ages),
                                                     with=F],
                            use.names=F) * p1618

    # 5-year cervical cancer prevalence data for 2012 is not available
    cecx_5y_prev <- cecx_5y_prev * 0
  }
  else if (canc.inc == "2008") {
    #inc=as.numeric(data.incidence08[c,2:(maxage+2)])*p1618
    #mort.cecx=as.numeric(data.mortcecx08[c,2:(maxage+2)])*p1618
  }

  # set incidence and mortality and prevalence values to 0 if they are missing
  inc          [which (is.na (inc)          )] <- 0
  mort.cecx    [which (is.na (mort.cecx)    )] <- 0
  cecx_5y_prev [which (is.na (cecx_5y_prev) )] <- 0

  ##############################################################################
  # daly.canc.seq <- switch(
  #   data.global[iso3==country_iso3,`WHO Mortality Stratum`],
  #   "A"=0.04,
  #   "B"=0.11,
  #   "C"=0.13,
  #   "D"=0.17,
  #   "E"=0.17
  # )
  ##############################################################################
  # disability weight for long term sequela based on WHO mortality stratum
  # this is specific for gbd_2001
  # stratum       <- data.global [iso3==country_iso3, `WHO Mortality Stratum`]
  # daly.canc.seq <- data.disability_weights [Source=="gbd_2001" &
  #                                             WHO_MortalityStratum==stratum, Mid]
  ##############################################################################

  # Calculate total and vaccinated cohort size
  # If UN population projections unavailable, cohort size = 1 otherwise
  # cohort size = number of 10-14y/5
  if (cohort==-1) {
    cohort <- unlist (data.popproj [iso3 == country_iso3,
                                    as.character(year_born + agecohort),
                                    with=F],
                      use.names=F)/5

    if (length(cohort)==0) {
      cohort <- 1
    } else if (proxy) {
      cohort <- switch (
        country_iso3,
        "ALB" = cohort * 1824000/2774000,
        # "KIR" = cohort * 52634/102351,
        # "FJI" = cohort * 9876/881065,
        "JOR" = cohort * 4170000/6459000,
        cohort
      )
    }
  }

  # create lifetables
  if (!unwpp_mortality) {
    # Use WHO mortality estimates
    mort.all <- unlist (data.mortall[iso3==country_iso3,
                                     as.character(ages),
                                     with=F],
                        use.names=F)
    lifetab  <- lifeTable (qx=mort.all,agecohort)
  } else {

    # Use UNWPP mortality estimates
    # if year is outside of scope of mortality estimates, use last available data
    mx <- numeric(length(ages))

    for (a in ages) {
      if (year_born+a > max(data.mortall.unwpp.mx[country_code==country_iso3,year])) {
        lookup.yr <- max(data.mortall.unwpp.mx[country_code==country_iso3,year])
      } else if (year_born+a < min(data.mortall.unwpp.mx[country_code==country_iso3,year])) {
        lookup.yr <- min(data.mortall.unwpp.mx[country_code==country_iso3,year])
      } else {
        lookup.yr <- year_born + a
      }

      mortality <- data.mortall.unwpp.mx [(country_code == country_iso3) &
                                            (age_from   <= a) &
                                            (age_to     >= a) &
                                            (year - (lookup.yr) <  1) &
                                            (year - (lookup.yr) > -5),
                                          value]

      # set mortality to 1 if no data is found
      if (length(mortality) < 1) {
        mortality <- 1
      }
      mx[which(ages==a)] <- mortality
    }

    lifetab <- lifeTable (mx=mx, agecohort=agecohort)
  }

  ## UPDATE: To be updated -- PSA for cecx_5y_prev

  if (is.list(psadat)) {
    #apply psa multipliers for incidence and mortality
    if ("incidence" %in% names(psadat)) {
      inc <- inc * psadat[["incidence"]]
    }
    if ("mortality" %in% names(psadat)) {
      mort.cecx <- mort.cecx * psadat[["mortality"]]
    }
    if ("vaccine_efficacy" %in% names(psadat)) {
      vaceff <- vaceff * psadat[["vaccine_efficacy"]]
    }
    if ("vaccine_cost" %in% names(psadat)) {
      cost.vac <- cost.vac * psadat[["vaccine_cost"]]
    }
    if ("cancer_cost" %in% names(psadat)) {
      cost.canc <- cost.canc * psadat[["cancer_cost"]]
    }
    if ("discounting_cost" %in% names(psadat)) {
      disc.cost <- disc.cost * psadat[["discounting_cost"]]
    }
    if ("discounting_ben" %in% names(psadat)) {
      disc.ben <- disc.ben * psadat[["discounting_ben"]]
    }
  }

  # calling RunCohort function
  result_cohort <- RunCohort (
    lifetab                     = lifetab,
    cohort                      = cohort,
    incidence                   = inc,
    mortality_cecx              = mort.cecx,
    prevalence                  = cecx_5y_prev,
    agevac                      = agevac,
    coverage                    = cov,
    campaigns                   = campaigns,
    vaccine_efficacy_nosexdebut = vaceff_beforesexdebut,
    vaccine_efficacy_sexdebut   = vaceff_aftersexdebut,
    cost_cancer                 = cost.canc,
    discounting                 = discounting,
    disc.cost                   = disc.cost,
    disc.ben                    = disc.ben,
    country_iso3                = country_iso3,
    run_country                 = TRUE,
    disability.weights          = disability.weights
  )

  if (analyseCosts) {
    # gdp_per_capita <- monetary_to_number (
    #   data.global [iso3==country_iso3, `GDP per capita (2011 i$) [7]`] )

    # Note: Refer to World Bank indicators at https://data.worldbank.org/indicator
    # For example, GDP per capita, PPP (current international $) - 2017
    # https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD

    # GDP/GNI per capita
    # wb.indicator: World Bank indicator for GDP/GNI per capita in I$/US$ and current/constant data
    # wb.year: year of the World Bank indicator value
    gdp_per_capita <- wb (country   = country_iso3,
                          indicator = wb.indicator,
                          startdate = wb.year,
                          enddate   = wb.year)$value

    return (analyseCosts (result_cohort, cost.vac, gdp_per_capita))
  }
  else {
    return (result_cohort)
  }

} # end of function -- RunCountry


#' Retrieve ISO3-code of country
#'
#' @param countryname Character string (required): Full name of the country
#' @param name Logical (optional): If TRUE, returns full name and alternative
#'        names of returned country (may be useful to double-check that it is
#'        the correct country)
#'
#' @return Character string with ISO3 code. Will also return full name
#'         if name=TRUE.
#' @export
#'
#' @examples
#' getISO3("Afghanistan")
#' getISO3("Congo",name=TRUE)
getISO3 <- function (countryname,
                     name = FALSE) {

  countryname <- data.table (country=countryname)
  if (name) {
    country_iso3 <- dtColMatch (countryname,
                                c("country"),
                                data.countryname,
                                c("name1", "name2", "name3", "name4"),
                                "iso3")

    name     <- data.countryname [iso3==country_iso3, name1]

    name_alt <- unique (unlist (data.countryname [iso3==country_iso3,
                                                  c("name2", "name3", "name4"),
                                                  with=FALSE],
                                use.names=FALSE))

    name_alt <- name_alt[!(name_alt %in% c(""," "))]

    if (length(name_alt) > 0) {
      name <- paste0(
        name," (",
        paste0(name_alt,collapse="; "),
        ")"
      )
    }

    return ( paste0(name, ": ", country_iso3) )
  } else {

    return (dtColMatch (countryname,
                        c("country"),
                        data.countryname,
                        c("name1", "name2", "name3", "name4"),
                        "iso3")
            )
  }

} # end of function -- getISO3


#' Returns cost-effectiveness for a single birthcohort in a single country
#'
#' Usually called using RunCountry(..., analyseCosts=TRUE)
#'
#' @param results Data.table (required): results from RunCohort()
#' @param vaccine_cost Number (required): cost of a single vaccine
#' @param gdp_per_capita Number (required): GDP per capita
#'
#' @return Data.table with cost-analysis
#' @export
#'
#' @examples analyseCosts(RunCountry("AFG"), 100, 561)
analyseCosts <- function (results,
                          vaccine_cost,
                          gdp_per_capita) {

  #check if required variables are present
  if (sum (!sapply(ls(), function(x) {checkSize(get(x))})) > 0) {
    stop ("Not all values have the required length")
  }

  results [, "vaccinated"] <- results [, vaccinated] * results [, cohort_size]
  results [, "immunized"]  <- results [, immunized]  * results [, cohort_size]
  results [, "inc.cecx"]   <- results [, inc.cecx]   * results [, cohort_size]
  results [, "mort.cecx"]  <- results [, mort.cecx]  * results [, cohort_size]
  results [, "lifey"]      <- results [, lifey]      * results [, cohort_size]
  results [, "disability"] <- results [, disability] * results [, cohort_size]
  results [, "cost.cecx"]  <- results [, cost.cecx]  * results [, cohort_size]

  costvariables <- c ("Cohort size",
                      "Vac cohort size",
                      "Vaccine cost",
                      "Costs saved",
                      "Net cost",
                      "CeCx prevented",
                      "Deaths prevented",
                      "Life years saved",
                      "Nonfatal DALYs prevented",
                      "Cost/death prevented",
                      "Cost/life year saved",
                      "Cost/DALY prevented",
                      "GDP/capita",
                      "CE at 1xGDP/capita?",
                      "CE at 3xGDP/capita?",
                      "Cut-off price"
  )

  costeffect <- data.table (variable = costvariables)

  types <- unique (results[, type])

  for (d in types) {
    costeffect [,d] <- numeric(length(costvariables))
  }

  # get agevac
  vaccinated <- 0
  a          <- -1

  while (vaccinated==0) {
    a <- a+1
    vaccinated <- results [scenario == "post-vaccination" &
                             type   == "undiscounted" &
                             age    == a,
                           vaccinated]
  }
  agevac <- a

  cohort_size <- 0
  a           <- -1

  while (cohort_size==0) {
    a <- a+1
    cohort_size <- results [scenario == "post-vaccination" &
                              type   == "undiscounted" &
                              age    == a,
                            cohort_size]
  }
  agecohort <- a

  aggregated <- dtAggregate (results,
                             "age",
                             id.vars = c("scenario", "type") )

  difference <- aggregated [scenario == "pre-vaccination"]

  difference [, "scenario"] <- "difference"

  difference [, "cohort_size"] <-
    abs (aggregated [scenario=="pre-vaccination", cohort_size] -
           aggregated [scenario=="post-vaccination", cohort_size])

  difference [, "vaccinated"] <-
    abs (aggregated [scenario=="pre-vaccination", vaccinated] -
           aggregated [scenario=="post-vaccination", vaccinated])

  difference [, "immunized"] <-
    abs (aggregated [scenario=="pre-vaccination", immunized] -
           aggregated [scenario=="post-vaccination", immunized])

  difference [, "inc.cecx"] <-
    aggregated [scenario=="pre-vaccination",  inc.cecx] -
    aggregated [scenario=="post-vaccination", inc.cecx]

  difference [, "mort.cecx"] <-
    aggregated [scenario=="pre-vaccination",  mort.cecx] -
    aggregated [scenario=="post-vaccination", mort.cecx]

  difference [, "lifey"] <-
    aggregated [scenario=="pre-vaccination",  lifey] -
    aggregated [scenario=="post-vaccination", lifey]

  difference [, "disability"] <-
    aggregated [scenario=="pre-vaccination",  disability] -
    aggregated [scenario=="post-vaccination", disability]

  difference [, "cost.cecx"] <-
    aggregated [scenario=="pre-vaccination",  cost.cecx] -
    aggregated [scenario=="post-vaccination", cost.cecx]

  for (d in types) {

    costeffect [variable=="Cohort size", d] <- cohort_size

    costeffect [variable == "Vac cohort size", d] <-
      results [age        == agevac &
                 scenario == "post-vaccination" &
                 type     == d,
              vaccinated]

    costeffect [variable == "Vaccine cost", d] <-
      unlist (costeffect [variable == "Vac cohort size",
                          d,
                          with = FALSE],
              use.names=FALSE) * vaccine_cost

    costeffect [variable == "Costs saved", d] <-
      difference [type == d, cost.cecx]

    costeffect [variable == "Net cost", d] <-
      unlist (costeffect [variable == "Vaccine cost",
                          d,
                          with = FALSE],
              use.names = FALSE) -
      unlist (costeffect [variable == "Costs saved",
                          d,
                          with = FALSE],
              use.names = FALSE)

    costeffect [variable == "CeCx prevented", d] <-
      difference [type == d, inc.cecx]

    costeffect [variable == "Deaths prevented", d] <-
      difference [type == d, mort.cecx]

    costeffect [variable == "Life years saved", d] <-
      difference [type == d, lifey]

    costeffect [variable == "Nonfatal DALYs prevented", d] <-
      difference [type == d, disability]

    costeffect [variable == "Cost/death prevented", d] <-
      unlist (costeffect [variable == "Net cost",
                          d,
                          with = FALSE],
              use.names = FALSE) /
      unlist (costeffect [variable == "Deaths prevented",
                          d,
                          with = FALSE],
              use.names = FALSE)

    costeffect [variable == "Cost/life year saved", d] <-
      unlist (costeffect [variable == "Net cost",
                          d,
                          with = FALSE],
              use.names = FALSE) /
      unlist (costeffect [variable == "Life years saved",
                          d,
                          with = FALSE],
              use.names = FALSE)

    costeffect [variable == "Cost/DALY prevented", d] <-
      unlist (costeffect [variable == "Net cost",
                          d,
                          with = FALSE],
              use.names = FALSE) /
      (unlist (costeffect [variable == "Life years saved",
                           d,
                           with = FALSE],
               use.names = FALSE) +
         unlist (costeffect [variable == "Nonfatal DALYs prevented",
                             d,
                             with = FALSE],
                 use.names = FALSE) )

    costeffect [variable == "GDP/capita", d] <- gdp_per_capita

    costeffect [variable == "CE at 1xGDP/capita?", d] <-
      as.numeric (unlist (costeffect [variable == "Cost/DALY prevented",
                                      d,
                                      with = FALSE],
                          use.names = FALSE) <
                    gdp_per_capita)

    costeffect [variable == "CE at 3xGDP/capita?", d] <-
      as.numeric (unlist (costeffect [variable == "Cost/DALY prevented",
                                      d,
                                      with = FALSE],
                          use.names = FALSE) <
                    (gdp_per_capita * 3) )

    costeffect [variable == "Cut-off price", d] <-
      (unlist (costeffect [variable == "Costs saved",
                           d,
                           with = FALSE],
               use.names = FALSE) +
         (unlist (costeffect [variable == "Life years saved",
                              d,
                              with = FALSE],
                  use.names = FALSE) +
            unlist (costeffect [variable == "Nonfatal DALYs prevented",
                                d,
                                with = FALSE],
                    use.names = FALSE) ) *
         gdp_per_capita) /
      unlist (costeffect [variable == "Vac cohort size",
                          d,
                          with = FALSE],
              use.names = FALSE)

    costeffect [, d] <- round (unlist (costeffect [, d, with=FALSE],
                                       use.names = FALSE), 2)
  }

  return (costeffect)

} # end of function -- analyseCosts


#' Checks whether the size of a variable is larger than 0
#'
#' Used to determine that all required variables are passed to a function
#' Checks whether a vector has length > 0 or a data.table/data.frame has nrow > 0
#'
#' @param v Variable (required)
#'
#' @return Logical: TRUE if size is not 0, false if size is 0
#' @export
#'
#' @examples
#' x <- c()
#' checkSize(x)
#'
#' x <- c(2,5)
#' checkSize(x)
#'
#' A <- c()
#' B <- c(1,2,3)
#' sapply(c("A","B"),function(x){checkSize(get(x))})
checkSize <- function (v) {

  if (is.vector(v)) {
    size <- length(v)
  } else if (is.data.frame(v) | is.data.table(v)) {
    size <- nrow(v)
  } else {
    size <- 0
  }

  if (size > 0) {
    return (TRUE)
  } else {
    return (FALSE)
  }

} # end of function -- checkSize


#' Construct lifetable based on qx-column
#'
#' qx = age-specific probability of dying
#'
#' @param qx Numeric vector (required): Age-specific probabilities of dying
#' @param agecohort Number (optional): Age at which cohort is started
#'
#' @return Data.table with lifetable
#' @export
#'
#' @examples
#'
#' qx <- unlist(data.mortall[iso3=="AFG", as.character(0:100), with=F], use.names=F)
#' lifeTable(qx, 9)
lifeTable <- function (qx        = NULL,
                       mx        = NULL,
                       agecohort = 0) {

  if (is.null(qx) & is.null(mx)) {
    stop ("Provide qx or mx values")
  } else if (is.null(qx)) {
    # convert central mortality rate to qx
    qx <- 2*mx/(2+mx)
  }

  ages <- c(0:(length(qx)-1))

  lifetab <- data.table (
    age    = ages,
    qx     = qx,
    px     = 1 - qx,
    lx     = rep (0, length(ages)),
    lx.adj = rep (0, length(ages)),
    llx    = rep (0, length(ages)),
    ttx    = rep (0, length(ages)),
    ex     = rep (0, length(ages))
  )

  # proportion of people that have survived up until this year
  lifetab [age==0, "lx"] <- 1
  lifetab [age>0,  "lx"] <- sapply (ages [-which(ages==0)],
                                    function (a) {prod (lifetab [age<a, px])} )

  # ((proportion of people that has survived up until this year) +
  # (proportion of people that will survive this year)) / 2
  lifetab [age <  max(ages), "llx"] <- sapply (ages [-which (ages == max(ages))],
                                               function (a) {
                                                 mean (lifetab [age %in% c(a,a+1), lx])
                                                 }
                                               )
  lifetab [age == max(ages), "llx"] <- lifetab [age == max(ages), lx] / 2

  # ttx is summed proportion that survives - llx (years of life left)
  lifetab [age==0, "ttx"] <- sum (lifetab [, llx])
  lifetab [age>0,  "ttx"] <- sapply (ages [which (ages>0)],
                                     function (a) {
                                       lifetab [age==0, ttx] -
                                         sum (lifetab [age<a, llx])
                                       }
                                     )

  # if any ttx is extremely small (as a result of very small fractions and
  # floating-point errors) set to zero
  lifetab[ttx < 1e-13,"ttx"] <- 0

  # ttx/lx = (years of life left)
  lifetab [, "ex"]                 <- lifetab [, ttx] / lifetab [, lx]
  lifetab [age == max(ages), "ex"] <- 0

  # Now create a new lx which starts at age agecohort for use in calculating
  # impact of vaccinating people at age agecohort
  lifetab [age >= agecohort, "lx.adj"] <- lifetab [age >= agecohort, lx] /
                                          lifetab [age == agecohort, lx]
  lifetab [age < agecohort,  "lx.adj"] <- 0

  return(lifetab)

} # end of function -- lifeTable


#' Get age-specific coverage-rates
#'
#' @param ages Numeric vector (required): ages in model
#' @param routine_coverage Number (required): proportion of population that
#'        receives routine vaccination
#' @param vaccine_efficacy Number (required): proportion indicating
#'        vaccine-efficacy
#' @param campaigns List or number (required): if a list, applies MAC
#'        vaccination (needs to change)
#' @param lifetab Data.table (required): lifetable generated with lifeTable()
#' @param cohort Number (required): cohort-size (only used in MAC campaigns)
#' @param agevac Number (required): target age for vaccination
#'
#' @return Data.table with coverage and effective coverage by age. Used in RunCohort()
#' @export
#'
#' @examples
#' ages <- c(0:100)
#' routine_coverage <- 0.75
#' vaccine_efficacy <- 0.8
#' lifetab <- lifeTable(unlist(data.mortall[iso3=="AFG", as.character(0:100),
#'   with=F], use.names=F), 9)
#' cohort <- unlist(data.popproj[iso3=="AFG", "2020"], use.names=F)
#' agevac <- 9
#' ageCoverage (ages, routine_coverage, vaccine_efficacy, -1,
#'   lifetab, cohort, agevac)
ageCoverage <- function (ages,
                         routine_coverage,
                         vaccine_efficacy_nosexdebut,
                         vaccine_efficacy_sexdebut,
                         campaigns,
                         lifetab,
                         cohort,
                         agevac,
                         country_iso3 = NULL) {

  coverage <- data.table (
    age      = ages,
    coverage = rep (0, length(ages))
  )

  coverage[age >= agevac,"coverage"] <- routine_coverage
  coverage[,"effective_coverage"] <-
    (coverage [, coverage] * vaccine_efficacy_nosexdebut *
       (1 - propSexDebut (agevac, country_iso3))) +
    (coverage [, coverage] * vaccine_efficacy_sexdebut *
       (propSexDebut(agevac, country_iso3)))

  if (is.list(campaigns)) {
    for (y in 1:length(campaigns)) {
      campaign_age      <- campaigns[[y]][["ages"]]
      campaign_coverage <- campaigns[[y]][["coverage"]]
      # if activity type is campaign, coverage proportion still needs to be calculated
      # if(campaigns[[y]][["type"]] == "campaign"){
      #	campaign_coverage <- campaign_coverage/(lifetab[age==campaign_age,"lx.adj"]*cohort)
      #}

      if (campaign_coverage > 1) {
        campaign_coverage <- 1
      }

      # coverage increases for all subsequent age-strata
      init_cov <- coverage[age >= campaign_age, coverage]
      coverage[age >= campaign_age, "coverage"] <- init_cov +
        (1-init_cov) * campaign_coverage

      # vaccine not efficacious for girls that have sexually debuted
      coverage [age >= campaign_age, "effective_coverage"] <-
        (coverage [age >= campaign_age, effective_coverage]) +
        ((1 - init_cov) *
           campaign_coverage *
           (1 - propSexDebut (campaign_age, country_iso3)) *
           vaccine_efficacy_nosexdebut) +
        ((1 - init_cov) *
           campaign_coverage *
           (propSexDebut (campaign_age, country_iso3)) *
           vaccine_efficacy_sexdebut)
    }
  }

  return (coverage)

} # end of function -- ageCoverage


#' Proportion of girls sexually debuted
#'
#' \code{propSexDebut} returns proportion of girls sexually debuted in
#'   country \code{country_iso3} at age \code{age}.
#'
#' @param age age of girls
#' @param country_iso3 ISO3 country code
#'
#' @return Returns proportion of girls in a given country that has
#'   sexually debuted at a given age.
#'
#' @examples
#' propSexDebut (20, "IND")
#' propSexDebut (30, "ETH")
#'
#' @export
propSexDebut <- function (age,
                          country_iso3) {

  if (age < 12) {
    prop_sexdebut <- 0
  } else {
    # calculate proportion of girls that have sexually debuted at age a + 1
    if (nrow (data.sexual_debut  [iso3 == country_iso3]) != 1 ||
        is.na (data.sexual_debut [iso3 == country_iso3, a]) ||
        is.na (data.sexual_debut [iso3 == country_iso3, b]) ||
        is.na (data.sexual_debut [iso3 == country_iso3, cluster.id]) ) {

      # cannot estimate data.sexual_debut, use prop_sexdebut of 0
      prop_sexdebut <- 0

    } else if (!is.na (data.sexual_debut [iso3 == country_iso3, a]) &
               !is.na (data.sexual_debut [iso3 == country_iso3, b]) ) {

      # estimate proportion sexual debut with country specific parameters
      prop_sexdebut <- pgamma (
        # prop will be 0 for ages lower than 12
        (age + 1 - 12),
        shape = data.sexual_debut [iso3 == country_iso3, a],
        scale = data.sexual_debut [iso3 == country_iso3, b]
      )
    } else {

      # estimate proportion sexual debut at 1+age-of-vaccination,
      # based on parameters of country with highest proportion of girls
      # sexually debuting at age 15
      cluster_id  <- data.sexual_debut [iso3 == country_iso3, cluster.id]
      cluster_max <- data.sexual_debut [cluster.id == cluster_id &
                                          X15 == data.sexual_debut [
                                            cluster.id == cluster_id,
                                            max (X15)],
                                        iso3]
      prop_sexdebut <- pgamma (
        #prop will be 0 for ages lower than 12
        (age + 1 - 12),
        shape = data.sexual_debut [iso3 == cluster_max, a],
        scale = data.sexual_debut [iso3 == cluster_max, b]
      )
    }
  }

  return (prop_sexdebut)

} # end of function -- propSexDebut


#' Match two data-tables on multiple columns
#'
#' Returns vector with column-of-interest where columns match
#'
#' If at least one value in any of the input_match_on columns matches with a
#'   value in any of the reference_match_on columns, the two rows will match
#'
#' @param input Data.table (required): input-table to match
#' @param input_match_on Character vector (required): column-names in
#'        input-table to match
#' @param reference Data.table (required): reference-table to match
#' @param reference_match_on Character vector (required): column-names in
#'        reference-table to match
#' @param reference_return Character string (required): column-name in
#'        reference-table that is returned (where values match)
#'
#' @return Character vector with values from reference_return column in
#'         reference_match_on data.table where values match
#' @export
#'
#' @examples
#' dtColMatch (data.global, c("Country"), data.countryname,
#'   c("name1", "name2", "name3", "name4"), "iso3")
#'
# Extend data.table library
# Used to match multiple columns of different data-tables.
# Return variable of interest.
dtColMatch <- function (input,
                        input_match_on,
                        reference,
                        reference_match_on,
                        reference_return) {

  rows <- rep (NA, nrow(input))

  for (imatch in input_match_on) {
    for (rmatch in reference_match_on) {
      rows [is.na(rows)] <- pmatch (
        tolower (
          unlist (input [which(is.na(rows)), imatch, with=F], use.names=F)
        ),
        tolower (
          unlist (reference [, rmatch, with=F], use.names=F)
        )
      )
    }
  }

  return (unlist (data.countryname [rows,
                                    reference_return,
                                    with = FALSE],
                  use.names = FALSE))

} # end of function -- dtColMatch


#' Collapse data-tables
#'
#' @param DT Data-table (required)
#' @param aggr_on Character string (required): column-name that will be used to
#'        collapse on (i.e. combine all age-strata)
#' @param measure.vars Character string (optional): column-names that will be
#'        collapsed (function will be applied to all these columns)
#' @param id.vars Character string (optional): column-names that will remain
#'        stratified
#'
#' N.b. if measure.vars is not provided, all columns that are not in id.vars
#'   and aggr_on will be assumed to be assumed
#'
#' @param func Character string (optional): function that will be applied to
#'        data (if optional, values will be summed)
#' @param na.rm Logical (optional): if TRUE, removes NA from measure.vars
#'        columns before applying function (or passes na.rm=TRUE to function)
#'
#' @return Returns collapsed data.table
#' @export
#'
#' @examples dtAggregate (data.popproj, "iso3", id.vars="")
dtAggregate <- function (DT,
                         aggr_on,
                         measure.vars = c(),
                         id.vars      = c(),
                         func         = "sum",
                         na.rm        = TRUE) {

  # private function
  dtAggregateSingle <- function (DT,
                                 aggr_on,
                                 measure.vars,
                                 id.vars,func="sum") {
    return (switch (
      func,
      "sum" = DT[
        ,
        .(
          list (
            unique (
              get (aggr_on)
            )
          ),
          sum (
            get (measure.vars),
            na.rm = na.rm
          )
        ),
        by = eval(id.vars)
        ],
      DT
    ))
  }

  available_funcs <- c("sum")

  if (!(func %in% available_funcs)) {
    stop (
      paste0(
        "Not possible to aggregate using function '",
        func,
        "'."
      )
    )
  }

  # Use all other columns if measure or id vars are not provided
  if (length(measure.vars) == 0 & length(id.vars) == 0) {
    stop (
      "Please provide measure.vars and/or id.vars"
      )
  } else if (length(measure.vars) == 0) {
    measure.vars <- colnames(DT) [!(colnames(DT) %in% c(id.vars, aggr_on))]
  } else if (length(id.vars) == 0) {
    id.vars <- colnames(DT) [!(colnames(DT) %in% c(measure.vars, aggr_on))]
  }

  c <- 1
  dt_main <- dtAggregateSingle (DT,
                                aggr_on,
                                measure.vars[c],
                                id.vars,func)
  class (dt_main$V1) <- "character"
  dt_main [, "V1"]   <- "aggregated"

  colnames (dt_main) [colnames(dt_main) == "V1"] <- aggr_on
  colnames (dt_main) [colnames(dt_main) == "V2"] <- measure.vars[c]

  for (c in 1:length(measure.vars)) {
    dt_current <- dtAggregateSingle (DT,
                                     aggr_on,
                                     measure.vars[c],
                                     id.vars,
                                     func)
    dt_main [, measure.vars[c]] <- dt_current[, V2]
  }

  return (dt_main)

} # end of function -- dtAggregate


#' Convert monetary character-strings to numeric values
#'
#' @param x Character string to convert
#'
#' @return Returns number with value, stripped from any currency symbols and
#'   thousand-seperators (i.e. "B#2,010.50" becomes 2010.5)
#' @export
#'
#' @examples
#'
#' monetary_to_number ("$2,200.20")
#'
#' # Note that values using German or Dutch notation (i.e. using a comma to
#'   separate decimals and a dot to seperate thousands) are converted as well.
#'   monetary_to_number ("$2.200,20")
#'
monetary_to_number <- function (x) {

  if (!is.character (x)) {

    # return value since incoming value is not a character-string
    return (x)

  } else {

    # remove any valuta_signs
    valuta <- c ("$", "B#", "B%", "b,")

    for (v in valuta) {
      x <- gsub (paste0 ("\\",v), "", x)
    }

    # check what the decimal sign is
    dot   <- sum (strsplit (x,"")[[1]] == ".")
    comma <- sum (strsplit (x,"")[[1]] == ",")

    if (dot>1 & comma>1) {
      stop ("Value has multiple comma's and dots")

    } else if (comma>1 & dot<=1) {

      # assume that comma is used to separate thousands
      # (i.e. English notation)
      x <- gsub (",", "", x, fixed=T)

    } else if (dot>1 & comma<=1) {

      # assume that dot is used to separate thousands
      # (i.e. Dutch or German notation)
      x <- gsub (".", "",  x, fixed=T)
      x <- gsub (",", ".", x, fixed=T)

    } else if (comma==1 & dot==1) {

      # check whether comma or dot comes first
      chars <- strsplit (x, "")[[1]]
      i     <- 0

      while (i < length (chars)) {

        i <- i+1
        if (chars[i] %in% c (".", ",")) {

          thousand_sep <- chars [i]
          i            <- length (chars)
        }
      }

      if (thousand_sep == ",") {

        # comma is used to separate thousands
        x <- gsub (",", "", x, fixed=T)

      } else {

        # dot is used to separate thousands
        x <- gsub (".", "",  x, fixed=T)
        x <- gsub (",", ".", x, fixed=T)
      }

    } else {

      # assume that comma is used to separate thousands
      # (i.e. English notation)
      x <- gsub (",", "", x, fixed=T)
    }

    # fix to keep decimalvalues with big numbers
    x <- as.numeric (x)

    # return monetary value in numeric format
    return (x)
  }

} # end of function -- monetary_to_number
