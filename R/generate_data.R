#' @importFrom stats median qbinom quantile rbinom rcauchy rexp rgamma rlnorm rnorm rt rweibull t.test var wilcox.test
#' @importFrom utils capture.output

#' @export
generate_data <- function(
    n = 50, # default sample size; if not specified it might be overridden by a sample size from the context
    distribution = "debug", # if context is specified, this is not used
    context = NULL, # specify an option (see code for possible values) to generate values based on a real dataset
    adjust_parameter1 = NULL, # used with a context, an adjustment (via adding) to the 1st parameter value (useful for power simulation studies)
    adjust_parameter2 = NULL, # used with a context, an adjustment (via adding) to the 2nd parameter value (only used if appropriate for the distribution; useful for power simulation studies)
    verbose = FALSE, # if TRUE, report the distribution and parameter values used via print(); only used when context is specified
    DEBUG = FALSE,
    dump_stats = FALSE,
    ...
){

  # Contexts: specify distributions -----------------------------------------

  # if a specific context is chosen, override the distribution argument
  # match the context based on regex
  if (!is.null(context)){
    if (stringr::str_detect(context, "^pollution\\d{1,2}$")){
      distribution <- "lognormal"
    }
    else if(stringr::str_detect(context, "^aerosol\\d{1,2}$")){
      distribution <- "lognormal"
    }
    else if(stringr::str_detect(context, "^contactresistance\\d{1,2}$")){
      distribution <- "cauchy"
      # If the user doesn't specify n, override it to match the paper
      if (!("n" %in% names(as.list(match.call())))){
        print("Setting n to 35 to match original context")
        n <- 35
      }
    }
    else if(stringr::str_detect(context, "^watervelocity\\d{1,2}$")){
      distribution <- "cauchy"
    }
    else if (context == "radioactivedecay"){
      distribution <- "exponential"
    }
    # HARD CODED - fix
    else if (context %in% c("derriford_interrarival_AEC1", "derriford_interrarival_GPED1", "derriford_interrarival_GPED2")){
      distribution <- "exponential"
    }
    else if (context == "swedishinfants"){
      distribution <- "normal"
    }
    else if (stringr::str_detect(context, "^earthquakes_*")){
      distribution <- "exponential"
    }
    else if (stringr::str_detect(context, "^derriford_interrarival_ward\\d{1,2}$")){
      distribution <- "weibull"
    }
  }
  if (DEBUG){
    print(distribution)
    print(context)
  }

  if (is.null(context) & !is.null(distribution)){
    context <- ""
  }

  if (distribution == "debug"){
    "Not yet implemented"
  }
  else if (distribution == "normal"){
    if (context == "swedishinfants"){
      list_lookup_string <- paste(list(...)$infantvar,"-",list(...)$infantFM, sep = "")
      row_num <- which(swedish_infants[[list_lookup_string]]$GA == list(...)$infantGA)
      mean <- swedish_infants[[list_lookup_string]][row_num, "Mean"]
      sd <- swedish_infants[[list_lookup_string]][row_num, "SD"]
      #browser()
    }
    else{ # no context specified, use is directly passing parameters
      return(rnorm(n = n, ...))
    }
    # Adjust the parameters as needed.
    if (!is.null(adjust_parameter1)){
      mean <- mean + adjust_parameter1
    }
    if (!is.null(adjust_parameter2)){
      sd <- sd + adjust_parameter2
    }
    if (dump_stats){
      if (verbose) {print("dump_stats = TRUE, data will not be generated")}
      #browser()
      #browser()
      return(list(
        distribution = distribution,
        context = context,
        mean = mean,
        sd = sd,
        adjust_parameter1 = adjust_parameter1,
        adjust_parameter2 = adjust_parameter2
      ))
    }
    return(rnorm(n = n, mean = mean, sd = sd))
  }
  else if (distribution == "t"){
    if (dump_stats){
      if (verbose) {print("dump_stats = TRUE, data will not be generated")}
      #browser()
      return(list(
        distribution = distribution,
        context = context,
        df = list(...)$df,
        ncp = list(...)$ncp,
        adjust_parameter1 = adjust_parameter1,
        adjust_parameter2 = adjust_parameter2
      ))
    }
    return(rt(n = n, ...))
  }
  # else if (distribution == "t2"){
  #   return(rt(n = n, df = 2, ...))
  # }
  else if (distribution == "binomial"){
    if (dump_stats){
      if (verbose) {print("dump_stats = TRUE, data will not be generated")}
      #browser()
      return(list(
        distribution = distribution,
        context = context,
        size = list(...)$size,
        prob = list(...)$prob,
        adjust_parameter1 = adjust_parameter1,
        adjust_parameter2 = adjust_parameter2
      ))
    }
    return(rbinom(n = n, ...))
  }


  # Cauchy ------------------------------------------------------------------

  else if (distribution == "cauchy"){

    ## Contact Resistance ----
    #
    # Winterton, S.S., Smy, T.J. & Tarr, N.G. On the source of scatter in
    # contact resistance data. J. Electron. Mater. 21, 917–921 (1992).
    # https://doi.org/10.1007/BF02665549
    #
    # Parameter values estimated from Figure 7 using GPT-5.6 Luna
    #
    # Brief description: In manufacturing/designing semiconductors, contact
    # resistance is the specific resistance of the point where a semiconductor
    # is attached to metal, and understanding variation in this value is
    # important, especially as components continue to be miniaturized.
    #
    if (context == "contactresistance1"){
      return(rcauchy(n = n, location = 32.5, scale = 3.3))
    }
    else if (context == "contactresistance2"){
      return(rcauchy(n = n, location = 42, scale = 3.8))
    }

    ## Water Velocity ----
    #
    # Christensen, K. M., Gray, A. R., & Riser, S. C. (2024). Global estimates
    # of mesoscale vertical velocity near 1,000 m from Argo observations.
    # Journal of Geophysical Research: Oceans, 129(1), e2023JC020003.
    #
    # Parameter values estimated from Figure 5 using GPT-5.6 Luna
    #
    # Brief description: In the ocean, large volumes of water move up and/or
    # down long distances very slowly. This is called the mesoscale vertical
    # velocity. These are very small values, and while many of them are
    # relatively close to the median value, there are extreme events that occur
    # often. Researchers measured the mesoscale vertical velocities (units 10^-4
    # m/s) as six locations; the distribution of these values differ depending
    # on if the ocean is barren (normal dist.) or has topographical features
    # (cauchy dist.).
    else if (context == "watervelocity1"){ # PAR
      location <- 0.07
      scale <- 0.35
    }
    else if (context == "watervelocity2"){ # KP
      location <- 0.01
      scale <- 0.32
    }
    else if (context == "watervelocity3"){ # NA
      location <- -0.01
      scale <- 0.58
    }
    else if (context == "watervelocity4"){ # DP
      location <- 0.10
      scale <- 0.58
    }
    else if (context == "watervelocity5"){ # POG
      # plausible that a contaminated normal might be a better fit
      location <- 0
      scale <- 0.18
    }
    else if (context == "watervelocity6"){ # IOG
      # plausible that a contaminated normal might be a better fit
      location <- 0
      scale <- 0.23
    }
    # if no context specified
    else {
      return(rcauchy(n = n, ...))
    }
    # only reach here if a context was specified
    if (verbose){
      print(paste("Distribution: ", distribution, sep = ""))
      print(paste("Location:     ", location, sep = ""))
      print(paste("Scale:        ", scale, sep = ""))
      cat("\n\n")
    }
    # Adjust the parameters as needed.
    if (!is.null(adjust_parameter1)){
      location <- location + adjust_parameter1
    }
    if (!is.null(adjust_parameter2)){
      scale <- scale + adjust_parameter2
    }
    if (dump_stats){
      if (verbose) {print("dump_stats = TRUE, data will not be generated")}
      return(list(
        distribution = distribution,
        context = context,
        location = location,
        scale = scale,
        adjust_parameter1 = adjust_parameter1,
        adjust_parameter2 = adjust_parameter2
      ))
    }
    return(rcauchy(n = n, location = location, scale = scale))
  }


  # Lognormal ---------------------------------------------------------------

  else if (distribution == "lognormal"){

    ## Atlanta Air Pollution ----
    #
    # Goldman, G. T., Mulholland, J. A., Russell, A. G., Srivastava, A.,
    # Strickland, M. J., Klein, M., ... & Edgerton, E. S. (2010). Ambient air
    # pollutant measurement error: characterization and impacts in a
    # time-series epidemiologic study in Atlanta. Environmental science &
    # technology, 44(19), 7692.
    # Values from Table 1 p. 16
    #
    # Short description:
    #   Air pollution concentration in Atlanta
    #
    # Due to different measurement types, the following pairs are appropriate:
    #   pollution11, pollution12 (1-h max NO2; urban, rural)
    #   pollution21, pollution32 (24-h PM2.5; urban, rural)
    #   pollution31, pollution32 (24-h PM2.5-NH4; urban, rural)
    #
#    meanlog <- sdlog <- c()
    if (context == "pollution11"){
      # urban (site A), 1-h max NO2
      meanlog <- log(38.1)
      sdlog <- log(1.51)
      if (DEBUG){
        print("reach pollution11 DEBUG statement")
      }
    }
    else if (context == "pollution12"){
      # rural (site B), 1-h max NO2
      meanlog <- log(7.74)
      sdlog <- log(2.47)
    }
    else if (context == "pollution21"){
      # urban (site A), 24-h PM2.5
      meanlog <- log(15.2)
      sdlog <- log(1.62)
    }
    else if (context == "pollution22"){
      # rural (site B), 24-h PM2.5
      meanlog <- log(11.5)
      sdlog <- log(1.73)
    }
    else if (context == "pollution31"){
      # urban (site A), 24-h PM2.5-NH4
      meanlog <- log(2.02)
      sdlog <- log(1.79)
    }
    else if (context == "pollution32"){
      # rural (site B), 24-h PM2.5-NH4
      meanlog <- log(1.97)
      sdlog <- log(1.95)
    }

    ## Aerosol Optical Properties ----
    #
    #
    # Magi, B. I., Fu, Q., & Redemann, J. (2007). A methodology to retrieve
    # self‐consistent aerosol optical properties using common aircraft
    # measurements. Journal of Geophysical Research: Atmospheres, 112(D24).
    # Values from Table 2 p. 8
    #
    # Short description:
    #   Optical properties of aerosols from burning biofuels in Africa
    #   (Data collected in South Africa, Mozambique, Botswana, and Zambia)
    #
    else if (context == "aerosol1"){
      # 22 August 2000 (South Africa)
      meanlog <- log(0.173)
      sdlog <- log(1.886)
    }
    else if (context == "aerosol2"){
      # 24 August 2000 (Mozambique)
      meanlog <- log(0.196)
      sdlog <- log(1.923)
    }
    else if (context == "aerosol3"){
      # 31 August 2000 (Mozambique)
      meanlog <- log(0.152)
      sdlog <- log(1.732)
    }
    else if (context == "aerosol4"){
      # 3 September 2000 (Botswana)
      meanlog <- log(0.215)
      sdlog <- log(1.674)
    }
    else if (context == "aerosol5"){
      # 6 September 2000 (Zambia)
      meanlog <- log(0.198)
      sdlog <- log(1.799)
    }
    else if (context == "aerosol6"){
      # 6 September 2000 (Zambia) # about 30 minutes after aerosol5
      meanlog <- log(0.189)
      sdlog <- log(1.854)
    }
    # if no context specified
    else{
      if (DEBUG){
        print("parameters not passed to rlnorm")
        print(context)
      }
      return(rlnorm(n = n, ...))
    }
    if (DEBUG){
      print(meanlog)
      print(sdlog)
    }
    # only reach here if a context was specified
    if (verbose){
      print(paste("Distribution: ", distribution, sep = ""))
      print(paste("meanlog:      ", meanlog, sep = ""))
      print(paste("sdlog:        ", sdlog, sep = ""))
      cat("\n\n")
    }
    # Adjust the parameters as needed.
    if (!is.null(adjust_parameter1)){
      meanlog <- meanlog + adjust_parameter1
      if (verbose){
        print(paste("adjusted meanlog: ", meanlog, sep = ""))
      }
    }
    if (!is.null(adjust_parameter2)){
      sdlog <- sdlog + adjust_parameter2
      if (verbose){
        print(paste("adjusted sdlog: ", sdlog, sep = ""))
      }
    }
    if (dump_stats){
      if (verbose) {print("dump_stats = TRUE, data will not be generated")}
      return(list(
        distribution = distribution,
        context = context,
        meanlog = meanlog,
        sdlog = sdlog,
        adjust_parameter1 = adjust_parameter1,
        adjust_parameter2 = adjust_parameter2
      ))
    }
    return(rlnorm(n = n, meanlog = meanlog, sdlog = sdlog))
  }

  # Exponential -------------------------------------------------------------

  else if (distribution == "exponential"){

    ## Wait time examples ----

    ### Derriford Hospital (Inter-Arrival Times for AEC and GP/ED) ----
    #
    # This article provides examples of Exponential, Gamma, and Weibull
    #
    # Chalk, D. (2020). Using computer simulation to model the expansion needs
    # of the ambulatory emergency care unit at Derriford Hospital. Future
    # Healthcare Journal, 7(1), 60-64.
    #
    # Brief description: This study uses probability models to estimate simulate
    # the effects of changing the hours that the AEC is open and the
    # simultaneous capacity to offload some of the burden of experienced by the
    # ED and other wards at the hospital. Various distributions are used to
    # model interarrival times (times between arrivals).
    #
    # The researchers had access to AEC arrivals data and arrivals data to other
    # hospital departments/wards for patients who might have benefited from the
    # AEC instead of where they received treatment.
    #
    # Abbreviations:
    #       AEC = Ambulatory Emergency Care
    #       GPs = Acute General Practitioners
    #       ED  = Emergency Department
    #       MAU = Medical Assessment Unit
    #   GPs and ED refer patients to AEC (lowers pressure on EDs)
    #   Patients at AEC when it closes are admitted to MAU which counts as hospital admission.
    #
    # Most likely comparisons for teaching purposes:
    #   Actual 2016 AEC vs. Actual 2016 GPED (_1 and _1)
    #   Weekday GPED vs. Weekend GPED (_1 and _2)
    #
    # Rates are constructed as 1/mean with mean given in minutes
    if (length(context) > 0){

    }
    if (context == "derriford_interrarival_AEC1"){
      # this is the actual AEC interarrival time at the AEC in 2016
      rate <- 1/39
    }
    else if (context == "derriford_interrarival_GPED1"){
      # This is the actual interarrival time for GP and ED patients combined in 2016 (all data)
      # For simplicity and teaching purposes this can be described as weekDAY arrivals (probably includes weekday and weekend)
      rate <- 1/37.1
    }
    else if (context == "derriford_interrarival_GPED2"){
      # This is the actual interarrival time for GP and ED patients combined in 2016 (weekend data)
      # For simplicity and teaching purposes this can be described as weekEND arrivals
      rate <- 1/42.9
    }

    ### Earthquake Occurences ----
    #
    #Polymenis, A. Modelling earthquake interevent times using mixtures of
    #exponential distributions. Discov Appl Sci 7, 343 (2025).
    #https://doi.org/10.1007/s42452-025-06790-z
    #
    else if (context == "earthquakes_turkey1"){
      rate <- 0.0006995
    }
    #
    # "Each year, approximately 450 earthquakes occur in eastern Canada. Of this
    # number, perhaps four will exceed magnitude 4, thirty will exceed magnitude
    # 3, and about twenty-five events will be reported felt. A decade will, on
    # average, include three events greater than magnitude 5."
    # https://www.seismescanada.rncan.gc.ca/zones/eastcan-en.php
    #
    else if (context == "earthquakes_ec_any"){
      rate <- 450/365
    }
    else if (context == "earthquakes_ec_4"){
      rate <- 4/365
    }
    else if (context == "earthquakes_ec_3"){
      rate <- 30/365
    }
    else if (context == "earthquakes_ec_felt"){
      rate <- 25/365
    }
    else if (context == "earthquakes_ec_5"){
      rate <- 3/(365*10)
    }

    ## Radioactive Decay ----

    # rate = log_e(2)/halflife
    # mean halflife = 1/rate
    else if (context == "radioactivedecay"){
      # radioactive_data_path <- "data/radioactive-decay/radioactive-decay-clean.csv" # change when making a package
      # radioactive_data <- read.csv(radioactive_data_path)
      if (!("isotope" %in% names(list(...)))){
        if (!verbose){
          warning("For this context, isotope must be specified.\nRerun with verbose = TRUE to see a list of isotopes (long).")
          return(NA)
        }
        else{
          print("List of available isotopes, from shortest to longest half-life:")
          print(radioactive_data$isotope)
          return(NA)
        }
      }
      else{
        isotope <- list(...)[["isotope"]]
        if (!(isotope %in% radioactive_data$isotope)){
          warning("Isotope specified not available.")
          print("List of available isotopes, from shortest to longest half-life:")
          print(radioactive_data$isotope)
          return(NA)
        }
        if (!("halflife_unit" %in% names(list(...)))){
          halflife_unit <- "seconds"
        }
        else{
          halflife_unit <- list(...)[["halflife_unit"]]
        }
        #browser()
        rate <- 1/radioactive_data[radioactive_data$isotope == isotope, halflife_unit]
      }
    }

    ## Raindrop Sizes ----
    #
    # This context is based on the older Marshall-Palmer formula which uses an
    # exponential distribution to estimate the number of drops in a given volume
    # of air over time. There's some mismatch with the coefficient reported in
    # the paper vs. the coefficient I use here implicitly. As it is, this
    # probability distribution can be used to talk about the PROPORTION of drops
    # that are within certain sizes (mm) while the paper wanted to estimate the
    # COUNT. I will  supply a sample size in the problem/example. I also wrote
    # two functions to help with the construction of examples/problems:
    # estimate_number_drops() and raindrop_context_helper().
    #
    # For a 150 cm^2 collector over 20 minutes, the number of drops in heavy rain (10mm/hour) is about 200,000.
    # Very, very rough approximation.
    #
    # Marshall, J. S., & Palmer, W. M. K. (1948). THE DISTRIBUTION OF RAINDROPS
    # WITH SIZE. Journal of Meteorology, 5(4), 165–166.
    # https://doi.org/10.1175/1520-0469(1948)005<0165:tdorws>2.0.co;2
    #
    # Others have done work extending the use of the exponential distribution as
    # well as trying gamma and lognormal distributions for raindrop sizes, but
    # I haven't yet incorporated them.
    #
    else if (context == "raindrops1"){
      if (!("rainfall_mm_per_hour" %in% names(list(...)))){
        rainfall_mm_per_hour <- 10 # default to 10 mm/hour (heavy rain)
      }

      rate <- 4.1 * (rainfall_mm_per_hour)^(-0.21) # units are 1/mm
    }

    ## (Wrap-up) ----

    else {
      if (dump_stats){
        if (verbose) {print("dump_stats = TRUE, data will not be generated")}
        #browser()
        return(list(
          distribution = distribution,
          context = context,
          rate = list(...)$rate,
          adjust_parameter1 = adjust_parameter1,
          adjust_parameter2 = adjust_parameter2
        ))
      }
      return(rexp(n = n, ...))
    }
    # Adjust the parameters as needed.
    if (!is.null(adjust_parameter1)){
      rate <- rate + adjust_parameter1
    }
    if (dump_stats){
      if (verbose) {print("dump_stats = TRUE, data will not be generated")}
      #browser()
      return(list(
        distribution = distribution,
        context = context,
        rate = rate,
        adjust_parameter1 = adjust_parameter1,
        adjust_parameter2 = adjust_parameter2
      ))
    }
    return(rexp(n = n, rate = rate))
  }


  # Gamma -------------------------------------------------------------------
  else if (distribution == "gamma"){

    ## Derriford Hospital (Length of Stay in AEC) ----
    #
    # This article provides examples of Exponential, Gamma, and Weibull
    #
    # Chalk, D. (2020). Using computer simulation to model the expansion needs
    # of the ambulatory emergency care unit at Derriford Hospital. Future
    # Healthcare Journal, 7(1), 60-64.
    #
    # Brief description: This study uses probability models to estimate simulate
    # the effects of changing the hours that the AEC is open and the
    # simultaneous capacity to offload some of the burden of experienced by the
    # ED and other wards at the hospital. Various distributions are used to
    # model interarrival times (times between arrivals).
    #
    # The researchers had access to AEC arrivals data and arrivals data to other
    # hospital departments/wards for patients who might have benefited from the
    # AEC instead of where they received treatment.
    #
    # Abbreviations:
    #       AEC = Ambulatory Emergency Care
    #       GPs = Acute General Practitioners
    #       ED  = Emergency Department
    #       MAU = Medical Assessment Unit
    #   GPs and ED refer patients to AEC (lowers pressure on EDs)
    #   Patients at AEC when it closes are admitted to MAU which counts as hospital admission.
    #
    if (context == "derriford_lengthofstay1"){
      # length of stay in AEC in minutes
      shape <- 2.63
      scale <- 84.4
    }
    else {
      rgamma(n = n, ...)
    }
    rgamma(n = n, shape = shape, scale = scale)
  }


  # Weibull -----------------------------------------------------------------

  else if (distribution == "weibull"){

    ## Derriford Hospital (Ward Inter-Arrival Times) ----
    #
    # This article provides examples of Exponential, Gamma, and Weibull
    #
    # Chalk, D. (2020). Using computer simulation to model the expansion needs
    # of the ambulatory emergency care unit at Derriford Hospital. Future
    # Healthcare Journal, 7(1), 60-64.
    #
    # Brief description: This study uses probability models to estimate simulate
    # the effects of changing the hours that the AEC is open and the
    # simultaneous capacity to offload some of the burden of experienced by the
    # ED and other wards at the hospital. Various distributions are used to
    # model interarrival times (times between arrivals).
    #
    # The researchers had access to AEC arrivals data and arrivals data to other
    # hospital departments/wards for patients who might have benefited from the
    # AEC instead of where they received treatment.
    #
    # Abbreviations:
    #       AEC = Ambulatory Emergency Care
    #       GPs = Acute General Practitioners
    #       ED  = Emergency Department
    #       MAU = Medical Assessment Unit
    #   GPs and ED refer patients to AEC (lowers pressure on EDs)
    #   Patients at AEC when it closes are admitted to MAU which counts as hospital admission.
    #
    if (context == "derriford_interrarival_ward1"){
      # this models the interarrival time of patients to hospital wards who were
      # determined to be candidates for the AEC after the fact, so about every
      # 101 minutes a patient was admitted to a hospital ward whose stay ended
      # up being 0 or 1 days and thus was a candidate for the AEC
      #
      # this can be thought of as weekday arrivals (though it seems like it was based on all data)
      #
      shape <- 0.982
      scale <-  101
      #weibull_mean <- function(scale, shape){scale * gamma(1 + 1/shape)}
    }
    else if (context == "derriford_interrarival_ward2"){
      # this models the interarrival time of patients to hospital wards who were
      # determined to be candidates for the AEC after the fact, so about every
      # 101 minutes a patient was admitted to a hospital ward whose stay ended
      # up being 0 or 1 days and thus was a candidate for the AEC
      #
      # this can be thought of as weekend arrivals
      #
      shape <- 1.04
      scale <-  116
    }
    else {
      if (dump_stats){
        if (verbose) {print("dump_stats = TRUE, data will not be generated")}
        #browser()
        return(list(
          distribution = distribution,
          context = context,
          shape = list(...)$shape,
          scale = list(...)$scale,
          adjust_parameter1 = adjust_parameter1,
          adjust_parameter2 = adjust_parameter2
        ))
      }
      return(rweibull(n = n, ...))
    }
    # Adjust the parameters as needed.
    if (!is.null(adjust_parameter1)){
      shape <- shape + adjust_parameter1
    }
    if (!is.null(adjust_parameter2)){
      scale <- scale + adjust_parameter2
    }
    if (dump_stats){
      if (verbose) {print("dump_stats = TRUE, data will not be generated")}
      #browser()
      return(list(
        distribution = distribution,
        context = context,
        shape = shape,
        scale = scale,
        adjust_parameter1 = adjust_parameter1,
        adjust_parameter2 = adjust_parameter2
      ))
    }
    return(rweibull(n = n, shape = shape, scale = scale))
  }




  # End of function, nothing below this line
}
