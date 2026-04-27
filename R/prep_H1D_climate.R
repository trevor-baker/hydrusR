#' Prepare climate data for Hydrus-1D
#'
#' Prepare climate data for Hydrus-1D. Returns a dataframe and optionally writes it to file in your project directory.
#' @param project.path Hydrus project path. Must have SELECTOR.IN already, which is made by create.H1D.project().
#' @param TimeUnit character, length 1. your project's time unit (default = "hours"). permitted: "seconds", "minutes", "hours", "days", "years".
#' @param SpaceUnit character, length 1. your project's space (length) unit (default = "cm"). permitted: "mm", "cm", "m".
#' @param endTime numeric, length = 1. what is the ending time of your simulation? in project TimeUnits.
#' @param time_values numeric, any length. Last row must be for the maximum time (endTime) of your simulation. Given in your project's TimeUnit. Two
#' formats of climate data inputs are allowed to this function: time_values is given with one or both of PPT_values and Prec_values; or time_values
#' can be omitted (left as NULL) and simple daily rates of PET_mmd and/or Prec_mmd can be given.
#' @param PET_mmd numeric, length 1. PET rate in mm/d. always in mm/d, no matter your project's units. This can be omitted if 'PET_values' is given.
#' @param PET_values numeric vector, any length. Potential evapotranspiration values at each interval listed in 'time_values'. Length must match the
#' length of 'time_values'. Unlike PET_mmd, which is alwys in mm/d, this vector is in project units, SpaceUnit per TimeUnit.
#' @param trans.pc numeric, length = 1, between 0 and 1. If PET is a column in atmos.df, what percent of PET should be assigned to transpiration
#' (rRoot)? The remainder will be assigned to evaporation (rSoil). Default = 1, i.e. all transpiration. Not required if 'PET' column is not given in
#' atmos.df.
#' @param Prec_mmd numeric, length 1. Precipitation rate in mm/d. Always in mm/d, no matter your project's units. This can be omitted if 'Prec_values' is given.
#' @param Prec_values numeric vector, any length. Precipitation values at each interval listed in 'time_values'. Length must match the length of
#' 'time_values'. Unlike Prec_mmd, which is alwys in mm/d, this vector is in project units, SpaceUnit per TimeUnit.
#' @param diurnal logical, length 1. If your TimeUnit is less than "days": TRUE - PET is partitioned into diurnal patterns by pet.hourly(); FALSE -
#' distributed evenly across all hours (FALSE). Ignored if TimeUnit = "days" or "years"
#' @param diurnal.hours integer, length 2. Default c(6,20). Giving ~sunrise and ~sunset times in 24-hour format. The first hour with non-zero PET in
#' the morning, and the first hour with zero PET at night. Ignored if diurnal = FALSE or TimeUnit = "days" or "years".
#' @param save.df logical, length 1. Default FALSE. should this climate data be saved as 'df_clim.csv' to the project.path? The reason to do this
#' would be for long simulations that need to utilize the looping function run.H1D.simulation. With code edits there, it could read the csv file to
#' select climate data for the next loop iteration.
#' @export
#' @examples
#' ppath <- "C:/users/t/documents/temp/tdbexample1"
#' #as simple example using _mmd format and no diurnal pattern
#' df.at1 <- prep.H1D.climate(project.path = ppath,
#'                            TimeUnit = "hours", SpaceUnit = "cm",
#'                            endTime = 24*365,
#'                            Prec_mmd = 4,
#'                            PET_mmd = 3,
#'                            diurnal = FALSE,
#'                            trans.pc = 0.8)
#' df.at1[1:24,] #all hours are the same, no diurnal
#' sum(df.at1$rRoot[1:24])
#' sum(df.at1$rSoil[1:24]) #0.3 cm/d for rRoot+rSoil = 3 mm/d
#' sum(df.at1$Prec[1:24]) #0.4 cm/d Prec = 4 mm/d
#' sum(df.at1$Prec) #yearly sum is 146 cm, 1460 mm.
#'
#' #same but with diurnal pattern
#' df.at2 <- prep.H1D.climate(project.path = ppath,
#'                            TimeUnit = "hours", SpaceUnit = "cm",
#'                            endTime = 24*365,
#'                            Prec_mmd = 4,
#'                            PET_mmd = 3,
#'                            diurnal = TRUE,
#'                            trans.pc = 0.8)
#' df.at2[1:24,] #all hours are the same for Prec, but diurnal for rRoot and rSoil - as intended
#' sum(df.at2$rRoot[1:24])
#' sum(df.at2$rSoil[1:24]) #0.3 cm/d for rRoot+rSoil = 3 mm/d
#' sum(df.at2$Prec[1:24]) #0.4 cm/d Prec = 4 mm/d
#' sum(df.at2$Prec) #yearly sum is 146 cm, 1460 mm.
#'
#' #now the same as df.at2 but with m/year units
#' df.at3 <- prep.H1D.climate(project.path = ppath,
#'                            TimeUnit = "years", SpaceUnit = "m",
#'                            endTime = 1.1,
#'                            Prec_mmd = 4,
#'                            PET_mmd = 3,
#'                            diurnal = TRUE,
#'                            trans.pc = 0.8)
#' df.at3 #this has only one row, covers the whole 1.1 years.
#' # all values are rates, and rates match the ones above after unit conversion:
#' df.at3$rRoot/365 *1000 #2.4 mm/d
#' df.at3$rSoil/365 *1000 #0.6 mm/d = 3 mm/d together
#' df.at3$Prec/365 *1000 #4 mm/d
#'
#' #now similar to df.atm1 but with _values vectors, not _mmd format
#' # added in a couple short timesteps of zero values to show that _values get put into final result properly
#' df.at4 <- prep.H1D.climate(project.path = ppath,
#'                            TimeUnit = "hours", SpaceUnit = "cm",
#'                            endTime = 2400,
#'                            time_values = c(1,2399,2400),
#'                            #same values as df.atm1 but I need to do time and length conversions myself
#'                            Prec_values = c(4/10/24, 4/10/24, 0),
#'                            PET_values =  c(0, 3/10/24, 3/10/24),
#'                            trans.pc = 0.8)
#' df.at4
#' df.at1[c(1,2399,2400),] #this is basically the same except the rates are exactly same, never zero, for all hours
#'
#' #now a similar one with no PET
#' df.at5 <- prep.H1D.climate(project.path = ppath,
#'                            TimeUnit = "hours", SpaceUnit = "cm",
#'                            endTime = 2400,
#'                            time_values = c(1,2399,2400),
#'                            #same values as df.atm1 but I need to do time and length conversions myself
#'                            Prec_values = c(4/10/24, 4/10/24, 0),
#'                            #PET_values =  c(0, 3/10/24, 3/10/24),
#'                            trans.pc = 0.8)
#' df.at5 #missing rRoot and rSoil, as intended
#' df.at4 #same Prec
#'
#' #now a similar one with no PET
#' df.at6 <- prep.H1D.climate(project.path = ppath,
#'                            TimeUnit = "hours", SpaceUnit = "cm",
#'                           endTime = 2400,
#'                           time_values = c(1,2399,2400),
#'                            #same values as df.atm1 but I need to do time and length conversions myself
#'                            #Prec_values = c(4/10/24, 4/10/24, 0),
#'                            PET_values =  c(0, 3/10/24, 3/10/24),
#'                            trans.pc = 0.8)
#' df.at6 #missing Prec, as intended
#' df.at4 #same rRoot and rSoil
#'
#' #an example with big variations in PET and Prec over a long time
#' df.at7 <- prep.H1D.climate(project.path = ppath,
#'                            TimeUnit = "hours", SpaceUnit = "cm",
#'                            endTime = 2400,
#'                            time_values = 1:2000,
#'                            #strange values, in whatever pattern
#'                            Prec_values = c( (1:100)/100*3+1, rnorm(2200,5,1), (100:1)/100*5+1),
#'                            PET_values =  sort(c(rnorm(1200,2,1),rnorm(1200,6,2))),
#'                            trans.pc = 0.8)
#' #the values carry through as expected:
#' plot(df.at7$time, df.at7$Prec)
#' points(df.at7$time, df.at7$rSoil+df.at7$rRoot, col = "red")


prep.H1D.climate <- function(project.path,
                             TimeUnit = "hours",
                             SpaceUnit = "cm",
                             endTime = NULL,
                             time_values = NULL,
                             PET_mmd = 4,
                             PET_values = NULL,
                             trans.pc = 1,
                             Prec_mmd = 0.5,
                             Prec_values = NULL,
                             diurnal = TRUE,
                             diurnal.hours = c(6,20),
                             save.df = FALSE){



  ###
  # check arguments
  if(!TimeUnit %in% c("seconds","minutes","hours","days","years")){
    stop("TimeUnit must be one of: seconds, minutes, hours, days, years")
  }
  if(!SpaceUnit %in% c("mm","cm","m")){
    stop("SpaceUnit must be one of: mm, cm, m")
  }
  if(is.null(endTime)){
    stop("endTime must always be given")
  }
  if(!is.null(time_values)){
    if(max(time_values) != endTime){
      stop("time_values vector must end at 'endTime'.")
    }

    if(!is.null(PET_values)){
      if(length(PET_values) != length(time_values)){
        stop("time_values and PET_values must be the same length.")
      }
    }
    if(!is.null(Prec_values)){
      if(length(Prec_values) != length(time_values)){
        stop("time_values and Prec_values must be the same length.")
      }
    }

    if(is.null(PET_values) & is.null(Prec_values)){
      cat("If 'time_values' is given, then one or both of 'PET_values' and 'Prec_values' is expected. 'time_values' will be ignored.\n")
    } else {
      #if it made it this far without stopping, then there are valid vectors and the daily mmd values, if any, should be overwritten to NULL
      PET_mmd <- NULL
      Prec_mmd <- NULL
    }

  }
  if(!is.null(PET_values) | !is.null(PET_mmd)){
    #if there is PET, then transpc must be valid
    if(trans.pc < 0 | trans.pc > 1){
      stop("trans.pc must be between 0 and 1")
    }
  }


  ###
  # prep daily_mmd values
  # - if these made it to here as non-NULL (i.e., time_values and PET/Prec_values were not given), then these need to be processed
  #     into vectors for df.clim
  if(!is.null(PET_mmd)){

    #split into dirunal if necessary and requested
    if(TimeUnit %in% c("seconds","minutes","hours")){ #diurnal only matters if time unit is shorter than a day
      if(diurnal){
        PET_values.mmd <- pet.hourly(PET_mmd, hours = diurnal.hours[1]:diurnal.hours[2]) #this splits PET into length per hour starting from length per day.
        PET_values.mmd <- PET_values.mmd$pet #get just the vector, not dataframe
        PET_values.mmd <- PET_values.mmd * 24 #need to keep units in mm/d for now. it will be converted below to project units.
      } else {
        #else it is an even 24 hour vector
        PET_values.mmd <- rep(PET_mmd, 24) #in mm/d units
      }
      #in either case, diurnal or flat, set time_values as 1:24 to start. the PET_values above will always be one per hour here.
      time_values <- 1:24

    } else {                #longer time unit gets
      PET_values.mmd <- PET_mmd # one single value in mm/d
      time_values <- 1 #time.values are one per day so far
    }


    #the values above are in mm/d but I need them in project units (length L per day == .Ld)
    # - convert space
    if(SpaceUnit == "cm"){
      PET_values.Ld <- PET_values.mmd/10
    } else if(SpaceUnit == "m"){
      PET_values.Ld <- PET_values.mmd/1000
    } else {
      PET_values.Ld <- PET_values.mmd #no conversion if SpaceUnit = mm because it is already in mm
    }
    # - convert for time
    if(TimeUnit == "seconds"){
      time_mult <- 24*60*60
    } else if(TimeUnit == "minutes"){
      time_mult <- 24*60
    } else if(TimeUnit == "hours"){
      time_mult <- 24
    } else if(TimeUnit == "days"){
      time_mult <- 1
    } else if(TimeUnit == "years"){
      time_mult <- 1/365
    }
    PET_values.LT <- PET_values.Ld / time_mult #convert rate to the correct unit. this gives Length per Time units .LT

    #now that time_mult is known, convert time_values to project units
    #this is a bit confusing because intuitively it seems like it should be the number of TimeUnits in a day (because
    # PET originally came as mm/d). However, time_mult needs to be divided by 24 for seconds, minutes, and hours because their
    # time_values are hourly (length = 24)
    if(TimeUnit %in% c("seconds", "minutes","hours")){
      time_values <- time_values * time_mult/24

      #replicate each hours in vector of hourly values to its correct length to align with TimeUnit
      # - vector is currently 24 hourly values. each hour needs to be 60*60 times for second units or
      #     60 times for minute units if TimeUnit is shorter.
      # - rep them evenly, not trying to set a pretty bell curve
      PET_values <- sapply(1:24, function(x){ rep(PET_values.LT[x], time_mult/24) })

      #make the vectors extremely long (repeat 10000 times) so they can be trimmed below no matter the endTime
      PET_values2 <- PET_values[rep(1:length(PET_values), 1e5)]
      time_values2 <- as.vector( sapply(1:1e5, function(x){ max(time_values)*(x-1) + time_values }) )
      #cut it off at endTime
      end.indx <- min(which(time_values2 >= endTime))
      time_values <- time_values2[1:end.indx]
      time_values[length(time_values)] <- endTime #force it to have same end time
      PET_values <- PET_values2[1:end.indx]
      rm(list = c("time_values2", "PET_values2")) #remove large objects

    } else {
      #if this is days or years, then the final step is very easy.
      # the data is simply one value, it is in correct units already, and because this is a flat _mmd format, this value
      #   extends to the very end of the simulation
      time_values <- endTime
      PET_values <- PET_values.LT

    }

    # #for debug
    # head(cbind(time_values, time_values2, PET_values.mmd, PET_values.Ld, PET_values.LT))

    df.pet <- data.frame(time = time_values,
                         PET = PET_values)
  } else {
    #if PET_mmd is NULL, then the steps above are skipped because PET_values should already be in project units and be of correct length
    df.pet <- NULL
  }


  #repeat same steps for Prec_mmd
  if(!is.null(Prec_mmd)){

    #split into dirunal if necessary and requested
    if(TimeUnit %in% c("seconds","minutes","hours")){ #diurnal only matters if time unit is shorter than a day

      #there is no diurnal for Precip so this step is simpler than for PET_mmd
      Prec_values.mmd <- rep(Prec_mmd, 24) #in mm/d units
      #in either case, diurnal or flat, set time_values as 1:24 to start. the PET_values above will always be one per hour here.
      time_values <- 1:24

    } else {                #longer time unit gets
      Prec_values.mmd <- Prec_mmd # one single value in mm/d
      time_values <- 1 #time.values are one per day so far
    }


    #the values above are in mm/d but I need them in project units (length L per day == .Ld)
    # - convert space
    if(SpaceUnit == "cm"){
      Prec_values.Ld <- Prec_values.mmd/10
    } else if(SpaceUnit == "m"){
      Prec_values.Ld <- Prec_values.mmd/1000
    } else {
      Prec_values.Ld <- Prec_values.mmd #no conversion if SpaceUnit = mm because it is already in mm
    }
    # - convert for time
    if(TimeUnit == "seconds"){
      time_mult <- 24*60*60
    } else if(TimeUnit == "minutes"){
      time_mult <- 24*60
    } else if(TimeUnit == "hours"){
      time_mult <- 24
    } else if(TimeUnit == "days"){
      time_mult <- 1
    } else if(TimeUnit == "years"){
      time_mult <- 1/365
    }
    Prec_values.LT <- Prec_values.Ld / time_mult #convert rate to the correct unit. this gives Length per Time units .LT

    #now that time_mult is known, convert time_values to project units
    #this is a bit confusing because intuitively it seems like it should be the number of TimeUnits in a day (because
    # PET originally came as mm/d). However, time_mult needs to be divided by 24 for seconds, minutes, and hours because their
    # time_values are hourly (length = 24)
    if(TimeUnit %in% c("seconds", "minutes","hours")){
      time_values <- time_values * time_mult/24

      #replicate each hours in vector of hourly values to its correct length to align with TimeUnit
      # - vector is currently 24 hourly values. each hour needs to be 60*60 times for second units or
      #     60 times for minute units if TimeUnit is shorter.
      # - rep them evenly, not trying to set a pretty bell curve
      Prec_values <- sapply(1:24, function(x){ rep(Prec_values.LT[x], time_mult/24) })

      #make the vectors extremely long (repeat 10000 times) so they can be trimmed below no matter the endTime
      Prec_values2 <- Prec_values[rep(1:length(Prec_values), 1e5)]
      time_values2 <- as.vector( sapply(1:1e5, function(x){ max(time_values)*(x-1) + time_values }) )
      #cut it off at endTime
      end.indx <- min(which(time_values2 >= endTime))
      time_values <- time_values2[1:end.indx]
      time_values[length(time_values)] <- endTime #force it to have same end time
      Prec_values <- Prec_values2[1:end.indx]
      rm(list = c("time_values2", "Prec_values2")) #remove large objects

    } else {
      #if this is days or years, then the final step is very easy.
      # the data is simply one value, it is in correct units already, and because this is a flat _mmd format, this value
      #   extends to the very end of the simulation
      time_values <- endTime
      Prec_values <- Prec_values.LT

    }

    # #for debug
    # head(cbind(time_values, time_values2, Prec_values.mmd, Prec_values.Ld, Prec_values.LT))

    df.prec <- data.frame(time = time_values,
                          Prec = Prec_values)
  } else {
    #if Prec_mmd is NULL, then the steps above are skipped because Prec_values should already be in project units and be of correct length due to earlier prepping
    df.prec <- NULL
  }

  #this is a clunky way of extracting data back into vectors
  # the dataframes made just above are temporary and this code is inefficient but it works...
  if(!is.null(df.prec) & !is.null(df.pet)){
    if(!identical(df.prec$time, df.pet$time)){
      stop("error in Prec_mmd or PET_mmd processing.")
    } else {
      time_values <- df.prec$time #doesn't matter which one gives these values because they are identical
      PET_values <- df.pet$PET
      Prec_values <- df.prec$Prec
    }

  } else if(!is.null(df.pet)){
    time_values <- df.pet$time
    PET_values <- df.pet$PET

  } else  if(!is.null(df.prec)){
    time_values <- df.prec$time
    Prec_values <- df.prec$Prec
  }


  #no matter whether these came from _values arguments, or were translated from _mmd values, these vectors need to be bound into a dataframe
  #bind_cols to make a dataframe
  df.clim <- dplyr::bind_cols(time = time_values,
                              PET = PET_values, #doesn't matter if one of PPT or Prec is NULL, bind_cols will work
                              Prec = Prec_values)

  #if there is PET data (might not be if PET_values was NULL to start and no PET_mmd)
  # then convert it to rSoil and rRoot using trans.pc
  if(any(names(df.clim) == "PET")){
    df.clim$rRoot <- df.clim$PET * trans.pc
    df.clim$rSoil <- df.clim$PET * (1-trans.pc)
    df.clim <- df.clim[,-which(names(df.clim)=="PET")] #drop PET column after converting it
  }



  #depending on the argument, this may get saved to csv
  if(save.df){
    this.path <- file.path(project.path, "df_clim.csv")
    write.csv(df.clim, this.path,
              row.names = FALSE,
              append = FALSE)
  }


  return(df.clim)

} #end fn
