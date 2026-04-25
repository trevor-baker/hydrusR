#' Create inputs to set boundary conditions (atmospheric, top, and bottom)
#'
#' This functions houses write.bottom.bc and write.atmosph.in, which work together to set the boundary conditions for a Hydrus simulation. Based on
#' the supplied arguments, it creates a dataframe of variable boundary conditions, including any atmospheric conditions, sets up the required
#' blocks of SELECTOR.IN, and writes ATMOSPH.IN. \cr
#' This is one of three parts to prepping a HYDRUS simulation, with create.H1D project and create.soil.profile. This function must be preceded by
#' create.H1D.project() because a pre-existing SELECTOR.IN file is required to pull data from, such as LUnit (length unit), TUnit (time unit),
#' start time (tInit), and end time (tMax), to ensure that the boundary condition data is in alignment. \cr
#' There are several Hydrus boundary condition settings that are not currently coded in this function, including bottom deep drainage, bottom seepage
#' face, and bottom flow to drains.
#' @param project.path Hydrus project path. Must have SELECTOR.IN already, which is made by create.H1D.project().
#' @param atmos logical, length = 1. Does this project have atmospheric conditions as a boundary condition? If TRUE, this overrides any argument given
#' to 'top.bc.type' and 'top.bc.value'. If FALSE, an argument must be supplied to 'top.bc.type'
#' @param atmos.df dataframe woith columns for 'time' and one or more columns of 'Prec' (precipitation rate, L/T), 'rSoil' (potential evaporation rate,
#' L/T), 'rRoot' (potential transpiration rate, L/T). a column 'PET' (potential evapotranspiration rate, L/T), can also be given instead of rSoil
#' and rRoot, and PET will be split according to 'trans.pc'. If PET is given, rSoil and rRoot will be overwritten. Note that these are all rates (L/T)
#' not depths per the given time period. \cr
#' The times in the dataframe are used as the end points of the period (not starting points). As such, the upper row of the dataframe should not
#' begin at time=0, the first row will define the conditions from time=0 until time[1], and the second row will define conditions from time[1] to
#' time[2], and so on. If a constant atmospheric flux is desired, then the dataframe will have only one row, with time = max(time), the endpoint of
#' the simulation, and the values given will be applied for the whole duration.
#' @param trans.pc numeric, length = 1, between 0 and 1. If PET is a column in atmos.df, what percent of PET should be assigned to transpiration
#' (rRoot)? The remainder will be assigned to evaporation (rSoil). Default = 1, i.e. all transpiration. Not required if 'PET' column is not given in
#' atmos.df.
#' @param hCritS numeric, length = 1. the maximum allowed surface head. in project length units. Only required if atmos = TRUE. Default = 0, meaning
#' that water isn't allowed to pond on the surface. Set this value > 0 if ponding is to be allowed. Do not set this < 0. In the Hydrus GUI 'Water
#' Flow Boundary Conditions' menu, setting this to 0 is equivalent to selecting 'Atmospheric BC with Surface Run Off' and setting it > 0 is
#' 'Atmospheric BC with Surface Layer'.
#' @param top.bc.type character, length = 1. if atmos = TRUE, this is ignored. if atmos = FALSE, then one of four types is expected here: 'ch' constant
#' head, 'cf' constant flux, 'vh' variable head, 'vf' variable flux. If 'cf' or 'ch' then a numeric value must be supplied to 'top.bc.value'. If 'vf'
#' or 'vh', then a dataframe must be given to 'top.bc.value'.
#' @param top.bc.value either a numeric, length = 1, if top.bc.type = 'cf' or 'ch'; or a dataframe with two columns 'time' and 'head' (vh) or 'flux'
#' (vf) if top.bc.type = 'vf' or 'vh'. all must obey project units, with flux given as a rate (length/time).
#' @param FreeD logical, length = 1. default TRUE. is the bottom boundary free draining? meaning should a unity gradient be assigned throughout the simulation?
#' (i.e., matric potential is at equilibrium, and flux driven by gravity only)0. if this is TRUE, then bot.bc.type is ignored. if this is FALSE, then
#' bot.bc.type and bot.bc.value must be given.
#' @param bot.bc.type character, length = 1. if FreeD = TRUE, this is ignored. if FreeD = FALSE, then one of four types is expected here: 'ch' constant
#' head, 'cf' constant flux, 'vh' variable head, 'vf' variable flux. If 'cf' or 'ch' then a numeric value must be supplied to 'bot.bc.value'. If 'vf'
#' or 'vh', then a dataframe must be given to 'bot.bc.value'.
#' @param hCritA numeric, length = 1. the minimum allowed surface head, i.e. a dryness limit. Once this is hit, evaporation will cease. If left NULL,
#' the default will be set at 1e5 cm, which will be converted to the unit given by SELECTOR.IN's LUnit. This can be left as NULL for nearly all uses.
#' @export

create.bc <- function(project.path,
                      atmos = TRUE,
                      atmos.df = NULL,
                      hCritS = 0,
                      top.bc.type = NULL,
                      top.bc.value = NULL,
                      FreeD = TRUE,
                      bot.bc.type = NULL,
                      bot.bc.value = NULL,
                      hCritA = NULL, ...){


  ###
  # read SELECTOR.IN to get pre-set parameters
  sel.in = readLines(con = file.path(project.path, "SELECTOR.IN"),
                     n = -1L, encoding = "unknown")
  #get units
  unit_line <- which(grepl("LUnit", sel.in))
  l.unit <- trimws(sel.in[unit_line+1])
  t.unit <- trimws(sel.in[unit_line+2])
  #get times
  time_line <- which(grepl("tInit", sel.in))
  time_dat <- sel.in[time_line+1]
  time_dat <- strsplit(time_dat, " ")[[1]]
  time_dat <- time_dat[-which(time_dat == "")]
  t0 <- as.numeric(time_dat[1])
  t1 <- as.numeric(time_dat[2])

  #End section to get SELECTOR.IN info
  ####


  ####
  # clean arguments

  if(atmos){

    #this is needed so that NULL args don't trip the data prep code below
    # if atmos is TRUE, then top.bc is atmos. there can't be a top.bc besides the atmospheric inputs
    top.bc.type <- "atmos"

    if(is.null(atmos.df)){
      stop("'atmos.df' must be given")
    } else {

      #cap the maximum time
      if(max(atmos.df$time) > t1){
        print("atmos.df needed to be cut off at the end time defined in SELECTOR.IN (tMax). Double check your data.")
        if(all(atmos.df$time > t1)){ #if all are higher, then keep only last row
          atmos.df <- atmos.df[nrow(atmos.df),]
          atmos.df$time <- t1
        } else {
          #else only some are higher. drop all but the first too-high row
          too.high <- which(atmos.df$time > t1)
          too.high <- too.high[1:(length(too.high)-1)] #keep the last one that was too high
          atmos.df <- atmos.df[-too.high,]
          atmos.df$time[nrow(atmos.df)] <- t1
        }
      } #end max time capping

      #this dataframe must span to the end of the simulation or Hydrus error
      # if it stops early, then warn and overwrite final value
      if(max(atmos.df$time) < t1){
        print("atmos.df stops before the end of the simulation. Its last row has been extended to tMax.")
        atmos.df$time[nrow(atmos.df)] <- t1
      }


      #column checks
      has.time <- any(names(atmos.df) == "time")
      if(!has.time){ stop("atmos.df must have a 'time' column.") }
      has.dat <- any(names(atmos.df) %in% c("Prec","rRoot", "rSoil", "PET"))
      if(!has.dat){  stop("atmos.df must have at least one of these columns: Prec, rRoot, rSoil, PET.") }
      #convert PET and drop it
      has.pet <- any(names(atmos.df) == "PET")
      if(has.pet){
        if(trans.pc < 0 | trans.pc > 1){ stop("trans.pc must be between 0 and 1. it is a decimal percent.")}
        rs.cols <- which(names(atmos.df) %in% c("rRoot", "rSoil"))
        if(len(rs.cols)>0){ atmos.df <- atmos.df[, -rs.cols] } #rRoot and rSoil are ignored, and set again below by trans.pc
        atmos.df$rRoot <- atmos.df$PET * trans.pc
        atmos.df$rSoil <- atmos.df$PET * (1-trans.pc)
        atmos.df <- atmos.df[,-which(names(atmos.df)=="PET")] #drop PET now that it is converted
      }
    }

    #surface wetness (ponding) limit
    if(is.null(hCritS)){
      warning("hCritS should be set explicitly when setting an atmospheric boundary condition. It has been assumed as 0, meaning surface ponding cannot occur.")
      hCritS <- 0
    }
  } #end settings for atmospheric bc

  #if !atmos, then a top boundary condition must be given
  if(!atmos){
    if(is.null(top.bc.type)){ stop("top.bc.type must be given because atmos = FALSE.") }
    if(is.null(top.bc.value)){ stop("top.bc.value must be given because atmos = FALSE.") }
    if(!top.bc.type %in% c("ch", "cf", "vh", "vf")){ stop("top.bc.type must be 'ch', 'cf', 'vh', or 'vf'") }
    if(top.bc.type %in% c("ch","cf")){
      if(!is.vector(top.bc.value)){ stop("Constant head (ch) or flux (cf) top boundary type was given, so 'top.bc.value' must be numeric with length = 1") }
      if(length(top.bc.value) != 1){ stop("Constant head (ch) or flux (cf) top boundary type was given, so 'top.bc.value' must be numeric with length = 1") }

    } else {
      #else this is a variable boundary and dataframe needs to be checked

      if(!is.data.frame(top.bc.value)){ stop("Variable head (vh) or flux (vf) top boundary type was given, so 'top.bc.value' must be a dataframe") }

      #check columns
      if(!any(names(top.bc.value) == "time")){ stop("top.bc.value must have a 'time' column")}
      has.dat <- any(names(top.bc.value) %in% c("head", "flux"))
      if(!has.dat){  stop("top.bc.value must have at least one of these columns: head, flux") }
      #head at the top is named hT in ATMOSPH.IN but there is no equiv for flux at the top (e.g. rT).
      # therefore, a flux at the top can only be treated as precipitation and needs renaming to Prec
      top.bc.value <- top.bc.value %>%
        dplyr::rename(dplyr::any_of(c("Prec" = "flux", "hT" = "head")))

      #cap the maximum time
      if(max(top.bc.value$time) > t1){
        print("top.bc.value needed to be cut off at the end time defined in SELECTOR.IN (tMax). Double check your data.")
        if(all(top.bc.value$time) > t1){ #if all are higher, then keep only last row
          top.bc.value <- top.bc.value[nrow(top.bc.value),]
          top.bc.value$time <- t1
        } else {
          #else only some are higher. drop all but the first too-high row
          too.high <- which(top.bc.value$time > t1)
          too.high <- too.high[1:(length(too.high)-1)] #keep the last one that was too high
          top.bc.value <- top.bc.value[-too.high,]
          top.bc.value$time[nrow(top.bc.value)] <- t1
        }
      } #end max time capping

      #this dataframe must span to the end of the simulation or Hydrus error
      # if it stops early, then warn and overwrite final value
      if(max(top.bc.value$time) < t1){
        print("top.bc.value stops before the end of the simulation. Its last row has been extended to tMax.")
        top.bc.value$time[nrow(top.bc.value)] <- t1
      }

    } #end checks on top.bc.type and .value
  } #end !atmos section

  if(FreeD){
    #as above with atmos, need to fill bot.bc to pass through data prep sections (NULL not allowed)
    # if FreeD, then bot.bc will be managed by Hydrus and I will set it here so it isn't NULL
    bot.bc.type <- "FreeD"

    #no data prep needed for bot.bc.value, because this object will be ignored. if FreeD, then Hydrus manages it all. no inputs are given.
  }

  #if !FreeD, then check bot.bc.type and .value
  if(!FreeD){
    if(is.null(bot.bc.type)){ stop("bot.bc.type must be given because FreeD = FALSE.") }
    if(is.null(bot.bc.value)){ stop("bot.bc.value must be given because FreeD = FALSE.") }
    if(!bot.bc.type %in% c("ch", "cf", "vh", "vf")){ stop("bot.bc.type must be 'ch', 'cf', 'vh', or 'vf'") }
    if(bot.bc.type %in% c("ch","cf")){
      if(!is.vector(bot.bc.value)){ stop("Constant head (ch) or flux (cf) bottom boundary type was given, so 'bot.bc.value' must be numeric with length = 1") }
      if(length(bot.bc.value) != 1){ stop("Constant head (ch) or flux (cf) bottom boundary type was given, so 'bot.bc.value' must be numeric with length = 1") }
    } else {
      if(!is.data.frame(bot.bc.value)){ stop("Variable head (vh) or flux (vf) bottom boundary type was given, so 'bot.bc.value' must be a dataframe") }

      #check columns
      if(!any(names(bot.bc.value) == "time")){ stop("bot.bc.value must have a 'time' column")}
      has.dat <- any(names(bot.bc.value) %in% c("head", "flux"))
      if(!has.dat){  stop("bot.bc.value must have at least one of these columns: head, flux") }
      #head at the top is named hT in ATMOSPH.IN but there is no equiv for flux at the top (e.g. rT).
      # therefore, a flux at the top can only be treated as precipitation and needs renaming to Prec
      bot.bc.value <- bot.bc.value %>%
        dplyr::rename(dplyr::any_of(c("rB" = "flux", "hB" = "head")))

      #cap the maximum time
      if(max(bot.bc.value$time) > t1){
        print("bot.bc.value needed to be cut off at the end time defined in SELECTOR.IN (tMax). Double check your data.")
        if(all(bot.bc.value$time) > t1){ #if all are higher, then keep only last row
          bot.bc.value <- bot.bc.value[nrow(bot.bc.value),]
          bot.bc.value$time <- t1
        } else {
          #else only some are higher. drop all but the first too-high row
          too.high <- which(bot.bc.value$time > t1)
          too.high <- too.high[1:(length(too.high)-1)] #keep the last one that was too high
          bot.bc.value <- bot.bc.value[-too.high,]
          bot.bc.value$time[nrow(bot.bc.value)] <- t1
        }
      } #end max time capping

      #this dataframe must span to the end of the simulation or Hydrus error
      # if it stops early, then warn and overwrite final value
      if(max(bot.bc.value$time) < t1){
        print("bot.bc.value stops before the end of the simulation. Its last row has been extended to tMax.")
        bot.bc.value$time[nrow(bot.bc.value)] <- t1
      }

    } #end checks on bot.bc.type and .value
  } #end !FreeD section

  #surface dryness limit = 1e5 cm
  if(is.null(hCritA)){
    hCritA <- if(l.unit == "mm"){ 1e5*10
      } else if(l.unit == "cm"){ 1e5
      } else if(l.unit == "m"){ 1e5/100 }
  }

  # end clean arguments section
  ####


  ###
  # prepare dataframe to become ATMOSPH.IN
  # - ATMOSPH.IN does not just hold atmospheric boudnary conditions, it holds any variable boundary condition data, including those for variable
  #     bottom head (hB column) and flux (rB column).
  # - in this section, ATMOSPH.IN is put together if needed from some combination of atmos.df, top.bc.value, and bot.bc.value.
  # -- in the case of a constant top boundary condition and either a constant bottom condition or FreeD = TRUE, then ATMOSPH.IN is not needed at all.
  if(atmos |
     top.bc.type %in% c("vh","vf") |
     bot.bc.type %in% c("vh","vf")){

    #in this section I will make a dataframe called df.var that holds any variable BCs

    #there are two main paths, depending on atmos
    if(atmos){

      # if atmos=T, then either it is simply passed on its own (because bot.bc is not variable), or it is combined with bot.bc
      if(bot.bc.type %in% c("vh","vf")){

        df.var <- dplyr::full_join(atmos.df,
                                   bot.bc.value,
                                   by = "time")

        #this needs some work if timesteps don't match.
        # if not matching then the join will induce NAs, which means that the next value needs to be carried upward to fill.
        # value carried upward because a value given at a particular timestep covers all previous times after the last given value.
        #there will never be a missing value at the end of the dataframe because the data checks to start the fn ensure that the
        # final row in both is at time = t1
        df.var <- df.var %>% tidyr::fill(tidyr::everything(),
                                         .direction = "up")

      } else {
        #else atmos.df passed on its own
        df.var <- atmos.df
      }


    } else {
      #else !atmos, which means that either top.bc or bot.bc are passed on their own (if only one of them is variable), or they are combined because
      # both are variable.

      v.type <- which(sapply(c(bot.bc.type, top.bc.type), function(x){ grepl("v", x) }))
      if(length(v.type) == 2){

        df.var <- dplyr::full_join(bot.bc.value,
                                   top.bc.value,
                                   by = "time")

        #fill NAs if timesteps don't match.
        # if not matching then the join will induce NAs, which means that the next value needs to be carried upward to fill.
        # value carried upward because a value given at a particular timestep covers all previous times after the last given value.
        #there will never be a missing value at the end of the dataframe because the data checks to start the fn ensure that the
        # final row in both is at time = t1
        df.var <- df.var %>% tidyr::fill(tidyr::everything(),
                                         .direction = "up")

      } else {
        if(v.type == 1){ #then it is bottom only

          df.var <- bot.bc.value

        } else { #else it is top only

          df.var <- top.bc.value

        } #end else
      } #end else only one type
    } #end else, !atmos

    #some formatting applies, no matter the source dataframes
    # - time column is always named tAtm
    df.var <- df.var %>% dplyr::rename(tAtm = time)
    # - if this dataframe exists, hCritA seems to always be part of it
    df.var$hCritA <- hCritA #same value for all times, as currently coded does not allow it to vary
    # - only permitted columns are allowed
    df.var <- df.var %>%
      dplyr::select(dplyr::any_of(c("tAtm", "Prec", "rSoil", "rRoot", "hT", "hB", "rB", "hCritA")))



    #pass this to fn to write the file
    write.atmosph.in(project.path = project.path,
                     atm.bc.data = df.var,
                     hCritS = hCritS)
    # also need to pass correct kodes etc to SELECTOR
    # need to put comments in run.H1D.sim that it isn't set up, and that any sim with less than 960 print times and variable BC times
    #   should be run directly. otherwise fn needw work. specific work required are to override the flat deltaT. and that requires some way of saving
    #   the initial too-long versions of ATMOSPH and SELECTOR PrintTimes so that their irregular interval times can be pulled into the correct iteration.


  } #end if section around making ATMOSPH.IN


  #write the other boundary conditions, which is done whether they are variable or not
  # therefore all flux types are run through here.
  write.bc.top(project.path,
               atmos = atmos,
               constant.bc = top.bc.type %in% c("cf","ch"),
               bc.type = ifelse(top.bc.type %in% c("cf", "vf", "atmos"), "flux", "head"),
               bc.value = ifelse(is.vector(top.bc.value), top.bc.value, ""))

  write.bc.bottom(project.path,
                  FreeD = FreeD,
                  constant.bc = bot.bc.type %in% c("cf","ch"),
                  bc.type = ifelse(bot.bc.type %in% c("cf", "vf"), "flux", "head"),
                  bc.value = ifelse(is.vector(bot.bc.value), bot.bc.value, ""))

} #end fn
