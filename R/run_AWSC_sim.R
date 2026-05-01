#' Run Hydrus simulation to generate data for plant-available water calculations
#'
#' Run simulations of a free-draining soil profile using tables of theta-h-K values as hydraulic function inputs.
#' @returns if return.df=TRUE then a dataframe loaded from Nod_Inf.OUT is returned if simulation is successful. If FALSE, nothing is returned to console.
#' @param
#' @param project_path path to your project
#' @param hydrus.path path to Hydrus exe files, e.g. "C:/Program Files (x86)/PC-Progress/Hydrus-1D 4.xx",
#' @param TimeUnit your simulation time unit (default = "hours"). permitted: "seconds", "minutes", "hours", "days", "years". \cr
#' @param SpaceUnit: your simulation spatial (length) unit (default = "cm"). permitted: "mm", "cm", "m".
#' @param df.val dataframe with theta, h, K, id columns. example here: read.csv( system.file("examples/df_val.csv", package = "hydrusR") ).
#' see ?write.mater.in() for more detials and examples
#' @param depth_vec numeric vector of depths, in SpaceUnit, for setting the nodes in the soil to be simulated. recommended spacing is 1-2 cm. e.g.,
#' if SpaceUnit = "cm", then depth_vec=seq(0,100,1)
#' @param mat_num integer vector with length(depth_vec)+1 (assign the +1 to the bottom material). valid values are 1:(lu(df.val$id)). if only one
#' material, then enter 1 or leave NULL and 1 will be filled for all rows.
#' @param startTime numeric, length = 1. starting time, in project TimeUnits. Usually this is 0.
#' @param endTime numeric, length = 1. ending time, in project TimeUnits.
#' @param dt numeric, length 1. initial timestep. default = 1e-3
#' @param dtMin numeric, length 1. minimum allowed timestep. default = 1e-6
#' @param dtMax numeric, length 1. maximum allowed timestep. default = 1.
#' @param MaxIt max iterations for solver. default = 100
#' @param TolTh theta tolerance in solver. default = 0.001
#' @param TolH head tolerance in solver. in project SpaceUnit. default = 1 cm.
#' @param lScreen logical. length = 1. Should Hydrus outputs be shown on console?
#' @param rwu logical, length = 1. root water uptake enbaled (TRUE) or disabled (FALSE)
#' @param root_depth numeric, length = 1. in SpaceUnit. How deep is rooting in this profile? only used if rwu = TRUE.
#' @param rBeta numeric, length 1. see ?write.root.dist for examples. 0 = same root density at all rooting depths. 0.962 = former default. values
#' should be either 0, or range from 0.9 to 0.99 for reasonable results. try: plot(1-rBeta^(rev(seq(0,root_depth,1)))
#' @param WP = wilting point. in project length units. -8000 cm is Pasture default for Feddes root stress model.
#' @param trans_pc numeric, length 1, value between 0 and 1. decimal percent for how much of PET should be assigned as potential transpiration. the
#' remainder (1-trans_pc) is assigned as soil evaporation. Default = 1, meaning all PET is from roots.
#' @param PET_mmd potential evapotranspiration rate, in mm/d (not in SpaceUnit/TimeUnit)
#' @param Prec_mmd precipitation rate, in mm/d (not in SpaceUnit/TimeUnit)
#' @param diurnal should PET be put on a diurnal pattern?
#' @param head_init numeric, length 1. must be < 0. initial pressure head for all nodes, in SpaceUnit. Do not set this to 0 or solver likely will fail. if you want saturation, set it just below, e.g. -0.1 cm.
#' @param hCritS numeric, length 1. maximum surface hydraulic head. if > 0, ponding can occur up to that depth in project length units. if zero, then no ponding, and runoff occurs immediately.
#' @param return.df return dataframe loaded from Nod_Inf.OUT. if FALSE, nothing is returned to console.
#' @author Trevor Baker <tbaker@iegconsuting.com>
#' @examples
#' df.val <- read.csv(system.file("examples/df_val.csv", package = "hydrusR"))
#' df.out <- run.AWSC.sim(project_path = "C:/Users/t/Documents/temp/example_lookup", df.val = df.val, mat_num = 1, return.df = TRUE)
#' #look at moisture over time in your results.
#' df.out %>%
#'   filter(Depth %in% seq(0,-200,-20)) %>%
#'   ggplot(aes(x=Time, y = Moisture, color = Depth, group = Depth)) %>%
#'   geom_path() +
#'   scale_y_log10() +
#'   scale_x_log10()
#'
#' #if you run the same simulation in Hydrus GUI, then you can compare results like this:
#'   library(tidyverse)
#'   df.hyd <- read.nod_inf("C:/users/t/Documents/Hydrus1D/Examples/Direct/R_example_lookup2") #your Hydrus GUI project
#'
#'   #bind both results together
#'   df.both <- bind_rows(df.out %>% mutate(origin = "R"),
#'                        df.hyd %>% mutate(origin = "GUI"))
#'
#'   #plot Head over time - should be identical - repeat with any  other parameter (Moisture, K, Flux,...)
#'   df.both %>%
#'     filter(Depth %in% seq(0,-200,-20)) %>%
#'     ggplot(aes(x = Time, y = -Head, color = origin,
#'                linetype = origin,
#'                group = origin)) +
#'     geom_line(alpha = 0.6, lwd = 1.8) +
#'     scale_x_log10() +
#'     scale_y_log10() +
#'     theme_bw() +
#'     facet_wrap(~Depth)
#' @export

run.AWSC.sim <- function(project_path = "C:/Users/t/Documents/temp/example_lookup",
                         hydrus.path = "C:/Program Files (x86)/PC-Progress/Hydrus-1D 4.xx",
                         SpaceUnit = "cm",
                         TimeUnit = "hours",
                         df.val,
                         depth_vec = seq(0,100,1),
                         mat_num = c(rep(1,50),rep(2,51)),
                         startTime = 0,
                         endTime = 4800,
                         dt = 1e-4,
                         dtMin = 1e-6,
                         dtMax = 1,
                         MaxIt = 100,
                         TolTh = 0.001,
                         TolH = 1,
                         lScreen = FALSE,
                         rwu = TRUE,
                         root_depth = 100,
                         rBeta = 0,
                         WP = -8000,
                         trans_pc = 1,
                         PET_mmd = 0,
                         Prec_mmd = 0,
                         diurnal = FALSE,
                         head_init = -1,
                         hCritS = 0,
                         return.df = FALSE ){

  # #sample df.val for debug
  # df.val <- read.csv(system.file("examples/df_val.csv", package = "hydrusR"))

  if(head_init == 0){
    head_init <- -0.1
    cat("head_init cannot be zero. It was changed to -0.1\n") }
  head_init <- -abs(head_init) #force to negative.

  project_path <- gsub("//", "/", gsub("\\\\", "/", project_path)) #ensure all are forward slashes
  parent_dir <- gsub("/[^/]*$", "/", project_path) #strip off project name
  project_name <- gsub(parent_dir, "", project_path) #keep only project name


  print.at <- unique( c( as.vector(sapply(c(1,2,5), function(x){ x*10^seq(-4,2,1) })), #0.0001,0.0002,0.0005,0.001,..., to 500
                         seq(100,1000,5),
                         seq(1000,10000,50),
                         seq(10000,1e5,1000),
                         endTime) )
  print.at <- sort(print.at)
  if(any(print.at > endTime)){
    print.at <- print.at[which(print.at <= endTime)]   #make sure it ends on time.
  }#end if

  ##Simulation settings
  maxIt <- 100
  ## atmosphere
  # Boundary conditions inputs
  atmos_bc <- TRUE #has atmosphere BC
  FreeD <- TRUE #free draining at bottom
  #if freeD is TRUE, then these for bottom are ignored:
  const_botbc = TRUE
  bot_bc_type = "flux" #"head" #this determines which of the below. either 'flux' or 'head'
  const_botFlux = 0.0 #in project units L/T
  const_botHead = 0 #in project units L
  #if atmos = TRUE, then these for top are ignored:
  const_topbc <- TRUE
  top_bc_type <- "flux"
  const_topFlux <- 0 #in project units L/T
  const_tophead <- 0 #in project units L

  # soil inputs
  profile_depth = max(depth_vec)

  #if not given, then fill all as same.
  if(is.null(mat_num)){
    mat_num <- rep(1, length(depth_vec))
  } else if(length(mat_num)==1){
    mat_num <- rep(1, length(depth_vec))
  }

  #subregion every time a material changes
  subregions <- u(c( sapply(u(mat_num), function(x){ max(depth_vec[which(mat_num == x)]) }),
                     max(depth_vec)))
  #obs_nodes at same locations
  obs_nodes <- subregions

  #get arguments for creating project from boundary condition inputs
  bc.args <- data.frame(TopInf = NA, KodTop = NA_integer_,
                        BotInf = NA, KodBot = NA_integer_,
                        WLayer = NA,
                        FreeD = FreeD)
  bc.args$TopInf <- atmos_bc | !const_topbc  #is the top BC variable (TRUE) or constant (FALSE) - atmos gets entered here as FALSE even though it can vary
  bc.args$BotInf <- if(FreeD){ FALSE } else { #is the bottom BC variable (TRUE) or constant (FALSE) - FreeD gets set as FALSE by Hydrus
    if(const_botbc){ FALSE } else {TRUE } }

  bc.args$KodTop <- if(atmos_bc){ -1 } else { #this is entered as -1 (constant) for simulations with atmos data, even if the flux (e.g. P or ET) is variable over time.
    if(const_topbc){
      #constants are (1), flux is -1 and head is +1
      if(top_bc_type == "flux"){ -1 } else { +1 }} else {
        #constants are (3), flux is -3 and head is +3
        if(top_bc_type == "flux"){ -3 } else { +3 }
      }
  }
  bc.args$KodBot <- if(FreeD){ -1 } else { #this is entered as -1 (constant) for simulations with free-draining profiles, even if the flux will vary as the profile drains.
    if(const_botbc){
      #constants are (1), flux is -1 and head is +1
      if(bot_bc_type == "flux"){ -1 } else { +1 }} else {
        #constants are (3), flux is -3 and head is +3
        if(bot_bc_type == "flux"){ -3 } else { +3 }
      }
  }
  bc.args$WLayer <- if(hCritS > 0){ TRUE } else { FALSE } #if positive head allowed at surface, then a water layer can build up without running off







  ####################################################
  # Create project, input files, and run Hydrus

  create.H1D.project(project.name = project_name,
                     parent.dir = parent_dir,
                     description = paste("TDB MSc:", project_name),
                     overwrite = T,
                     processes = c(WaterFlow = TRUE, RootWaterUptake = rwu),
                     units = c(TimeUnit = TimeUnit, SpaceUnit = SpaceUnit),
                     profile = c(SubregionNumbers = length(subregions)),
                     setup = c(InitialCondition = 0),
                     geometry = c(ProfileDepth = profile_depth,
                                  NumberOfNodes = length(depth_vec),
                                  ObservationNodes = length(obs_nodes)),
                     times = list(time.range1 = startTime, time.range2 = endTime,
                                  dt = dt, dtMin = dtMin, dtMax = dtMax,
                                  DMul1 = 0.7, DMul2 = 1.3, ItRange1 = 3, ItRange2 = 7,
                                  print.at = print.at,
                                  print.step = NULL),
                     sim = list(MaxIt = MaxIt, TolTh = TolTh, TolH = TolH, lScreen = lScreen,
                                TopInf = bc.args$TopInf, BotInf = bc.args$BotInf, WLayer = bc.args$WLayer,
                                KodTop = bc.args$KodTop, KodBot = bc.args$KodBot,
                                InitCond = FALSE, qGWLF = FALSE,
                                FreeD = FreeD,
                                SeepF = FALSE, DrainF = FALSE, hSeep = 0),
                     root = list(model = 0, comp = 1,
                                 P0 = -10, P2H = -200, P2L = -800, P3 = -abs(WP),
                                 POptm = -25, r2H = 0.5, r2L = 0.1,
                                 root_depth = root_depth,
                                 rBeta = rbeta),
                     soil.para = NULL,
                     soil.df = df.val,
                     soil.model = 10, #switch to 10 because that what Hydrus assignd for lookup table format
                     soil.hys = 0,
                     soil.slope = 0)

  ### create the soil profile (PROFILE.DAT) info
  create.soil.profile(project.path = project_path,
                      profile.depth = profile_depth,
                      depth.vec = depth_vec,
                      root.depth = root_depth,
                      rBeta = rBeta,
                      mat = mat_num,
                      Head = head_init,
                      obs.nodes = obs_nodes)

  df.atm <- prep.H1D.climate(project.path = project_path,
                             TimeUnit = TimeUnit, SpaceUnit = SpaceUnit,
                             endTime = endTime,
                             diurnal = diurnal,
                             Prec_mmd = Prec_mmd,
                             PET_mmd = PET_mmd,
                             trans.pc = trans_pc)

  create.bc(project.path = project_path,
            atmos = TRUE,
            atmos.df = df.atm,
            hCritS = hCritS,
            top.bc.type = NULL,
            top.bc.value = NULL,
            FreeD = TRUE,
            bot.bc.type = NULL,
            bot.bc.value = NULL)

  call.H1D(project_path, hydrus.path = hydrus.path, show.output = lScreen)

  if(return.df){
    df.hyd <- read.nod_inf(project_path)
    return(df.hyd)
  } #else return nothing


} #end fn
