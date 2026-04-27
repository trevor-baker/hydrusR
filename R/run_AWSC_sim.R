#Developing a function to run my typical intended MSc/IEGsoil simulation
# - these are free draining, PET can be controlled by an argument, and they take tables of theta-h-K values as their inputs

#' @examples
#' df.val <- readRDS("calculation tools/awsc/r scripts/ieg awsc/under development/vG PTF/Hydrus/df_val.rds")
#' df.out <- run.AWSC.sim(project_path = "C:/Users/t/Documents/temp/example_lookup", df.val = df.val, return.df = TRUE)
#' df.out %>%
#'   filter(Depth %in% seq(0,-200,-20)) %>%
#'   ggplot(aes(x=Time, y = Moisture, color = Depth, group = Depth)) %>%
#'   geom_path() +
#'   scale_y_log10() +
#'   scale_x_log10()

run.AWSC.sim <- function(project_path = "C:/Users/t/Documents/temp/example_lookup",
                         hydrus.path = "C:/Program Files (x86)/PC-Progress/Hydrus-1D 4.xx",
                         SpaceUnit = "cm", ## Space units
                         TimeUnit = "hours", ## time units
                         df.val, #dataframe with theta, h, K, id columns.
                         depth_vec = seq(0,100,1),
                         mat_num = c(rep(1,50),rep(2,51)), #integer vector with same length as depth_vec. or a single integer if only one material.
                         startTime = 0,
                         endTime = 4800,
                         rwu = TRUE,
                         trans_pc = 1,
                         PET_mmd = 0,
                         Prec_mmd = 0,
                         head_init = -1,
                         hCritS = 0, #if > 0, ponding can occur up to that depth in project length units. if zero, then no ponding, and runoff occurs immediately.
                         root_depth = 100,
                         rBeta = 0, #0 = same root density at all rooting depths
                         WP = -8000, #wiling point. in project length units. -8000 cm is Pasture default for Feddes root stress model.
                         return.df = FALSE #return dataframe of results loaded from Nod_Inf.OUT
                         ){


  print.at <- unique( c( sort( as.vector(sapply(c(1,2,5), function(x){ x*10^seq(-3,0,1) })) ), #0.001,0.005,0.01, etc.
                         seq(10,1000,10),
                         seq(1000,1e5,100),
                         endTime) )
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
                                  dt = 1e-3, dtMin = 1e-6, dtMax = 1,
                                  DMul1 = 0.7, DMul2 = 1.3, ItRange1 = 3, ItRange2 = 7,
                                  print.at = print.at,
                                  print.step = NULL),
                     sim = list(MaxIt = 100, TolTh = 0.001, TolH = 1,
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

  hydrus.path <- "C:/Program Files (x86)/PC-Progress/Hydrus-1D 4.xx"
  call.H1D(project_path, hydrus.path = hydrus.path, show.output = TRUE)

  if(return.df){
    df.hyd <- read.nod_inf(project_path)
    return(df.hyd)
  } #else return nothing


} #end fn
