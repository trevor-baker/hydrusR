#' Creates a new hydrus project folder with "HYDRUS1D.DAT", "SELECTOR.IN" and "DESCRIPT.TXT" files
#'
#' @param project.name Name of the project
#' @param parent.dir  Path to the project folder, where a folder with project.name will be saved.
#' @param processes named vector, logical. Denotes the main processes to be simulated: "WaterFlow", "SoluteTransport",
#' "HeatTransport", "CO2Transport", "EquilibriumAdsorption", "RootWaterUptake", "RootGrowth", "Unsatchem", "HP1". Documented in Hydrus manual,
#' Table B.5. All should be FALSE except WaterFlow and, depending on your use, RootWaterUptake. Only the TRUE elements need to be listed, all others are
#' assumed to be FALSE. These are assigned to the Main section of HYDRUS1D.DAT.
#' @param units named vector giving units for this project, length = 2 (TimeUnit, SpaceUnit), each is a character string.  These are assigned to the
#' Main section of HYDRUS1D.DAT. \cr
#' TimeUnit: your simulation time unit (default = "hours"). permitted: "seconds", "minutes", "hours", "days", "years". \cr
#' SpaceUnit: your simulation spatial (length) unit (default = "cm"). permitted: "mm", "cm", "m".
#' @param profile information about the soil profile. Named vector "SubregionNumbers", value is integer length = 1, telling how many subregions for
#' mass balances are included in this simulation. These are assigned to the Main section of HYDRUS1D.DAT. This formerly held a value "MaterialNUmbers" but
#' it is now calculated from the soil information given in soil.df or soil.para.
#' @param solutes Named vector. Not currently set up for solute simulations. This argument can be omitted and defaults will be set without soute modelling.
#' vector names: "NumberOfSolutes", "MobileImmobile", "SolutionConc", "AdsorbedConc", "PrecipConc".  These are assigned to the Main section of HYDRUS1D.DAT.
#' @param setup Named vector, each is integer. These are assigned to the Main section of HYDRUS1D.DAT. These can be omitted and PrintTimes will be
#' calculated from data in the 'times' argument, while InitialCondition's default of 0 (pressure head) will be assumed \cr
#' PrintTimes = integer, length 1. how many times will results be printed to output files such as Nod_Inf.OUT? \cr
#' InitialCondition = 0 or 1. Whether the water flow initial condition is specified in terms of pressure heads (0) or water contents (1).
#' @param geometry Profile geometry info. Named vector: ProfileDepth, NumberOfNodes (number of discrete layers, i.e. rows in PROFILE.DAT),
#' ObservationNodes (number of obs nodes). These are assigned to the Profile section of HYDRUS1D.DAT.
#' @param grid Named vector. Not used. These seem to be Hydrus GUI settings. This argument can be omitted. Vector names: "GridVisible", "SnapToGrid",
#' "ProfileWidth","LeftMargin","GridOrgX","GridOrgY","GridDX","GridDY". These would be assigned to the Profile section of HYDRUS1D.DAT.
#' @param times Time settings. Named list: time.range1, time.range2, dt, dtMin, dtMax, DMul1, DMul2, ItRange1, ItRange2, print.step, print.at. See
#' ?hydrusR::write.time.settings for details. These are the settings from the GUI sections for Time Information-Time Discretization and Water Flow-Iteration
#' Criteria. These control iterations, time steps, run times, etc, and get written to Block C of SELECTOR.IN.
#' @param sim simulation settings. Named list: MaxIt, TolTh, TolH, TopInf, BotInf, WLayer, KodTop, KodBot, InitCond, qGWLF, FreeD, SeepF,
#' DrainF, hSeep. See ?hydrusR::write.sim.settings for details.
#' @param root Root water uptake stress parameters. Named list: model, comp, P0, P2H, P2L, P3, POptm, r2H, r2L. See ?hydrusR::write.rwu.stress for
#' details.
#' @param soil.df dataframe of theta-h-K values. four columns: 'theta', 'h' 'K' and 'id'. 'id' is an integer from 1:(count of soil materials). h
#' should span at least from -1e-2 to -1e5 cm. All of these values must be in project units, which by default are cm and hours, but note that ROSETTA
#' outputs and IEGsoil functions work in cm/day and require conversion prior to being assigned as 'soil.df'.
#' @param soil.para List of soil hydraulic parameters, one entry per material to be included in simulation. if 'soil.df' is given, this is ignored. See ?hydrusR::write.hydraulic.para for details.
#' @param soil.model integer, 0-7 or 10. identifies the soil model to be used. 0 = standard van Genuchten-Mualem. 10 = from lookup tables (soil.df).
#' Other options not currently enabled. See ?hydrusR::write.hydraulic.para for details.
#' @param soil.hys include hysteresis in simulation? integer. 0 = No, 1 = Yes.
#' @param soil.sub integer, length = 1. how many subregions (aka "Lay" in PROFILE.DAT) does this profile have? the number of subregions for
#' mass balance calculations.
#' @param soil.slope numeric, length = 1. slope angle in degrees.
#' @param overwrite if TRUE, function will not ask to overwrite the project if it already exists. careful.
#' @author Subodh Acharya <https://github.com/shoebodh>; Trevor Baker <tbaker@iegconsulting.com>
#' @export
#' @examples#'
#' create.H1D.project(project.name = "testproj2", parent.dir = parent_dir, description = NULL,
#'                    processes = c(WaterFlow = T, SoluteTransport = F, RootWaterUptake = F,
#'                                  RootGrowth = F, Unsatchem = F, HP1 = F, EquilibriumAdsorption = F),
#'                    units = c(SpaceUnit = "cm", TimeUnit = "hours"),
#'                    profile = c(SubregionNumbers = 1),
#'                    setup = c(PrintTimes = 1, InitialCondition = 0),
#'                    geometry = c(ProfileDepth = 200, NumberOfNodes = 4,
#'                                 ObservationNodes = 0),
#'                    times = list(time.range1 = 0, time.range2 = 2400,
#'                                 dt = 1e-3, dtMin = 1e-6, dtMax = 1,
#'                                 DMul1 = 0.7, DMul2 = 1.3, ItRange1 = 3, ItRange2 = 7,
#'                                 print.step = 10),
#'                    soil.para = soil_para, #vG params created prior
#'                    soil.model = 0, soil.hys = 0, soil.slope = 0,
#'                    root = list(model = 0, comp = 1, P0 = -10, P2H = -200, P2L = -800, P3 = -8000,
#'                                POptm = -25, r2H = 0.5, r2L = 0.1),
#'                    sim = list(MaxIt = 100, TolTh = 0.001, TolH = 1,
#'                               TopInf = TRUE, BotInf = FALSE, WLayer = TRUE,
#'                               KodTop = -1, KodBot = -1, InitCond = FALSE,
#'                               qGWLF = FALSE, FreeD = TRUE, SeepF = FALSE,
#'                               DrainF = FALSE, hSeep = 0))

create.H1D.project <- function(project.name,
                               parent.dir,
                               description = NULL,
                               #HYDRUS1D.DAT  Main section
                               processes = c(WaterFlow = T, RootWaterUptake = F),
                               units = c(TimeUnit = "days", SpaceUnit = "cm"),
                               profile = c(SubregionNumbers = 1),
                               solutes,
                               setup = c(PrintTimes = -999, InitialCondition = 0), #PrintTimes will be calculated from info in 'times'
                               #HYDRUS1D.DAT  Profile section
                               geometry = c(ProfileDepth = 200, NumberOfNodes = 4,
                                            ObservationNodes = 0),
                               grid,
                               #other arguments for SELECTOR.IN
                               times = list(time.range1 = 0, time.range2 = 2400,
                                            dt = 1e-3, dtMin = 1e-6, dtMax = 1,
                                            DMul1 = 0.7, DMul2 = 1.3, ItRange1 = 3, ItRange2 = 7,
                                            print.step = 10, print.at = NULL),
                               sim = list(MaxIt = 100, TolTh = 0.001, TolH = 1, lScreen = FALSE,
                                          TopInf = TRUE, BotInf = FALSE, WLayer = TRUE,
                                          KodTop = -1, KodBot = -1, InitCond = FALSE,
                                          qGWLF = FALSE, FreeD = TRUE, SeepF = FALSE,
                                          DrainF = FALSE, hSeep = 0),
                               root = list(model = 0, comp = 1,
                                           P0 = -10, P2H = -200, P2L = -800, P3 = -8000,
                                           POptm = -25, r2H = 0.5, r2L = 0.1),
                               #soil settings
                               soil.para = list( list(thr = 0.05, ths = 0.4, alpha = 0.3, n = 1.3, Ks = 5, l = 0.5) ), #one fake material for format example
                               soil.df = data.frame(id = 1, h = c(-1e-3,-1e5), theta = c(0.4, 0.05), K = c(5,1e-8) ), #wet and dry endpoints showing proper df format.
                               soil.model = 0,
                               soil.hys = 0,
                               soil.slope = 0,
                               #don't ask to overwrite previous
                               overwrite = FALSE, ...){


  # # #list all possible args and named elements of args
  # # # - this isn't used anywhere but could be helpful to understand file structure etc.
  # all_args = list(
  #   #metadata for setup
  #   "project.name", "parent.dir", "description",
  #   #Soil settings
  #   "soil.para", "soil.model", "soil.hys", "soil.slope",
  #   #Time settings
  #   times = c("time.range1", "time.range2", "dt", "dtMin", "dtMax",
  #             "DMul1", "DMul2", "ItRange1", "ItRange2", "print.step"),
  #   #simulation settings
  #   sim = c("MaxIt", "TolTh", "TolH", "TopInf", "BotInf", "WLayer", "lScreen",
  #            "KodTop", "KodBot", "InitCond", "qGWLF", "FreeD", "SeepF", "DrainF", "hSeep"),
  #   #root stress settings
  #   root = c("model", "comp", "P0", "P2H", "P2L", "P3", "POptm", "r2H", "r2L"),
  #   #HYDRUS1D.DAT - Main section
  #   main = list(processes = c("WaterFlow", "SoluteTransport", "HeatTransport", "CO2Transport",
  #                             "EquilibriumAdsorption", "RootWaterUptake", "RootGrowth",
  #                             "Unsatchem", "HP1"),
  #               units = c("SpaceUnit","TimeUnit"),
  #               profile = c("MaterialNumbers", "SubregionNumbers"), #material numbers doesn't need to be set. it is taken from length(soil.para)
  #               solute = c("NumberOfSolutes", "MobileImmobile", "SolutionConc","AdsorbedConc", "PrecipConc"),
  #               setup = c("PrintTimes","InitialCondition")),
  #   #HYDRUS1D.DAT - Profile section
  #   profile = list(geometry = c("NumberOfNodes", "ProfileDepth","ObservationNodes"),
  #                  grid = c("GridVisible","SnapToGrid","ProfileWidth","LeftMargin",
  #                           "GridOrgX","GridOrgY","GridDX","GridDY")))



  #set up paths and file names
  project_path = file.path(parent.dir, project.name)
  descript_file = file.path(project_path, "DESCRIPT.TXT")
  h1ddat_file = file.path(project_path, "HYDRUS1D.DAT")
  #set warning message
  disp_msg = paste("Folder", project.name, "already exists. All files will be deleted.Proceed? y/n \n")

  if(dir.exists(project_path)) {

    if(!overwrite){
      dir_answer = readline(prompt = disp_msg)
      dir_answer = substr(toupper(dir_answer), start = 1, stop = 1)

      if(dir_answer == "Y") {
        unlink(project_path, recursive = TRUE, force = TRUE)
        dir.create(project_path, showWarnings = F)
      } else {
        stop("HYDRUS1D project not created")
      }
    } else { #else arg says to overwrite without asking
      if(sim[["lScreen"]]){
        cat("project_path automatically overwritten due to 'overwrite' argument.\n")
      }

      unlink(project_path, recursive = TRUE, force = TRUE)
      dir.create(project_path, showWarnings = F)
    }

  } else {
    dir.create(project_path, showWarnings = F)
  }


  print("create.H1D.project: improvements needed so that not all arguments need to be explicit in the function call")
  #as written, for an argument to be passed properly, it needs to be an explicit part of the call. if not in the call, its default value
  #   is not transferred onwards. this could be accomplished using formals() to get the defaults, check which are in the call, and combine to
  #   fill out a complete list while giving the call args priority (default fills missing). this is more time than I want to spend now so I will
  #   just call everything but it makes for clunky function use and causes defaults to not be reliably used, which really should be improved.
  # a few notes and start in the right direction:
  # names(as.list( formals(hydrusR::create.H1D.project) )) #this tells the full list of top-level arguments (but not named elements of vector and list args)
  # as.list( formals(hydrusR::create.H1D.project) ) #this tells the values of the simple arguments (not the vectors and lists)
  # lapply(as.list( formals(hydrusR::create.H1D.project) ), names) #this tells the names within the vector and list args (need to drop leading "")
  # lapply(as.list( formals(hydrusR::create.H1D.project) ), as.list) #this gives the values of the vector and list args, but is not useful for the simple args.


  #calculate PrintTimes for inserting below
  # - this needs to depend on and agree with the data given in 'times'. The easiest way is to calc here:
  if(!is.null(times[["print.at"]])){
    setup[["PrintTimes"]] <- length(times[["print.at"]]) #it is the count of print.at times if they are given
  } else {
    #or it is the runtime divided by print.step if print.at is not given
    setup[["PrintTimes"]] <- floor( diff(c(times[["time.range1"]], times[["time.range2"]])) / times[["print.step"]])
  }

  #how many soil layers in this profile
  if(!is.null(soil.para)){
    nlayer <- length(soil.para)
  } else {
    nlayer <- ifelse(any(names(soil.df) == "id"), lu(soil.df$id), 1) #if no id column, assume 1 layer.
  }


  #process arguments into a vector
  args_vec0 <- as.list(match.call()) #renamed as 0 for debug
  #
  #need to pull out all of the arguments that are not part of HYDRUS1D.DAT. these are all dealt with separately. leave behind
  # a vector of arguments that will be fed into HYDRUS1D.DAT in the for(a in _) loop below.
  #pull out the soil arguments
  soil.args <- which(grepl("^soil",names(args_vec0)))
  if(length(soil.args)>0){
    args_vec0 <- args_vec0[-soil.args]
  }
  #pull out the root args
  root.args <- which(grepl("^root",names(args_vec0)))
  if(length(root.args)>0){
    args_vec0 <- args_vec0[-root.args]
  }
  #pull out the time args
  time.args <- which(grepl("^times",names(args_vec0)))
  if(length(time.args)>0){
    args_vec0 <- args_vec0[-time.args]
  }
  #pull out the simulation setting args to be dealt with separately
  sim.args <- which(grepl("^sim",names(args_vec0)))
  if(length(sim.args)>0){
    args_vec0 <- args_vec0[-sim.args]
  }


  args_list <- args_vec0[-1] #drop function name. args start at index 2
  args_eval <- NULL
  for(xx in 1:length(args_list)){
    eval0 <- eval.parent(args_list[[xx]])
    if(is.null(names(eval0))){
      names(eval0) <- names(args_list)[xx] #simple args (not part of vector or list) need to carry their argument name. it is needed below for assigning values
    } else {
      names(eval0) <- paste0(names(args_list)[xx],".",names(eval0)) #insert name of parent argument to match previous formatting
    }
    args_eval[[xx]] <- eval0
  } #end xx loop (cannot do this in lapply, eval.parent doesn't work.)
  args_vec <- do.call(c, args_eval)
    ## old method stopped working, wasn't able to get evaluated arguments from parent frame. In hindsight, I think
    ##   it is related to create.H1D.project now being housed inside of run.AWSC.sim so the call stack was different
    ##   than running creaet.H1D.proj as a standalone.
    # #deal with remaining arguments to make them into a named vector that can be subbed in the for(a in _) loop below.
    # args_vec = lapply(args_vec0[-1], FUN = function(x) unlist(x))
    # # args_vec = unlist(unclass(args_vec))
    # args_vec = do.call("c", args_vec) #this sets the values properly
  args_vec = ifelse(args_vec == TRUE, 1, args_vec) #convert logicals to numeric to match Hydrus input format
  args_vec = ifelse(args_vec == FALSE, 0, args_vec)


  #Manually edit a few args values that do not come from the function call.
  #sub in the updated PrintTimes value
  # - it won't be here because these are derived from match.call, whereas PrintTimes is set within this fn before here.
  args_vec["setup.PrintTimes"] <- as.character(setup[["PrintTimes"]])
  #add profile arg for material numbers
  args_vec["profile.MaterialNumbers"] <- as.character(nlayer) #number of list entries is number of materials


  #edit names to those expected by Hydrus. these all have prefixes of their arg names. remove prefixes.
  names(args_vec) <- gsub("^processes\\.", "", names(args_vec))
  names(args_vec) <- gsub("^initial.cond\\.", "", names(args_vec))
  names(args_vec) <- gsub("^units\\.", "", names(args_vec))
  names(args_vec) <- gsub("^profile\\.", "", names(args_vec))
  names(args_vec) <- gsub("^setup\\.", "", names(args_vec))
  names(args_vec) <- gsub("^geometry\\.", "", names(args_vec))
  names(args_vec) <- gsub("^grid\\.", "", names(args_vec))

  #convert profile depth into sci format wanted by Hydrus
  args_vec["ProfileDepth"] <- toupper(format2sci(as.numeric(args_vec["ProfileDepth"]),
                                                 ndec = 2, power.digits = 3))


  args_names <- names(args_vec)
  #keep only the args that get passed to HYDRUS1D.DAT - these 4 do not
  h1d_args_names = args_names[!(args_names %in% c("project.name", "parent.dir", "description", "overwrite"))]


  ######################
  #create HYDRUS1D.DAT file
  hydrus1d_template = system.file("templates/HYDRUS1D.DAT", package = "hydrusR")
  h1d_dat = readLines(hydrus1d_template, n = -1L, encoding = "unknown")

  #loop through the args prepared above and sub them into the file
  for(a in 1:length(h1d_args_names)){
        arg_a = h1d_args_names[a]
        arg_value = args_vec[arg_a]
        arg_index = grep(arg_a, h1d_dat)
        h1d_dat[arg_index] = paste0(arg_a, "=", arg_value)
  }
  #write it to file
  write(h1d_dat, file = h1ddat_file, append = FALSE)

  #note: the SubregionNumbers argument is edited in create.soil.profile


  ######################
  #write DESCRIPT.TXT file
  description = ifelse(is.null(description), #give description if there was none given
                       paste("project title:", project.name),
                       description)
  descript_vec = c("Pcp_File_Version=1", description)
  write(descript_vec, file = descript_file, append = FALSE)



  #####################
  #prepare SELECTOR.IN file
  selector_in = system.file("templates/SELECTOR.IN", package = "hydrusR")
  selector_data = readLines(selector_in, n = -1L, encoding = "unknown")

  #update units
  lunit_ind = grep("LUnit", selector_data)
  unit_lines = lunit_ind + 1:2
  selector_data[unit_lines] = c(as.character( args_vec["SpaceUnit"] ),
                                as.character( args_vec["TimeUnit"] ))

  #update NMat, NLay, and Slope (CosAlpha)
  nmat <- nlayer # number of list elements is number of materials
  soil.sub <- args_vec["SubregionNumbers"]
  slp <- cos(pracma::deg2rad(soil.slope))
  nmat_ind <- grep("NLay", selector_data)
  nmat_lines = nmat_ind + 1
  nmat_vals <- sapply(list(nmat, soil.sub, slp), as.character)
  fmt_nmat <- c("%3s", "%8s", "%8s")
  nmat_vals_fmt <- paste(sprintf(fmt_nmat, nmat_vals), collapse = "")
  selector_data[nmat_lines] <- nmat_vals_fmt

  #update lScreen - whether Hydrus should print to console
  lsc_ind <- grep("lScreen", selector_data)
  lsc_lines = lsc_ind + 1
  lsc_vals <- selector_data[lsc_lines]
  lsc_vals <- strsplit(lsc_vals, " ")
  lsc_vals <- lsc_vals[which(lsc_vals != "")]
  lsc_names <- selector_data[lsc_lines-1]
  lsc_names <- strsplit(lsc_names, " ")
  lsc_names <- lsc_names[which(lsc_names != "")]
  lsc_col <- which(lsc_names == "lScreen")
  lsc_vals[lsc_col] <- args_vec["lScreen"]

  lsc_vals <- sapply(list(nmat, soil.sub, slp), as.character)
  fmt_nmat <- c("%3s", "%8s", "%8s")
  nmat_vals_fmt <- paste(sprintf(fmt_nmat, nmat_vals), collapse = "")
  selector_data[nmat_lines] <- nmat_vals_fmt

  #write updates to SELECTOR.IN
  write(selector_data, file = file.path(project_path, basename(selector_in)), append = F)

  # TDB: commented this out because time is now set by its own functions below
  #####
  # timeinfo_ind = grep("*** BLOCK C", selector_data, fixed = TRUE)
  # timeinfo_data = selector_data[timeinfo_ind+2]
  #
  # timeinfo_split = unlist(strsplit(x = timeinfo_data, split = " "))
  # timeinfo_split = timeinfo_split[timeinfo_split != ""]
  # timeinfo_new = as.numeric(timeinfo_split)
  #
  # browser()
  #
  # names(timeinfo_split) = c("dt", "dtMin",  "dtMax", "DMul", "DMul2", "ItMin", "ItMax", "MPL")
  # names(timeinfo_new) = c("dt", "dtMin",  "dtMax", "DMul", "DMul2", "ItMin", "ItMax", "MPL")
  #
  # if(TimeUnit == "hours"){
  #   timeinfo_new[c("dt", "dtMin", "dtMax")] = 24*timeinfo_new[c("dt", "dtMin", "dtMax")]
  # } else if (TimeUnit == "minutes") {
  #   timeinfo_new[c("dt", "dtMin", "dtMax")] = 60*24*timeinfo_new[c("dt", "dtMin", "dtMax")]
  # } else if(TimeUnit == "seconds"){
  #   timeinfo_new[c("dt", "dtMin", "dtMax")] = 3600*24*timeinfo_new[c("dt", "dtMin", "dtMax")]
  # } else if(TimeUnit == "years") {
  #   timeinfo_new[c("dt", "dtMin", "dtMax")] = 1/365*timeinfo_new[c("dt", "dtMin", "dtMax")]
  # }
  #
  # timeinfo_new[c("dt", "dtMin", "dtMax")] = format2sci(timeinfo_new[c("dt", "dtMin", "dtMax")], ndec = 3, power.digits = 3)
  # fmt_space = c(12, 13, 12, 8, 8, 6, 6, 6)
  # fmt_vec = paste("%", fmt_space, "s", sep = "")
  # timeinfo_new_fmt = sprintf(fmt = fmt_vec, timeinfo_new)
  # timeinfo_new_str = paste(timeinfo_new_fmt, collapse = "")
  #
  # selector_data[timeinfo_ind + 2] = timeinfo_new_str
  #
  # write(selector_data, file = file.path(project_path, basename(selector_in)), append = F)


  ###
  # write time settings
  write.time.settings(project.path = project_path,
                      time.range = c(times[["time.range1"]],times[["time.range2"]]),
                      dt = times[["dt"]],
                      dtMin = times[["dtMin"]],
                      dtMax = times[["dtMax"]],
                      DMul = c(times[["DMul1"]], times[["DMul2"]]),
                      ItRange = c(times[["ItRange1"]],times[["ItRange2"]]),
                      print.step = times[["print.step"]],
                      print.at = times[["print.at"]])

  #######
  Sys.sleep(0.3)
  #add soil params to SELECTOR.IN
  write.hydraulic.para(project.path = project_path,
                       model = soil.model,
                       hysteresis = soil.hys,
                       para = soil.para,
                       vals = soil.df)

  #######
  Sys.sleep(0.3)
  #add root water stress params to SELECTOR.IN
  write.rwu.stress(project.path = project_path,
                   model = root[["model"]],
                   compensated.uptake = root[["comp"]],
                   P0 = root[["P0"]], P2H = root[["P2H"]], P2L = root[["P2L"]], P3 = root[["P3"]],
                   POptm = rep(root[["POptm"]], nlayer), #replicate once per material
                   r2H = root[["r2H"]], r2L = root[["r2L"]])

  ######
  Sys.sleep(0.3)
  #add simulation settings to SELECTOR.IN
  write.sim.settings(project.path = project_path,
                     MaxIt = sim[["MaxIt"]],
                     TolTh = sim[["TolTh"]], TolH = sim[["TolH"]],
                     TopInf = sim[["TopInf"]], BotInf = sim[["BotInf"]],
                     WLayer = sim[["WLayer"]], KodTop = sim[["KodTop"]], KodBot = sim[["KodBot"]],
                     InitCond = sim[["InitCond"]], qGWLF = sim[["qGWLF"]],
                     FreeD = sim[["FreeD"]], SeepF = sim[["SeepF"]],
                     DrainF = sim[["DrainF"]], hSeep = sim[["hSeep"]])

  if(sim[["lScreen"]]){
    cat("New HYDRUS-1D project created in", project_path, "...\n")
  }


} #end fn

