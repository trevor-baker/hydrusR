#' Creates a new hydrus project folder with "HYDRUS1D.DAT", "SELECTOR.IN" and "DESCRIPT.TXT" files
#'
#' @param project.name Name of the project
#' @param parent.dir  Path to the project folder, where a folder with project.name will be saved
#' @param processes Main processes (e.g., WaterFlow, SoluteTransport etc) to be simulated
#' @param TimeUnit Simulation time unit information (default = days). permitted: seconds, minutes, hours, days, years.
#' @param SpaceUnit Vertical spatial unit (decault = cm). permitted: mm, cm, m
#' @param PrintTimes Time levels at which the outputs should be printed. These are the times that willbe output in the Nod_Inf.out file among others.
#' @param times Time settings. Named vector: time.range1, time.range2, dt, dtMin, dtMax, DMul1, DMul2, ItRange1, ItRange2, print.step. See
#' ?hydrusR::write.time.settings for details.
#' @param geometry Profile geometry info. Named vector: ProfileDepth, NumberOfNodes, ObservationNodes [number of obs nodes]
#' @param soil.para List of soil hydraulic parameters, one entry per material to be included in simulation. See ?hydrusR::write.hydraulic.para for details.
#' @param soil.model integer, 0-7. identifies the soil model to be used. 0 = standard van Genuchten-Mualem. See ?hydrusR::write.hydraulic.para for details.
#' @param soil.hys include hysteresis in simulation? integer. 0 = No, 1 = Yes.
#' @param soil.sub integer, length = 1. how many subregions (aka "Lay" in PROFILE.DAT) does this profile have? the number of subregions for
#' mass balance calculations.
#' @param soil.slope numeric, length = 1. slope angle in degrees.
#' @param rwu.stress Root water uptake stress parameters. Named vector: model, comp, P0, P2H, P2L, P3, POptm, r2H, r2L. See ?hydrusR::write.rwu.stress for details.
#' @param overwrite if TRUE, function will not ask to overwrite the project if it already exists. careful.
#' @author Subodh Acharya <https://github.com/shoebodh>; Trevor Baker <tbaker@iegconsulting.com>
#' @export
#' @examples
#'
#' create.H1D.project(project.name = "testproj2", parent.dir = parent_dir, discription = NULL,
#'                    SpaceUnit = "cm", TimeUnit = "days", PrintTimes = 1,
#'                    processes = c(WaterFlow = T, SoluteTransport = F, RootWaterUptake = F,
#'                    RootGrowth = F, Unsatchem = F, HP1 = F, EquillibriumAdsorption = F),
#'                    initial.cond = c(NumberOfSolutes = 0, InitialCondition = 0),
#'                    times = c(time.range1 = 0, time.range2 = 2400,
#'                              dt = 1e-3, dtMin = 1e-6, dtMax = 1,
#'                              DMul1 = 0.7, DMul2 = 1.3, ItRange1 = 3, ItRange2 = 7,
#'                              print.step = 10),
#'                    geometry = c(ProfileDepth = 200, NumberOfNodes = 4,
#'                                 ObservationNodes = 0),
#'                    soil.para = soil_para, soil.model = 0, soil.hys = 0, soil.slope = 0, soil.sub = 1,
#'                    rwu.stress = c(model = 0, comp = 1, P0 = -10, P2H = -200, P2L = -800, P3 = -8000,
#'                                   POptm = -25, r2H = 0.5, r2L = 0.1) )

create.H1D.project <- function(project.name,
                               parent.dir,
                               description = NULL,
                               TimeUnit = "days",
                               SpaceUnit = "cm",
                               PrintTimes = 1,
                               processes = c(WaterFlow = T, RootWaterUptake = F),
                               geometry,
                               soil.para,
                               soil.model = 0,
                               soil.hys = 0,
                               soil.slope = 0,
                               soil.sub,
                               rwu.stress = c(model = 0, comp = 1,
                                              P0 = -10, P2H = -200, P2L = -800, P3 = -8000,
                                              POptm = -25,
                                              r2H = 0.5, r2L = 0.1),
                               initial.cond,
                               overwrite = FALSE, ...){

  # #list all possible args and named elements of args
  # # - this isn't used anywhere but could be helpful to understand file structure etc.
  # all_args = c("WaterFlow", "SoluteTransport", "RootWaterUptake",
  #              "RootGrowth", "Unsatchem", "HP1", "EquillibriumAdsorption",
  #              "NumberOfSolutes", "InitialCondition", "geometry",
  #              "soil.para", "soil.model", "soil.hys",
  #              "project.name", "parent.dir", "description",
  #              "TimeUnit", "SpaceUnit", "PrintTimes")

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
        dir.create(project_path)
      } else {
        stop("HYDRUS1D project not created\n")
      }
    } else { #else arg says to overwrite without asking
      cat("project_path automatically overwritten due to 'overwrite' argument.\n")
      unlink(project_path, recursive = TRUE, force = TRUE)
      dir.create(project_path)
    }

  } else {
    dir.create(project_path)
  }



  #process arguments into a vector
  args_vec0 <- as.list(match.call()) #renamed as 0 for debug
  #
  #pull out the soil arguments - they are dealt with separately
  soil.args <- which(grepl("soil\\.",names(args_vec0)))
  if(length(soil.args)>0){
    args_vec0 <- args_vec0[-soil.args]
  }
  #add soil-related arg for material numbers
  args_vec0$MaterialNumbers <- length(soil.para) #number of list entries is number of materials
  #
  #pull out the rwu.stress args to be dealt with separately
  rwu.args <- which(grepl("rwu\\.",names(args_vec0)))
  if(length(rwu.args)>0){
    args_vec0 <- args_vec0[-rwu.args]
  }
  #
  #pull out the time args to be dealt with separately
  time.args <- which(grepl("times\\.",names(args_vec0)))
  if(length(time.args)>0){
    args_vec0 <- args_vec0[-time.args]
  }
  #
  #deal with remaining arguments
  args_vec = lapply(args_vec0[-1], FUN = function(x) unlist(x))
  # args_vec = unlist(unclass(args_vec))
  args_vec = do.call("c", args_vec) #this sets the values properly
  args_vec = ifelse(args_vec == TRUE, 1, args_vec) #convert logicals to numeric to match Hydrus input format
  args_vec = ifelse(args_vec == FALSE, 0, args_vec)

  names(args_vec) = gsub("processes.", "", names(args_vec), fixed = TRUE) #edit names to those expected by Hydrus
  names(args_vec) = gsub("geometry.", "", names(args_vec), fixed = TRUE)
  names(args_vec) = gsub("initial.cond.", "", names(args_vec), fixed = TRUE)

  #convert profile depth into sci format wanted by Hydrus
  args_vec["ProfileDepth"] <- toupper(format2sci(as.numeric(args_vec["ProfileDepth"]),
                                                 ndec = 2, power.digits = 3))



  args_names <- names(args_vec)
  #keep only the args that get passed to HYDRUS1D.DAT - these 4 do not
  h1d_args_names = args_names[!(args_names %in% c("project.name", "parent.dir", "description","overwrite"))]


  ######################
  #create HYDRUS1D.DAT file
  hydrus1d_template = system.file("templates/HYDRUS1D.DAT", package = "hydrusR")
  h1d_dat = readLines(hydrus1d_template, n = -1L, encoding = "unknown")

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
  selector_data[unit_lines] = c(SpaceUnit, TimeUnit)

  #update NMat, NLay, and Slope (CosAlpha)
  nmat <- length(soil.para) #of list elements is number of materials
  slp <- cos(pracma::deg2rad(soil.slope))
  nmat_ind <- grep("NLay", selector_data)
  nmat_lines = nmat_ind + 1
  nmat_vals <- sapply(list(nmat, soil.sub, slp), as.character)
  fmt_nmat <- c("%3s", "%8s", "%8s")
  nmat_vals_fmt <- paste(sprintf(fmt_nmat, nmat_vals), collapse = "")
  selector_data[nmat_lines] <- nmat_vals_fmt


  #write updates SELECTOR.IN
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
  write.time.settings(time.range = c(times["time.range1"],times["time.range2"]),
                      dt = times["dt"],
                      dtMin = times["dtMin"],
                      dtMax = times["dtMax"],
                      DMul = c(times["DMul1"], times["DMul2"]),
                      ItRange = c(times["ItRange1"],times["ItRange1"]),
                      print.step = times["print.step"])

  #######
  #add soil params to SELECTOR.IN
  write.hydraulic.para(project.path = project_path,
                       model = soil.model,
                       hysteresis = soil.hys,
                       para = soil.para)

  #######
  #add root water stress params to SELECTOR.IN
  write.rwu.stress(project.path,
                   model = rwu.stress["model"],
                   compensated.uptake = rwu.stress["comp"],
                   P0 = rwu.stress["P0"], P2H = rwu.stress["P2H"], P2L = rwu.stress["P2L"], P3 = rwu.stress["P3"],
                   POptm = rep(rwu.stress["POptm"], length(soil.para)), #replicate once per material
                   r2H = rwu.stress["r2H"], r2L = rwu.stress["r2L"])


  cat("New HYDRUS-1D project created in", project_path, "...\n")

} #end fn

