#' Creates a new hydrus project folder with "HYDRUS1D.DAT" and "DISCRIPT.TXT" files
#'
#' @param project.name Name of the project
#' @param parent.dir  Path to the project folder, where a folder with project.name will be saved
#' @param processes Main processes (e.g., WaterFlow, SoluteTransport etc) to be simulated
#' @param TimeUnit Simulation time unit information (default = days). permitted: seconds, minutes, hours, days, years.
#' @param SpaceUnit Vertical spatial unit (decault = cm). permitted: mm, cm, m
#' @param PrintTimes Time levels at which the outputs should be printed. These are the times that willbe output in the Nod_Inf.out file among others.
#' @param geometry Profile geometry info (Depth, # of nodes and # of obs nodes)
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
#'                    geometry = c(ProfileDepth = 200, NumberOfNodes = 4,
#'                                 ObservationNodes = 0, MaterialNumber = 1, SubregionNumber = 1))

create.H1D.project <- function(project.name,
                               parent.dir,
                               description = NULL,
                               TimeUnit = "days",
                               SpaceUnit = "cm",
                               PrintTimes = 1,
                               processes = c(WaterFlow = T, RootWaterUptake = F),
                               geometry,
                               initial.cond,
                               overwrite = FALSE, ...){

  #list all possible args and named elements of args
  all_args = c("WaterFlow", "SoluteTransport", "RootWaterUptake",
               "RootGrowth", "Unsatchem", "HP1", "EquillibriumAdsorption",
               "NumberOfSolutes", "InitialCondition", "geometry",
               "project.name", "parent.dir", "description",
               "TimeUnit", "SpaceUnit", "PrintTimes")

  #set up paths and file names
  project_path = file.path(parent.dir, project.name)
  descript_file = file.path(project_path, "DESCRIPT.TXT")
  h1ddat_file = file.path(project_path, "HYDRUS1D.DAT")
  #give description if there was none given
  description = ifelse(is.null(description), paste("project title:", project.name), description)
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
  args_vec0 = as.list(match.call()) #renamed as 0 for debug
  args_vec = lapply(args_vec0[-1], FUN = function(x) unlist(x))
  # args_vec = unlist(unclass(args_vec))
  args_vec = do.call("c", args_vec)
  args_vec = ifelse(args_vec == TRUE, 1, args_vec)
  args_vec = ifelse(args_vec == FALSE, 0, args_vec)

  names(args_vec) = gsub("processes.", "", names(args_vec), fixed = TRUE)
  names(args_vec) = gsub("geometry.", "", names(args_vec), fixed = TRUE)
  names(args_vec) = gsub("initial.cond.", "", names(args_vec), fixed = TRUE)


  args_vec["ProfileDepth"] = toupper(format2sci(as.numeric(args_vec["ProfileDepth"]),
                                            ndec = 2, power.digits = 3))

  args_names = names(args_vec)

  h1d_args_names = args_names[!(args_names %in% c("project.name", "parent.dir", "description"))]
  # h1d_args_names = gsub("Profile.", "", h1d_args_names, fixed = TRUE)


  hydrus1d_template = system.file("templates/HYDRUS1D.DAT", package = "hydrusR")
  h1d_dat = readLines(hydrus1d_template, n = -1L, encoding = "unknown")

  descript_vec = c("Pcp_File_Version=1", description)

  for(a in 1:length(h1d_args_names)){
        arg_a = h1d_args_names[a]
        arg_value = args_vec[arg_a]
        arg_index = grep(arg_a, h1d_dat)
        h1d_dat[arg_index] = paste0(arg_a, "=", arg_value)

  }

  write(descript_vec, file = descript_file, append = FALSE)
  write(h1d_dat, file = h1ddat_file, append = FALSE)


  selector_in = system.file("templates/SELECTOR.IN", package = "hydrusR")
  selector_data = readLines(selector_in, n = -1L, encoding = "unknown")

  lunit_ind = grep("LUnit", selector_data)
  unit_lines = lunit_ind + 1:2
  selector_data[unit_lines] = c(SpaceUnit, TimeUnit)

  timeinfo_ind = grep("*** BLOCK C", selector_data, fixed = TRUE)
  timeinfo_data = selector_data[timeinfo_ind+2]

  timeinfo_split = unlist(strsplit(x = timeinfo_data, split = " "))
  timeinfo_split = timeinfo_split[timeinfo_split != ""]
  timeinfo_new = as.numeric(timeinfo_split)

  names(timeinfo_split) = c("dt", "dtMin",  "dtMax", "DMul", "DMul2", "ItMin", "ItMax", "MPL")
  names(timeinfo_new) = c("dt", "dtMin",  "dtMax", "DMul", "DMul2", "ItMin", "ItMax", "MPL")

  if(TimeUnit == "hours"){
    timeinfo_new[c("dt", "dtMin", "dtMax")] = 24*timeinfo_new[c("dt", "dtMin", "dtMax")]
  } else if (TimeUnit == "minutes") {
    timeinfo_new[c("dt", "dtMin", "dtMax")] = 60*24*timeinfo_new[c("dt", "dtMin", "dtMax")]
  } else if(TimeUnit == "seconds"){
    timeinfo_new[c("dt", "dtMin", "dtMax")] = 3600*24*timeinfo_new[c("dt", "dtMin", "dtMax")]
  } else if(TimeUnit == "years") {
    timeinfo_new[c("dt", "dtMin", "dtMax")] = 1/365*timeinfo_new[c("dt", "dtMin", "dtMax")]
  }

  timeinfo_new[c("dt", "dtMin", "dtMax")] = format2sci(timeinfo_new[c("dt", "dtMin", "dtMax")], ndec = 3, power.digits = 3)
  fmt_space = c(12, 13, 12, 8, 8, 6, 6, 6)
  fmt_vec = paste("%", fmt_space, "s", sep = "")
  timeinfo_new_fmt = sprintf(fmt = fmt_vec, timeinfo_new)
  timeinfo_new_str = paste(timeinfo_new_fmt, collapse = "")

  selector_data[timeinfo_ind + 2] = timeinfo_new_str

  write(selector_data, file = file.path(project_path, basename(selector_in)), append = F)

  cat("New HYDRUS-1D project created in", project_path, "...\n")

} #end fn

