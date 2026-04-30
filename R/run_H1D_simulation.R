#' Main simulation function for running Hydrus 1D
#'
#' This function houses call.H1D(), which directly runs Hydrus, and sets up the inputs for running it and managing its outputs. Specifically, Hydrus
#' will not run simulations with > 960 timesteps, and this function holds a loop structure for running longer simulations wherein the outputs of one
#' run are fed into input files for the next run, and all outputs are bound together at the end. For a short simulation (< 960 timesteps), call.H1D()
#' could be called directly, but always running Hydrus via run.H1D.simulation keeps workflow standardized. \cr
#' #' Note: this function can only be used while running R as an administrator, which can be done by opening RStudio via the right-click menu.#'
#' @param project.path
#' @param hydrus.path The folder where the Hydrus executable is stored. e.g., "C:/Program Files (x86)/PC-Progress/Hydrus-1D 4.xx"
#' @param profile.depth Numeric, length = 1. Total profile depth, in project length units.
#' @param root.depth Numeric, length = 1. rooting depth, in project length units.
#' @param beginT Numeric, length = 1.start time. in project time units
#' @param endT  Numeric, length = 1.end time. in project time units
#' @param deltaT Numeric, length = 1.time step. if running simulation at regular timesteps, then this is required. otherwise, timesteps can be declared as a vector via 'timesteps' argument. if 'timesteps' is given, then deltaT is ignored.
#' @param bot.bc.type Character, length = 1. bottom boundary condition type. "head" or "flux"
#' @param bot.bc.value  Numeric, length = 1. value of the bottom boundary condition. Only needed if 'const.bot.bc' = TRUE.
#' @param const.bot.bc Logical, length = 1. to set if bottom boundary condition is constant. If FALSE, then it is a variable boundary condition and data needs to be supplied in 'atm.bc.data' in either the rB (flux) or hB (head) columns, depending on 'bot.bc.type'
#' @param soil.para
#' @param atm.bc.data data frame containing atmonspheric boundary conditions (time variable BC)
#' @param ini.wt
#' @param TimeUnit character, length = 1. simulation time unit information (default = "days"). permitted: "seconds", "minutes", "hours", "days", "years". \cr
#' @param obs.nodes
#' @param show.output Logical, whether the shell output of HYDRUS1D run should be displayed on R console, default = F
#' @import data.table
#' @export

run.H1D.simulation = function(project.path,
                              hydrus.path = NULL,
                              profile.depth,
                              beginT, endT, deltaT, timesteps,
                              bot.bc.type, bot.bc.value, const.bot.bc,
                              #soil.para,
                              atm.bc.data,
                              #ini.wt,
                              TimeUnit = "days",
                              root.depth,
                              #obs.nodes,
                              show.output = FALSE, ...) {

  #clean args
  #######
  if(is.null(hydrus.path)|missing(hydrus.path)){
    hydrus.path = "C:/Program Files (x86)/PC-Progress/Hydrus-1D 4.xx"
  }


  #remove old data from project dir
  sapply(list.files(project.path, pattern = "\\.OUT|\\.out", full.names = T), file.remove)

  error_file = file.path(project.path, "Error.msg")
  if(file.exists(error_file))  file.remove(error_file)

  prev_sims = dir(project.path, pattern = "sim", full.names = T)
  if(length(prev_sims > 0)){
    mapply(FUN = unlink, prev_sims, recursive = T, force = T)
  }

  #save incoming warning option to reset it at the end
  warn.in <- options("warn")

  #define time variables depending if 'timesteps' is given
  if(!is.null(timesteps)){
    maxTp <- length(timesteps)
    times_s <- timesteps
  } else {
    maxTp = endT/deltaT #count of timesteps
    times_s = seq(beginT, endT, by = deltaT) #these are the timesteps
  }


  #the workflow for running a simulation depends how long it is. if less than 960 timesteps, then
  # it does not need to be run in a loop structure.
  if(maxTp <= 960) {

    ##this is run during data prep, via create.bc(). no need to run it if the simulation is not looped
    # write.atmosph.in(project.path, maxAL = maxTp, deltaT = deltaT,
    #                  atm.bc.data = atm.bc.data[1:maxTp, ])

    ##this is run during data prep, via write.time.settings(). no need to run it if the simulation is not looped
    # write.print.times(project.path, tmin = beginT, tmax = endT,
    #                   tstep = deltaT, TimeUnit = TimeUnit)


    call.H1D(project.path, hydrus.path = hydrus.path, show.output = show.output)

  } else {
    #else it is long and needs looping

    stop("Looping H1D simulations are not enabled with hydrusR updates. see notes")
    #I wrote new code, e.g. updated write.atmosph.in, but all the code below uses old arguments and has not been updated at all.
    # I dind't bother to update this function yet because I don't think any of my MSc uses are going to be long simulations. It
    # wouldn't be hard to update and make it functional, just not putting in the time right now.

    message("Calculating times ", 1, " to ", 960*deltaT, ".....\n")


    write.atmosph.in(project.path, maxAL = 960, deltaT = deltaT,
                     atm.bc.data = atm_bc_data[1:960, ])

    write.print.times(project.path, tmin = beginT, tmax = 960*deltaT,
                      tstep = deltaT, TimeUnit = TimeUnit)

    call.H1D(project.path, hydrus.path = hydrus.path, show.output = show.output)

    error_test = file.exists(error_file)
    #################
    if(isTRUE(error_test)){

      error_msg = readLines(error_file, n = -1L, encoding = "unknown")
      error_msg = paste(error_msg, collapse = "")
      cat(error_msg, ".....\n")
      return(invisible(error_msg))

    } else {

      sim_number = ceiling(maxTp/960)

      sim1_files = list.files(project.path, full.names = TRUE)

      sim1_folder = file.path(project.path,"sim1")
      dir.create (sim1_folder)

      sapply(sim1_files, file.copy, to = sim1_folder)

      options(warn = -1)
      h1d_output =  data.table::fread(file = file.path(project.path, "Nod_Inf.out"),
                                      fill = TRUE, blank.lines.skip = FALSE, skip = 10)

      time_ind = grep("Time:", h1d_output[["Node"]]) #which rows ar eheaders for a new timestep?
      to_skip = time_ind[length(time_ind)]+2 #skip to the final timestep

      head_profile = h1d_output[to_skip:nrow(h1d_output), c("Node", "Depth", "Head")]
      head_profile = as.data.frame(apply(head_profile, 2, as.numeric))
      head_profile = na.omit(head_profile)
      if(nrow(head_profile)==0){ stop("read nod_inf separately. see why all Head rows are NaN") }
      pressure_vec = head_profile$Head

      options(warn = 0)

      message("Calculations from time ", 1, " to ", 960*deltaT, " success .....\n")

      #I'm unsure why this loops starts at 2 and doesn't just include the first run above and start loop from 1.
      # might have to do with the first run getting an error test to make sure that the simulation has a functional start.
      for(s in 2:sim_number) {

        error_test = file.exists(error_file)
        #################
        if(isTRUE(error_test)){

           error_msg = readLines(error_file, n = -1L, encoding = "unknown")
           error_msg = paste(error_msg, collapse = "")
           cat(error_msg, ".....\n")
           return(invisible(error_msg))


        } else {

           sim_index = s

           beginTnew = ((sim_index-1)*960)

           if(s < sim_number){
              endTnew =  sim_index*960
           } else {
              endTnew = nrow(atm.bc.data)
           }

           sim_times_s = seq((beginTnew + 1), endTnew)

           sim_folder = paste("sim", s, sep = "")

           atm_bc_data_s = atm.bc.data[sim_times_s, ]

           message("Calculating times ", ceiling(beginTnew*deltaT), " to ",
               endTnew*deltaT, "\n")

           write.ini.cond(project.path, profile.depth = profile.depth,
                          pr.vec = pressure_vec)

           write.print.times(project.path, tmin = beginTnew*deltaT, tmax = endTnew*deltaT,
                             tstep = deltaT, TimeUnit = TimeUnit)

           write.atmosph.in(project.path, maxAL = nrow(atm_bc_data_s), deltaT = deltaT,
                            atm.bc.data = atm_bc_data_s)

           call.H1D(project.path, hydrus.path = hydrus.path, show.output = show.output)


           sim_out_dir = file.path(project.path, sim_folder)
           if(!dir.exists(sim_out_dir)) dir.create(sim_out_dir)

           sim_s_files = list.files(project.path, include.dirs = F, full.names = T)
           sapply(sim_s_files, FUN = file.copy, to = sim_out_dir)

           #################
           options(warn = -1) #silence warnings for fread
           h1d_output =    data.table::fread(file = file.path(project.path, "Nod_Inf.out"),
                                             fill = TRUE, blank.lines.skip = FALSE, skip = 10)

           time_ind = grep("Time:", h1d_output[["Node"]])
           to_skip = time_ind[length(time_ind)]+2

           head_profile = h1d_output[to_skip:nrow(h1d_output), c("Node", "Depth", "Head")]
           head_profile = as.data.frame(apply(head_profile, 2, as.numeric))
           head_profile = na.omit(head_profile)
           pressure_vec = head_profile$Head

           # sapply(list.files(project.path, pattern = "\\.OUT|\\.out", full.names = T), file.remove)

           options(warn = 0)

           message("simulation from time ", ceiling(beginTnew*deltaT), " to ",
               endTnew*deltaT, " success .....\n")

           next;

        } #end else
     } #end else

     message("combining all output files .....")

     join.output.files(project.path)

     sim_dirs = dir(project.path, pattern = "sim", full.names = TRUE)
     mapply(FUN = unlink, sim_dirs, recursive = T, force = T)

  }
  message("Calculations have finished successfully...")
 }

  options(warn = warn.in)

} #end fn
