#' Write time settings
#'
#' These are the settings from Time Information-Time Discretization and Water Flow-Iteration Criteria in the Hydrus GUI. The time settings that control
#' iterations, time steps, run times, etc. These get written to Block C of SELECTOR.IN.
#'
#' @param project.path your project path, where SELECTOR.IN is saved.
#' @param time.range numeric, length = 1. in your project's time units. the start and end times of this simulation.
#' @param dt numeric, length = 1. initial time step, in your project's time units.
#' @param dtMin,dtMax numeric, length = 1. minimum and maximum  allowed time step, in your project's time units.
#' @param DMul numeric, length = 2. range of time step multiplication factors. Default c(0.7,1.3).
#' @param ItRange integer, length = 2. optimal iteration range. Sets parameters ItMin and ItMax.
#' @param print.step integer, length = 1. the interval at which results will be printed (e.g. to "Nod_Inf.OUT"). in your project's time units.
#' @author Trevor Baker <tbaker@iegconsulting.com>
#' @export

write.time.settings <- function(project.path,
                                time.range = c(0,2400),
                                dt = 1e-3,
                                dtMin = 1e-6,
                                dtMax = 1,
                                DMul = c(0.7,1.3),
                                ItRange = c(3,7),
                                print.step){

  print("write.time.settings: better to add print.times arg for better print spacing. see notes")
  #print.step either forces too many closely spaced prints as soil dries, or has too much time between prints when soil is wet
  # if there was an arg print.times that gave exact times to print at, it would be better. this has implications for
  # write.print.times though, which is why it wasn't a quick change. write.print.times needs to be able to sensibly space
  # print times later in the simulation, for which print.step is useful. a solution where the first iteration uses
  # print.times (e.g. 0.001,0.01,0.1,1,10,20,30,...) and later ones just use a regular step would be good.

  ########
  # check/fix args
  if(min(DMul) > 1){
    DMul <- c(0.7, max(DMul))
  }
  if(max(DMul) < 1){
    DMul <- c(min(DMul), 1.3)
  }


  #####
  # read data
  input_data = readLines(con = file.path(project.path, "SELECTOR.IN"),
                         n = -1L, encoding = "unknown")

  #get line indices of all blocks and the end
  block_lines <- which(grepl("\\*\\*\\* ", input_data))
  time_info_ind <- grep("BLOCK C", input_data[block_lines])
  time_start_ind <- block_lines[time_info_ind]
  time_end_ind <- block_lines[time_info_ind+1]-1


  ##########
  #edit Block C
  this_block = input_data[time_start_ind : time_end_ind]
  dt_line_ind = grep("dtMin", this_block) #get index where model is declared. parameters are below this.

  #this line is where values for time settings are declared
  para_line_ind1 = grep("dt", this_block)+1 #the line below the one with param names
  para_name_fmt_vec1 = c("%12s", "%13s", "%12s", "%8s", "%8s", "%6s", "%6s", "%6s", "%6s") #spacing format
  MPL <- floor(diff(time.range)/print.step) #how many times will results be printed
  para_values1 <- c(dt, dtMin, dtMax, max(DMul), min(DMul), min(ItRange), max(ItRange), MPL)
  para_values1[1:3] <- format2sci(para_values1[1:3], ndec = 3, power.digits = 3) #put these in Hydrus sci notation
  para_line_fmt1 = mapply(FUN = sprintf, para_values1, fmt = para_name_fmt_vec1[1:length(para_values1)]) #format the param names
  para_line_new1 = paste(para_line_fmt1, collapse = "") #make it a new line to be subbed back into the block below
  this_block[para_line_ind1] <- para_line_new1

  #now overwrite the original values with those set here
  input_data[time_start_ind : time_end_ind] <- this_block

  #save the file
  write(input_data,
        file =  file.path(project.path, "SELECTOR.IN"),
        append = F)

  ##
  # now use the pre-existing write.print.times fn to fill the TPrint block
  # - this remains as a standalone fn because it is needed for resetting the Time Block for looping
  write.print.times(project.path,
                    tmin = min(time.range),
                    tmax = max(time.range),
                    print.step = print.step)

}
