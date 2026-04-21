#' Update print times, MPL, tInit, and tMax in SELECTOR.IN
#'
#' This function has two uses. To create a properly formatted TPrint block for SELECTOR.IN containing all times that values will be printed (e.g., to
#' "Nod_Inf.OUT"), and, for use in hydrusR's looping of long simulations, to replace the time range and MPL values for each successive loop's input file.
#' @param project.path Location of the H1D project in the directory
#' @param tmin,tmax Numeric, length = 1. Beginning and ending times of this simulation, in this project's time units. tmin will not have results printed, the first
#' print will be at tmin+print.step, and then at every print.step until tmax, i.e. seq(tmin+print.step, tmax, print.step). tmin does not need to be 0,
#' it can be any value.
#' @param print.step Numeric, length = 1. Time interval to print at, in this project's time units. This would be set in Hydrus GUI in Print Information-Print at Regular Time Interval
#' @param ...
#' @export

write.print.times   <- function(project.path,
                                tmin,
                                tmax,
                                print.step, ...){

  input.file = file.path(project.path, "SELECTOR.IN")

  hydrus_input = readLines(con = input.file, n = -1L, encoding = "unknown")

  Tprint_ind = grep("TPrint", hydrus_input) #get indices of block to be edited
  end_Tprint_ind = grep("BLOCK G", hydrus_input)

  ptimes = seq((tmin + print.step), tmax, by = print.step) #vector of times for printing

  if(length(ptimes) > 1000){
        stop("Hydrus 1D does not allow printing > 1000 time steps!")
  }

  #set up the TPrint block's structure
  nrows = floor(length(ptimes)/6)
  p1 = ptimes[1:(nrows*6)]
  rem_tstep = length(ptimes) - length(p1) #how many steps leftover
  rem_times = ptimes[(length(ptimes) - rem_tstep + 1):length(ptimes)] #what are the times of these leftover steps
  ptimes_mat = matrix(p1, nrow = nrows, ncol = 6, byrow = TRUE) #set up matrix for the main chunk of values

  fmt_vec = c("%11.0f", rep("%12.0f", 5))
  #adjust formatting if time step has decimals
  print.step_decimals = get.decimalplaces(print.step)
  if(print.step_decimals > 0){
    fmt_vec = gsub(pattern = "0", replacement = print.step_decimals, fmt_vec)
  }

  #format the matrix of time values
  ptimes_mat_fmt <- ptimes_mat
  for(p in 1:nrow(ptimes_mat_fmt)){
    ptimes_mat_fmt[p, ] = sprintf(fmt = fmt_vec, ptimes_mat[p, ])
  }
  ptimes_mat_fmt = apply(ptimes_mat_fmt, MARGIN = 1, FUN = paste, collapse = "") #paste each row into one string

  if(rem_tstep > 0) { #format and bind any leftovers (non-full rows)
    last_line = rem_times
    last_line_fmt = sprintf(fmt = fmt_vec[1:length(last_line)], last_line)
    last_line_fmt = paste(last_line_fmt, collapse = "")
    ptimes_mat_final = c(ptimes_mat_fmt, last_line_fmt)
  }  else {
    ptimes_mat_final = ptimes_mat_fmt
  }

  #Update Max print value
  mpl_ind = grep(pattern = " MPL", hydrus_input)
  mpl_line = hydrus_input[(mpl_ind+1)]
  mpl_line_split = unlist(strsplit(mpl_line, split = " ")) #split the line to get just MPL value
  mpl_line_split[length(mpl_line_split)] = sprintf(fmt = "%2s", as.character(length(ptimes))) #MPL is always last in the line
  mpl_line_new = paste(mpl_line_split, collapse = " ") #paste it back together
  hydrus_input[(mpl_ind+1)] = mpl_line_new #overwrite previous line

  #update tMin and tMax
  tmax_ind = grep(" tMax", hydrus_input)
  tmax_line = hydrus_input[(tmax_ind + 1)]
  tmax_line_split = c(tmin, tmax)
  tmax_line_new = sprintf(c("%11.0f", "%12.0f"), tmax_line_split)
  tmax_line_new = paste(tmax_line_new, collapse = "")
  hydrus_input[(tmax_ind + 1)] = tmax_line_new


  #these are the pieces for final output
  input_p1 = hydrus_input[1:Tprint_ind] #everything up to the TPrint block
  input_p2 = ptimes_mat_final #the TPrint block
  input_p3 = hydrus_input[end_Tprint_ind:length(hydrus_input)] #everything after
  #bind them together
  selector_data = c(input_p1, input_p2, input_p3)

  #write SELECTOR.IN with updated values.
  write(selector_data, file = input.file, append = F)


} #end fn
