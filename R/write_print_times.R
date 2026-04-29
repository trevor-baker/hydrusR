#' Update print times, MPL, tInit, and tMax in SELECTOR.IN
#'
#' This function has two uses. To create a properly formatted TPrint block for SELECTOR.IN containing all times that values will be printed (e.g., to
#' "Nod_Inf.OUT"), and, for use in hydrusR's looping of long simulations, to replace the time range and MPL values for each successive loop's input file.
#' @param project.path Location of the H1D project in the directory
#' @param print.at numeric vector, any length. The times to print results at, in this project's time units.
#' @export

write.print.times   <- function(project.path,
                                print.at){

  input.file = file.path(project.path, "SELECTOR.IN")

  hydrus_input = readLines(con = input.file, n = -1L, encoding = "unknown")

  Tprint_ind = grep("TPrint", hydrus_input) #get indices of block to be edited
  end_Tprint_ind = grep("BLOCK G", hydrus_input)

  ptimes <- print.at #keep pre-exisitng name. this used to be made by seq(tmin+print.step, tmax, by = print.step). now print.at is given as argument

  if(length(ptimes) > 1000){
        stop("Hydrus 1D does not allow printing > 1000 time steps!")
  }

  #set up the TPrint block's structure
  nrows = floor(length(ptimes)/6)
  p1 = ptimes[1:(nrows*6)]
  rem_tstep = length(ptimes) - length(p1) #how many steps leftover
  rem_times = ptimes[(length(ptimes) - rem_tstep + 1):length(ptimes)] #what are the times of these leftover steps
  ptimes_mat = matrix(p1, nrow = nrows, ncol = 6, byrow = TRUE) #set up matrix for the main chunk of values


  #format the matrix of time values
  fmt_vec0 = c("%11.0f", rep("%12.0f", 5)) #the standard spacing for a line of the TPrint table
  ptimes_mat_fmt <- ptimes_mat #make a copy of the matrix that will be formatted below
  for(p in 1:nrow(ptimes_mat_fmt)){
    #adjust formatting if time step has decimals
    options(scipen = 999) #temporary so get.decimalplaces doesn't fail on sci notation
    this.decimals <- sapply(ptimes_mat[p,], get.decimalplaces)
    options(scipen = 0)
    fmt_vec <- fmt_vec0 #preserve original for next iteration
    fmt_vec = sapply(1:length(this.decimals), #change number formatting for correct number of decimals
                     function(x){ gsub(pattern = "0", replacement = this.decimals[x], fmt_vec[x]) })
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

  # #Update Max print value - not needed. this is done now in write.time.settings based on length(print.at)
  # # - the other values in this line were set in the parent write.time.settings function, Just MPL needs update here
  # mpl_ind = grep(pattern = " MPL", hydrus_input)
  # mpl_line = hydrus_input[(mpl_ind+1)]
  # mpl_line_split = unlist(strsplit(mpl_line, split = " ")) #split the line to get just MPL value
  # mpl_line_split[length(mpl_line_split)] = sprintf(fmt = "%2s", as.character(length(ptimes))) #MPL is always last in the line
  # mpl_line_new = paste(mpl_line_split, collapse = " ") #paste it back together
  # hydrus_input[(mpl_ind+1)] = mpl_line_new #overwrite previous line

  # #update tMin and tMax - commneted out because moved to write_time_settings where the row of settings above is already made
  # tmax_ind = grep(" tMax", hydrus_input)
  # tmax_line = hydrus_input[(tmax_ind + 1)]
  # tmax_line_split = c(tmin, tmax)
  # tmax_line_new = sprintf(c("%11.0f", "%12.0f"), tmax_line_split)
  # tmax_line_new = paste(tmax_line_new, collapse = "")
  # hydrus_input[(tmax_ind + 1)] = tmax_line_new

  #these are the pieces for final output
  input_p1 = hydrus_input[1:Tprint_ind] #everything up to the TPrint block
  input_p2 = ptimes_mat_final #the TPrint block
  input_p3 = hydrus_input[end_Tprint_ind:length(hydrus_input)] #everything after
  #bind them together
  selector_data = c(input_p1, input_p2, input_p3)

  #write SELECTOR.IN with updated values.
  write(selector_data, file = input.file, append = F)


} #end fn
