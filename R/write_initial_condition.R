#' Write initial pressure head conditions in profile.dat (water table)
#'
#' This function will allow the user to give either a vector of pressure head values or a water table depth to a soil profile. If both are given,
#' water table is ignored. These inputs will determine the initial moisture condition of the soil profile. This is used both when setting up a simulation
#' and by run.H1D.simulation() when looping through long timeframes to transfer values from the endpoint of one simulaiton to the start of the next. \cr
#' Note that the Head argument of create.soil.profile makes the pr.vec part of this function redundant for setting up a simulation. For setting up,
#' it will be easier for traceback to enter this data via create.soil.profile. If a water table is desired for initial conditions, then this function
#' can be used.
#' @param project.path Path of HYDRUS1D project. Must contain a PROFILE.DAT file already. e.g., from create.soil.profile()
#' @param pr.vec A vector of pressure head values (length = # of total nodes in the profile). Same length units as overall project.
#' @param wt.depth Depth of water table (to assign hydrostatic initial condition). In same length units as overall project.
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

write.ini.cond<- function(project.path,
                          pr.vec = NULL,
                          wt.depth = NULL, ...) {

  file.profile.dat = file.path(project.path, "PROFILE.DAT")
  def_profile_data = readLines(con = file.profile.dat,
                               n = -1L, encoding = "unknown")

  profile_summary = def_profile_data[1:4] #the header rows

  pr_header = trimws(def_profile_data[4])
  num_nodes = as.numeric(unlist(strsplit(pr_header, " "))[1])

  #needed editing to accomodate flexible depths and layers in PROFILE.DAT
  # previously a profile of x cm had x+1 nodes in PROFILE.DAT, so the profile depth could be found like this: profile_depth = num_nodes - 1
  # now that depth needs to be read rather than calculated.
  #start by finding the final row of the profile data, which can be found off of the column numbers. last row to have same number of columns
  # as the 5th row, which seems to always be the first row with Node data
  num.cols <- as.vector(sapply(def_profile_data, function(x){ length(which(strsplit(x, " ")[[1]] != "")) })) #ugly way to count 'columns' in every line
  rle.cols <- rle(num.cols[5:length(num.cols)])
  last.row <- 5 + rle.cols$lengths[1] - 1 #the last consecutive row with same number of columns as the 5th row
  bottom_layer <- def_profile_data[last.row] #grab the data for this row
  bottom_layer <- as.numeric(unlist(strsplit(trimws(bottom_layer), " "))[2]) #split string and grab depth from second element
  profile_depth <- -bottom_layer #return positive value to match existing format

  profile_body = def_profile_data[5:(5 + num_nodes - 1)] #the table of soil profile data, one row per Node

  # node_num_ind = grep(pattern =  ("^[0-9]$"), def_profile_data)
  #
  # pr_tail = tail(def_profile_data, 1)
  # tail_split = unlist(strsplit(pr_tail, split = " "))

  node_info_lines = def_profile_data[(num_nodes + 5):(length(def_profile_data))]

  #this is the table header. separate it into individual values
  header_split = unlist(strsplit(def_profile_data[4], split = " "))
  header_split2 = header_split[header_split != ""]

  #get the profile data as a matrix rather than one line of text per row
  profile_data_split = strsplit(profile_body, split = " ")
  profile_data_split2 = sapply(profile_data_split, FUN = function(x) x[x!= ""])
  profile_data_new = t(profile_data_split2)

  #old code that assumes even spacing (dz) between all Nodes
  # deltaz = abs(as.numeric(profile_data_new[3, 2]) - as.numeric(profile_data_new[2, 2]))
  #
  # if(!is.null(pr.vec)){
  #       ini_pr_vec = pr.vec
  # } else {
  #       ini_pr_vec = seq(0, profile_depth, by = deltaz) - wt.depth #this sets heads relative to zero water depth. -1 cm/cm above water, +1 cm/cm below
  #
  # }

  #note: the structure of this can cause issues. if someone gives pr.vec, then their wt.depth will be ignored.
  if(!is.null(pr.vec)){
    ini_pr_vec = pr.vec #if user gave pressure heads, then use them
    if(!is.null(wt.depth)){ warning("wt.depth is being ignored because pr.vec was given.") }
  } else if(!is.null(wt.depth)){
    node.depths <- -as.numeric(profile_data_new[,2]) #need these to be positive to match previous code
    ini_pr_vec =  node.depths - wt.depth #this sets heads relative to zero water depth. -1 cm/cm above water, +1 cm/cm below
  } else {
    cat("write.ini.cond: No data for pressure head or water table given. No changes made.")
    return()
  }

  #format these pressure heads
  pr_vec_fmt = mapply(FUN = format2sci, ini_pr_vec, ndec = 6, power.digits = 3)

  profile_data_new[1:length(pr_vec_fmt), 3] = pr_vec_fmt #replace the pre-existing Head values

  #format and past back together the output in ugly Hydrus format.
  fmt_space = c(5, 15, 15, 5, 5, 15, 15, 15, 15, 15, 15)
  fmt_vec = paste("%", fmt_space, "s", sep = "")
  fmt_vec = fmt_vec[1:ncol(profile_data_new)]

  profile_data_fmt = profile_data_new
  for(n in 1:nrow(profile_data_new)){

        profile_data_fmt[n, ] = sprintf(fmt_vec, profile_data_new[n, ])
  }

  tspace = sprintf("%13s", "")
  profile_data_fmt2 = apply(profile_data_fmt, MARGIN = 1, FUN = paste, collapse = "")
  profile_data_fmt2 = paste(profile_data_fmt2, tspace)

  profile_data_new = c(profile_summary, profile_data_fmt2, node_info_lines)

  write(profile_data_new, file.profile.dat, append = FALSE)

}
