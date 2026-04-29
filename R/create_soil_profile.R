#' Creates a new PROFILE.DAT file using a template included in the package.
#'
#' @param project.path Location of the H1D project in the directory
#' @param out.file Default is always 'PROFILE.DAT'
#' @param profile.depth total profile depth. numeric, length = 1. positive value. in same length units as project.
#' @param root.depth rooting depth. numeric, length = 1. positive value. in same length units as project. Roots will be assigned between surface
#' (depth = 0) and this depth, at a density set by rBeta. see ?hydrusR::write.root.dist for details.
#' @param rBeta coefficient for defining root distribution. Default = 0.962. Values can be between 0 and 1, but sensible root distributions are
#' obtained with values ranging from ~0.9 to 0.99 (~linear decrease with depth). Enter 0 if rooting should be the same from 0 to root.depth. The
#' distribution of roots is calculated with [1-rBeta^d], for d values max(root.depth):min(root.depth), which are assigned in the reverse order to
#' min(root.depth):max(root.depth). See ?hydrusR::write.root.dist for example plots.
#' @param depth.vec the depths of discrete layers within the soil profile. numeric, any length. in same length units as project. must span from
#' 0 for profile upper boundary to the value given in profile.depth for the lower boundary. positive values. if this is not given, then the soil
#' profile will be split into layers of dz length.
#' @param dz numeric, length = 1. the length of discrete layers in the soil profile. only used if depth.vec not given, in which case the profile.depth
#' is split into layers of length=dz.
#' @param mat the material number associated with each value in depth.vec. integer, same length as depth.vec. or length = 1 if all are same material.
#' integer values must correspond to the row numbers of the soil hydraulic parameters table. e.g., if that table has two rows, then all values in
#' 'mat' will be either 1 or 2 to denote which material to use at this point.
#' @param lyr the layer number (aka subregion) for mass balance calculations. integer, same length as depth.vec. or length = 1 if only one mass
#' balance node is wanted. Leave as NULL to have it set identically to 'mat', which will mean one subregion per distinct material. It is recommended
#' to have at least one subregion per distinct material, which could be optionally split into smaller subregions. These mass balance subregions
#' are assessed at each timestep for conformance to solution tolerances (e.g., within 1 cm head and 0.001 m3/m3 moisture).
#' @param obs.nodes Vector of observation points in the profile
#' @param Head starting values for head (matric potential). Default is -0.1. If given as positive values, they will be converted to negative for
#' creating the input file. In the same length unit as the overall project. if length = 1, it will be replicated for every layer. if length > 1,
#' then it must be same length as depth.vec, one Temp value per layer. Beware setting this to 0 if the intention is to simulate drainage from
#' saturation due to instability and convergence problems near saturation. Starting at slightly negative values (e.g. -0.1 or -1 cm) will increase modelling
#' success and affect results minimally.
#' @param Temp Temperature input. default is 20 degree C. if length = 1, it will be replicated for every layer. if length > 1, then it must be same length
#' as depth.vec, one Temp value per layer.
#' @param Conc Concentration in initial profile. if length = 1, it will be replicated for every layer. if length > 1, then it must be same length
#' as depth.vec, one Conc value per layer. If not doing solute modelling, the Conc value will have no effect and can be left as default.
#' @author Subodh Acharya <https://github.com/shoebodh>; Trevor Baker <tbaker@iegconsulting.com>
#' @export

create.soil.profile <- function(project.path,
                                out.file = "PROFILE.DAT",
                                profile.depth = 100,
                                root.depth = 100,
                                rBeta = 0.962,
                                depth.vec = NULL,
                                dz = 1,
                                mat = 1,
                                lyr = NULL,
                                Head = -0.1,
                                Temp = 20,
                                Conc = 0,
                                obs.nodes = NULL, ...) {

  #PROFILE.DAT columns:
  # - column 1, unnamed in PROFILE.DAT, z [L]: z-coordinate of node n [L] (depth).
  # - h [L]: pressure head
  # - Mat [-]: material code. integer corresponding to soil parameters.
  # - Lay [-]: subregion number. integer.
  # - Beta [1/L]: root density in this Node. displayed as 'Root' in Hydrus GUI. Numeric between 0-1. Sum of all Beta values will be normalized to sum=1 by Hydrus, so only relative values are meaningful here.
  # - Axz, Bxz, Dxz [-]: Nodal value of the dimensionless scaling factor associated with the pressure head, hydraulic conductivity, and moisutre content. Leave all as 1.
  # - Temp [deg C]: temperature of this Node to start. doesn't matter for water flow, only for solute modelling.
  # - Conc [M/L^3]: concentration of solute in this Node to start. ignore for water flow.


  #check inputs
  if(length(profile.depth) != 1 | !is.numeric(profile.depth)){
    stop("profile.depth must be numeric, length=1")
  }
  if(is.null(depth.vec)){
    depth.vec <- seq(0, profile.depth, by=dz)
  }
  if(min(depth.vec) != 0 | max(depth.vec) != profile.depth){
    stop("depth.vec must span from 0 to profile.depth")
  }
  if(length(mat) != length(depth.vec) & length(mat) != 1){
    if(length(mat) == (length(depth.vec)-1) ){
      mat <- c(mat, mat[length(mat)]) #fill the last material for the final unfilled node.
    } else {
      stop("length of mat must be the same as length of depth.vec. One identifying integer per layer.")
    }
  } else if (length(mat) == 1){
    mat <- rep(mat, length(depth.vec))
  }
  if(is.null(lyr)){
    lyr <- mat
  } else if(length(lyr) != length(depth.vec) & length(lyr) != 1){
    stop("length of lyr must be the same as length of depth.vec. One identifying integer per layer.")
  } else if (length(mat) == 1){
    lyr <- rep(lyr, length(depth.vec))
  }
  if(!(length(Temp) %in% c(1, length(depth.vec)))){
    stop("Temp must have length = 1 or same length as depth.vec")
  }
  if(!(length(Conc) %in% c(1, length(depth.vec)))){
    stop("Conc must have length = 1 or same length as depth.vec")
  }
  #force to positive numbers
  profile.depth = abs(profile.depth)
  depth.vec <- abs(depth.vec)
  #force negative
  Head <- -abs(Head)




  #read template for file structure
  profile.template = system.file("templates/PROFILE.DAT", package = "hydrusR")
  profile_dat = readLines(profile.template, n = -1L, encoding = "unknown")

  #extract upper header
  header_line = profile_dat[1:2]

  #extract line containing depths
  dline = profile_dat[3]
  dline_split  = unlist(strsplit(dline, " "))
  dline_split = dline_split[dline_split!= ""]
  #I'm unsure what the other 3 values mean, but the second one is the total profile depth.
  dformat_new = format2sci(-profile.depth, ndec = 6, power.digits = 3) #format new depth for new dline
  dline_split_new = dline_split
  dline_split_new[2] = dformat_new #insert new profile depth
  #format and paste the new dline together
  fmt_space = c(5, 15, 15, 15) #the spacing pattern expected by Hydrus
  fmt_vec = paste("%", fmt_space, "s", sep = "")
  dline_fmt_new = sprintf(fmt = fmt_vec, dline_split_new)
  dline_fmt_new = paste(dline_fmt_new, collapse = "")

  #extract the header row of the table (incl column names)
  table_header = profile_dat[4]
  dhead_val = substr(table_header, start = 1, stop = 5) #the first part indicates profile depth
  dhead_val = as.numeric(trimws(dhead_val)) #the depth of the template file
  header_rest = substr(table_header, start = 6, stop = nchar(table_header)) #this is the remainder of the header
  dhead_val_new = sprintf("%5s", length(depth.vec)) #expected number is 1 higher than the depth
  #paste together the header line with the new depth in it
  table_header_new = paste0(dhead_val_new, header_rest)

  #work with the main table of values
  table_body = profile_dat[5:length(profile_dat)]
  table_body = table_body[-length(table_body)] #drop row with number of observation points value
  #strip the whitespaces off of the data
  body_split = strsplit(table_body, split = " ")
  body_split2 = sapply(body_split, FUN = function(x) x[x!= ""])
  #transpose into proper shape
  table_body_new = t(body_split2)

  row_counts = seq(length(depth.vec)) #this makes the first column of row ID numbers

  #prepare temp and conc columns
  temp_vec = rep(Temp, length(depth.vec))[1:length(depth.vec)] #rep then subset to correc tlength. works for single value or one value per layer
  conc_vec = rep(Conc, length(depth.vec))[1:length(depth.vec)]
  temp_vec_fmt = mapply(FUN = format2sci, temp_vec, ndec = 6, power.digits = 3)
  conc_vec_fmt = mapply(FUN = format2sci, conc_vec, ndec = 6, power.digits = 3)

  #prepare beta, axz, bxz, dxz columns
  zero_vec = rep(0, length(depth.vec)) #this will fill the beta column
  one_vec = rep(1, length(depth.vec)) #this will fill the axz, bxz, dxz columns
  zero_vec_fmt = mapply(FUN = format2sci, zero_vec, ndec = 6, power.digits = 3)
  one_vec_fmt = mapply(FUN = format2sci, one_vec, ndec = 6, power.digits = 3)
  beta_vec_fmt = zero_vec_fmt
  axz_vec_fmt = one_vec_fmt
  bxz_vec_fmt = one_vec_fmt
  dxz_vec_fmt = one_vec_fmt

  #prepare depth column
  depth_vec_fmt = mapply(FUN = format2sci, depth.vec, ndec = 6, power.digits = 3)
  depth_vec_fmt = paste0("-", depth_vec_fmt)

  #prepare Head column of initial matric potential.
  head_vec = rep(Head, length(depth.vec))[1:length(depth.vec)]
  head_vec_fmt = mapply(FUN = format2sci, head_vec, ndec = 6, power.digits = 3)

  #prepare vector of material ID numbers
  mat_vec = rep(mat, length(depth.vec))[1:length(depth.vec)]
  mat_vec_fmt = mat_vec #these do not get the fancy decimal formatting

  layer_vec_fmt = mat_vec_fmt #these prob should not be the same. does not get fancy decimal formatting. just an integer.

  #bring all the prepped and formatted columns together
  profile_df = data.frame(row_counts,
                          depth_vec_fmt,
                          head_vec_fmt,
                          mat_vec_fmt,
                          layer_vec_fmt,
                          beta_vec_fmt,
                          axz_vec_fmt,
                          bxz_vec_fmt,
                          dxz_vec_fmt,
                          temp_vec_fmt,
                          conc_vec_fmt)
  profile_mat = as.matrix(profile_df)

  #format the
  fmt_space_body = c(5, 15, 15, 5, 5, 15, 15, 15, 15, 15, 15)
  fmt_vec_body = paste("%", fmt_space_body, "s", sep = "")

  profile_mat_fmt = profile_mat
  colnames(profile_mat_fmt) = NULL

  for(n in 1: nrow(profile_mat_fmt)){

        profile_mat_fmt[n, ] = sprintf(fmt_vec_body, profile_mat[n, ])

  }

  profile_mat_fmt2 = apply(profile_mat_fmt, MARGIN = 1, FUN = paste, collapse = "")
  tspace = sprintf("%13s", "")
  profile_mat_fmt2 = paste(profile_mat_fmt2, tspace)

  profile_data_new = c(header_line, dline_fmt_new, table_header_new, profile_mat_fmt2, " 0")

  profile_file = file.path(project.path, out.file)

  #write PROFILE.DAT, which may be modified below by subsequent functions
  write(profile_data_new, file = profile_file, append = FALSE)

  Sys.sleep(0.3)
  #Run modifying functions
  ########################

  #add observation node info, if there are any
  if(!is.null(obs.nodes)) write.obs.nodes(project.path, obs.nodes)
  print("PROFILE: what are obs.nodes?")

  # ##this is now done in create.H1D.project with all other HYDRUS1D.DAT edits.
  # #add Subregion count to HYDRUS1D.DAT file
  # h1d_path = file.path(project.path, "HYDRUS1D.DAT")
  # h1d_dat = readLines(h1d_path, n = -1L, encoding = "unknown")
  # h1d_indx <- which(grepl("SubregionNumbers", h1d_dat))
  # h1d_sr <- paste0("SubregionNumbers=",length(unique(lyr)))
  # h1d_dat[h1d_indx] <- h1d_sr
  # write(h1d_dat, file = h1d_path, append = FALSE)

  Sys.sleep(0.3)
  #edit rooting profile (Beta column)
  write.root.dist(project.path = project.path,
                  root.depth = root.depth,
                  rBeta = rBeta)


} #end fn


#original function code from hydrusR
#
# create.soil.profile<- function(project.path, out.file = "PROFILE.DAT", profile.depth,
#                                dz = 1,
#                                Temp = 20, Conc = 0, obs.nodes = NULL, ...) {
#
#   profile.template = system.file("templates/PROFILE.DAT", package = "hydrusR")
#   profile_dat = readLines(profile.template, n = -1L, encoding = "unknown")
#
#   header_line = profile_dat[1:2]
#
#   dline = profile_dat[3]
#   dline_split  = unlist(strsplit(dline, " "))
#   dline_split = dline_split[dline_split!= ""]
#
#   dval = dline_split[2]
#
#   dformat_new = format2sci(-profile.depth, ndec = 6, power.digits = 3)
#
#   dline_split_new = dline_split
#   dline_split_new[2] = dformat_new
#   #   dline_split_new = dline_split_new[]
#
#
#   fmt_space = c(5, 15, 15, 15)
#   fmt_vec = paste("%", fmt_space, "s", sep = "")
#
#   dline_fmt_new = sprintf(fmt = fmt_vec, dline_split_new)
#   dline_fmt_new = paste(dline_fmt_new, collapse = "")
#
#   table_header = profile_dat[4]
#
#   dhead_val = substr(table_header, start = 1, stop = 5)
#   dhead_val = as.numeric(trimws(dhead_val))
#
#   header_rest = substr(table_header, start = 6, stop = nchar(table_header))
#   dhead_val_new = sprintf("%5s", (profile.depth + 1))
#
#   table_header_new = paste0(dhead_val_new, header_rest)
#
#   table_body = profile_dat[5:length(profile_dat)]
#   table_body = table_body[-length(table_body)] #### row with number of observation points value
#
#   body_split = strsplit(table_body, split = " ")
#   body_split2 = sapply(body_split, FUN = function(x) x[x!= ""])
#
#   # body_new = do.call("rbind", body_split2)
#
#   table_body_new = t(body_split2)
#
#   depth_vec = seq(0, profile.depth, by = dz)
#   head_vec = numeric(length(depth_vec))
#
#   row_counts = seq(length(depth_vec))
#
#   zero_vec = numeric(length(depth_vec))
#   one_vec = rep(1, length(depth_vec))
#   temp_vec = rep(Temp, length(depth_vec))
#   conc_vec = rep(Conc, length(depth_vec))
#
#   zero_vec_fmt = mapply(FUN = format2sci, zero_vec, ndec = 6, power.digits = 3)
#   one_vec_fmt = mapply(FUN = format2sci, one_vec, ndec = 6, power.digits = 3)
#
#
#   depth_vec_fmt = mapply(FUN = format2sci, depth_vec, ndec = 6, power.digits = 3)
#   depth_vec_fmt = paste0("-", depth_vec_fmt)
#   head_vec_fmt = zero_vec_fmt
#
#   mat_vec = one_vec
#   layer_vec = one_vec
#
#   beta_vec_fmt = zero_vec_fmt
#   axz_vec_fmt = one_vec_fmt
#   bxz_vec_fmt = one_vec_fmt
#   dxz_vec_fmt = one_vec_fmt
#
#   temp_vec_fmt = mapply(FUN = format2sci, temp_vec, ndec = 6, power.digits = 3)
#   conc_vec_fmt = mapply(FUN = format2sci, conc_vec, ndec = 6, power.digits = 3)
#
#
#   profile_df = data.frame(row_counts,
#                           depth_vec_fmt,
#                           head_vec_fmt,
#                           mat_vec,
#                           layer_vec,
#                           beta_vec_fmt,
#                           axz_vec_fmt,
#                           bxz_vec_fmt,
#                           dxz_vec_fmt,
#                           temp_vec_fmt,
#                           conc_vec_fmt)
#   profile_mat = as.matrix(profile_df)
#
#   fmt_space_body = c(5, 15, 15, 5, 5, 15, 15, 15, 15, 15, 15)
#   fmt_vec_body = paste("%", fmt_space_body, "s", sep = "")
#
#   profile_mat_fmt = profile_mat
#   colnames(profile_mat_fmt) = NULL
#
#   for(n in 1: nrow(profile_mat_fmt)){
#
#     profile_mat_fmt[n, ] = sprintf(fmt_vec_body, profile_mat[n, ])
#
#   }
#
#   profile_mat_fmt2 = apply(profile_mat_fmt, MARGIN = 1, FUN = paste, collapse = "")
#   tspace = sprintf("%13s", "")
#   profile_mat_fmt2 = paste(profile_mat_fmt2, tspace)
#
#   profile_data_new = c(header_line, dline_fmt_new, table_header_new, profile_mat_fmt2, " 0")
#
#   profile_file = file.path(project.path, out.file)
#
#   write(profile_data_new, file = profile_file, append = FALSE)
#
#
