#' Creates a new PROFILE.DAT file using a template included in the package.
#'
#' @param project.path Location of the H1D project in the directory
#' @param out.file Default is always 'PROFILE.DAT'
#' @param profile.depth total profile depth. numeric, length = 1. positive value. in same length units as project.
#' @param depth.vec the depths of discrete layers within the soil profile. numeric, any length. in same length units as project. must span from
#' 0 for profile upper boundary to the value given in profile.depth for the lower boundary. positive values. if this is not given, then the soil
#' profile will be split into layers of dz length.
#' @param dz numeric, length = 1. the length of discrete layers in the soil profile. only used if depth.vec not given, in which case the profile.depth
#' is split into layers of length=dz.
#' @param mat the material number associated with each value in depth.vec. integer, same length as depth.vec. integer values must correspond to the
#' row numbers of the soil hydraulic parameters table. e.g., if that table has two rows, then all values in mat will be either 1 or 2 to
#' denote which material to use at this point.
#' @param obs.nodes Vector of observation points in the profile
#' @param head starting values for head (matric potential). Default is -0.1. If given as positive values, they will be converted to negative for
#' creating the input file. In the same length unit as the overall project. if length = 1, it will be replicated for every layer. if length > 1,
#' then it must be same length as depth.vec, one Temp value per layer. Beware setting this to 0 if the intention is to simulate drainage from
#' saturation due to instability and convergence problems near saturation. Starting at slightly negative values (e.g. -0.1 or -1 cm) will increase modelling
#' success and affect results minimally.
#' @param Temp Temperature input. default is 20 degree C. if length = 1, it will be replicated for every layer. if length > 1, then it must be same length
#' as depth.vec, one Temp value per layer.
#' @param Conc Concentration in initial profile. if length = 1, it will be replicated for every layer. if length > 1, then it must be same length
#' as depth.vec, one Conc value per layer. If not doing solute modelling, the Conc value will have no effect and can be left as default.
#'
#' @return
#' @export
#'
#' @examples

create.soil.profile<- function(project.path,
                               out.file = "PROFILE.DAT",
                               profile.depth = 100,
                               depth.vec,
                               dz = 1,
                               mat,
                               head = -0.1,
                               Temp = 20,
                               Conc = 0,
                               obs.nodes = NULL, ...) {

  #check inputs
  if(length(profile.depth) != 1 | !is.numeric(profile.depth)){
    stop("profile.depth must be numeric, length=1")
  }
  if(is.null(depth.vec)){
    depth.vec <- seq(0, profile.depth, by=dz)
  }
  if(min(depth.vec) != 0 | max(depth.vec) != profile.depth){
    stop("node.depths must span from 0 to profile.depth")
  }
  if(length(mat) != length(depth.vec)){
    stop("length of mat must be the same as length of depth.vec. One identifying integer per layer.")
  }
  if(!(length(Temp) %in% c(1, length(depth.vec)))){
    stop("Temp must have length = 1 or same length as depth.vec")
  }
  if(!(length(Conc) %in% c(1, length(depth.vec)))){
    stop("Conc must have length = 1 or same length as depth.vec")
  }
  #force to positive numbers
  profile.depth = abs(profile.depth)
  node.depths <- abs(node.depths)
  #force negative
  head <- -abs(head)




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
  dhead_val = as.numeric(trimws(dhead_val))
  header_rest = substr(table_header, start = 6, stop = nchar(table_header)) #this is the remainder of the header
  dhead_val_new = sprintf("%5s", (profile.depth + 1)) #expected number is 1 higher than the depth
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

  #prepare head column of initial matric potential.
  head_vec = rep(head, length(depth.vec))[1:length(depth.vec)]
  head_vec_fmt = mapply(FUN = format2sci, head_vec, ndec = 6, power.digits = 3)

  #prepare vector of material ID numbers
  mat_vec = rep(mat, length(depth.vec))[1:length(depth.vec)]
  mat_vec_fmt = mat_vec #these do not get the fancy decimal formatting

  layer_vec_fmt = mat_vec_fmt #these prob should not be the same. does not get fancy decimal formatting. just an integer.
  print("PROFILE: unsure difference of mat and layer")

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

  write(profile_data_new, file = profile_file, append = FALSE)

  if(!is.null(obs.nodes)) write.obs.nodes(project.path, obs.nodes)
  print("PROFILE: what are obs.nodes?")

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
