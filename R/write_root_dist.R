#' Write root distribution in PROFILE.DAT
#'
#' Calculate root distribution values to be assigned to soil nodes in PROFILE.DAT. Values must be between 0 and 1, inclusive. The absolute values
#' of the outputs do not matter, all will be normalized by Hydrus. Values = 0 have zero roots.
#' @param project.path the Hydrus projet path. where PROFILE.DAT is saved.
#' @param root.depth the maximum depth of rooting. in same length units as project.
#' @param rBeta coefficient for defining root distribution. Default = 0.962. Values can be between 0 and 1, but sensible root distributions are
#' obtained with values ranging from ~0.9 to 0.99 (~linear decrease with depth). Enter 0 if rooting should be the same from 0 to root.depth. The
#' distribution of roots is calculated with [1-rBeta^d], for d values max(root.depth):min(root.depth), which are assigned in the reverse order to
#' min(root.depth):max(root.depth). See Example for sample plots showing the effects of rBeta.
#' @export
#' @examples
#' rBeta <- 0.962
#' root.depth <- 100
#' root.depths <- seq(0, root.depth, 1)
#' root.dist <- 1-rBeta^(rev(root.depths))
#' plot(root.depths, root.dist)
#' points(root.depths, 1-0.9^(rev(root.depths)), type = "l")
#' points(root.depths, 1-0.99^(rev(root.depths)), type = "l")
#' points(root.depths, 1-0^(rev(root.depths)), type = "l")

write.root.dist <- function(project.path,
                            root.depth,
                            rBeta = 0.962) {

  #read PROFILE.DAT
  file.profile.dat = file.path(project.path, "PROFILE.DAT")
  def_profile_data = readLines(con = file.profile.dat, n = -1L, encoding = "unknown")

  #these are the header lines
  profile_summary = def_profile_data[1:4]
  pr_header = trimws(def_profile_data[4]) #this is the line that tells number of nodes in the profile (rows in the soil data table)
  num_nodes = as.numeric(unlist(strsplit(pr_header, " "))[1])

  #needed editing to accomodate flexible depths and layers in PROFILE.DAT
  # previously a profile of x cm had x+1 nodes in PROFILE.DAT, so the profile depth could be found like this: profile_depth = num_nodes - 1
  # now that depth needs to be read rather than calculated:
  #start by finding the final row of the profile data, which can be found off of the column numbers. last row to have same number of columns
  # as the 5th row, which seems to always be the first row with Node data
  num.cols <- as.vector(sapply(def_profile_data, function(x){ length(which(strsplit(x, " ")[[1]] != "")) })) #ugly way to count 'columns' in every line
  rle.cols <- rle(num.cols[5:length(num.cols)])
  last.row <- 5 + rle.cols$lengths[1] - 1 #the last consecutive row with same number of columns as the 5th row
  bottom_layer <- def_profile_data[last.row] #grab the data for this row
  bottom_layer <- as.numeric(unlist(strsplit(trimws(bottom_layer), " "))[2]) #split string and grab depth from second element
  profile_depth <- -bottom_layer #return positive value to match existing format

  profile_body = def_profile_data[5:(5 + num_nodes - 1)] #the main table of soil values, one row per Node

  node_info_lines = def_profile_data[(num_nodes + 5):(length(def_profile_data))] #the two lines that give count and depths of nodes

  header_split = unlist(strsplit(def_profile_data[4], split = " "))
  header_split2 = header_split[header_split != ""] #the header row at the top of profile_body

  profile_data_split = strsplit(profile_body, split = " ")
  profile_data_split2 = sapply(profile_data_split, FUN = function(x) x[x!= ""])
  profile_data_new = t(profile_data_split2) #the soil data as a matrix instead of lines of text

  # #can't use deltaz anymore because thicknesses are allowed flexibility by create.soil.data
  # deltaz = abs(as.numeric(profile_data_new[3, 2]) - as.numeric(profile_data_new[2, 2]))
  # rdepth_coord = seq(0, root.depth, by = deltaz)

  #the equivalent to the code above, which gave depth values for all Nodes that fall within the rooting depth is to subset the
  # depth column of profile_data
  all_depths <- -as.numeric(profile_data_new[,2])
  rdepth_coord <- all_depths[all_depths <= root.depth]
  if(!any(rdepth_coord == root.depth)){
    cat("write_root_dist: A row in the soil data should be added at bottom rooting depth, 'root.depth'. See code notes.")
    #fudge it so that the layer containing the bottom of roots gets rooting assigned.
    # e.g layers at 60-80 and 80-100. rooting depth at 90. without fusging this value, the 80-100 layer would be assigned no roots
    rdepth_coord <- sort(c(rdepth_coord,root.depth))
  }
  #it would not be hard to edit the code here to insert a new row at root.depth. would need to copy the correct Node row, and edit the value in
  # the header that gives num_nodes (add 1)


  #old code
  # doesn't seem quite right or flexible to depths and node counts
  # rdist = 1 - rBeta^rdepth_coord
  # rdist = rev(rdist)
  # rdist = c(1, rdist)
  # rdist_new = numeric(nrow(profile_data_new))
  # rdist_new[1:length(rdist)] = rdist
  #
  #new code uses rBeta but written differently because not all layers have same thickness
  smallest.int <- min(diff(all_depths)) #this needs to be the basis so that even the thinnest layer gets correct value
  all.int <- seq(0,max(rdepth_coord),smallest.int) #sequence from 0 to rooting depth at the smallest interval
  all.rdist <-  1-rBeta^all.int #rdist values for every interval
  #now group them into their nodes. some nodes have one value, some are wider intervals and contain many values
  node.rdist <- sapply(2:length(all_depths), function(x){
    this.deps <- c(all_depths[x-1], all_depths[x])
    this.index <- which(all.int >= this.deps[1] &
                          all.int < this.deps[2])
    this.rdist <- all.rdist[this.index]
    return(this.rdist)
  })
  node.rdist <- sapply(node.rdist, sum) #sum all to get one value per row of soil table
  print("write.root.dist: what is origin of rBeta? need ref.")
  #plot(all.int[1:length(node.rdist)], node.rdist) #rdist will have length one less than all.int so need to subset for plotting
  rdist <- rev(node.rdist) #highest values at top of soil
  #rdist <- rdist/sum(rdist) #normalize to sum 1 - this is not necessary. Hydrus results will be the same if rdist values by layer are 1,1,0,... as it would be with 0.5,0.5,0,...
  rdist_new <- rep(0, num_nodes) #make full vector. one value for every Node row. default zero roots.
  rdist_new[1:length(rdist)] <- rdist #sub the other values, starting from top
  #plot(all_depths, rdist_new)

  root_dist_fmt = mapply(FUN = format2sci, rdist_new, ndec = 6, power.digits = 3)

  profile_data_new[1:length(root_dist_fmt), 6] <- root_dist_fmt

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

# write.root.dist<- function(file.profile.dat, root.depth, rbeta = 0.962, n.nodes, ...) {
#
# profile_dat = readLines(con = file.profile.dat, n = -1L, encoding = "unknown")
#
#       # node_ind = grep(pattern = paste0(n.nodes, " "), profile_dat)
#       # diff(node_ind)
#
#       skip = 5
#
#
#       last_line = profile_dat[length(profile_dat)]
#       last_line_split = unlist(strsplit(last_line, split = " "))
#       last_line_split = last_line_split[last_line_split!= ""]
#
#       header_split = unlist(strsplit(profile_dat[5], split = " "))
#       header_split2 = header_split[header_split != ""]
#
#      node_data = profile_dat[(skip +1):(skip + n.nodes)]
#
#       # if(length(last_line_split) == 0) {
#       #       node_data = profile_dat[skip:length(profile_dat)]
#       # } else {
#       #       node_data = profile_dat[6:(length(profile_dat) - 1)]
#       # }
#
#       node_data_split = strsplit(node_data, split = " ")
#       node_data_split2 = lapply(node_data_split, FUN = function(x) x[x!= ""])
#       node_data_new = do.call("rbind", node_data_split2)
#
#       deltaz = abs(as.numeric(node_data_new[3, 2]) - as.numeric(node_data_new[2, 2]))
#       rdepth_coord = seq(0, root.depth, by = deltaz)
#
#       rdist = 1 - rbeta^rdepth_coord
#
#       rdist = rev(rdist)
#       rdist = c(1, rdist)
#       rdist_new = numeric(nrow(node_data_new))
#       rdist_new[1:length(rdist)] = rdist
#
#       root_dist_fmt = format(rdist_new, scientific = TRUE)
#
#       node_data_new[1:length(root_dist_fmt), 6] = root_dist_fmt
#
#       fmt_space = c(5, 15, 15, 5, 5, 15, 15, 15, 15)
#       fmt_vec = paste("%", fmt_space, "s", sep = "")
#
#       node_data_fmt = node_data_new
#       for(n in 1:nrow(node_data_new)){
#
#             node_data_fmt[n, ] = sprintf(fmt_vec, node_data_new[n, ])
#       }
#
#       tspace = sprintf("%13s", "")
#       node_data_fmt2 = apply(node_data_fmt, MARGIN = 1, FUN = paste, collapse = "")
#       node_data_fmt2 = paste(node_data_fmt2, tspace)
#       profile_data_new = c(profile_dat[1:5], node_data_fmt2)
#
#       write(profile_data_new, file.profile.dat, append = FALSE)
#
# }
