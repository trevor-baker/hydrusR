#' Write observation nodes to profile.dat
#'
#' Observation nodes (count and depths of each) are the last two rows of PROFILE.DAT
#' @param project.path path to the Hydrus project.
#' @param obs.nodes Observation node depths. Numeric vector. Same length units as the project. Leave NULL if none wanted.
#' @export
#' @examples \dontrun{
#' write.obs.nodes(project.path, obs.nodes = c(5, 10, 20, 30)) ## Writes at these depths
#' write.obs.nodes(project.path) ##
#' }
#'
write.obs.nodes<- function(project.path, obs.nodes = NULL, ...) {

  def_profile_data =   readLines(con = file.path(project.path, "PROFILE.DAT"),
                                 n = -1L, encoding = "unknown", warn = F)

  profile_summary = def_profile_data[1:4]

  pr_header = trimws(def_profile_data[4])
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

  profile_body = def_profile_data[5:(5 + num_nodes - 1)] #this is the body of the profile data, the values for each Node

  if(length(obs.nodes) == 1 && obs.nodes == 0) obs.nodes = NULL

  #num_obs_nodes is the 2nd last row in PROFILE.DAT. a count of how many observation nodes in the profile
  if(missing(obs.nodes)|is.null(obs.nodes)) {
     num_obs_nodes = sprintf("%5.0f", 0)

  } else {
     num_obs_nodes = sprintf("%5.0f", length(obs.nodes))
     if(max(obs.nodes) > profile_depth){
        cat ("omitting observation nodes deeper than  profile  depth ...\n" )
        obs.nodes = obs.nodes[obs.nodes <= profile_depth]
     }
  }
  #the last row in PROFILE.DAT gives the depths of the nodes
  nodes_fmt = sprintf(fmt = "%5.0f", obs.nodes)
  nodes_new = paste(nodes_fmt, collapse = "")

  #put these all together: the header, the main profile table, the obs nodes count, and the depth of all obs nodes.
  profile_data = c(profile_summary, profile_body, num_obs_nodes, nodes_new)

  write(profile_data,
        file = file.path(project.path, "PROFILE.DAT"),
        append = F)

} #end fn
