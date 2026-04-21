#' Write root water uptake variables
#'
#' These are the pressur eheads at which roots increase, decrease, or cease water uptake. Default values oif this function are for Hydrus' default setting,
#' which is a Feddes model with parameters for "Pasture (Wesseling, 1981)". Alternate values can be found by looking in the 'Root Water Uptake -
#' Water Stress Reduction' menu of the Hydrus GUI. This function is currently not coded to process an S-shaped model.
#' @param project.path your project path, where SELECTOR.IN is saved.
#' @param model 0 for Feddes, 1 for S-shape. Note that currently code only supports model = 0 and will stop if other value is given.
#' @param compensated.uptake Numeric, length = 1, value <= 1. Set equal to one for a noncompensated root water uptake and smaller than one for
#' compensated root water uptake. This function's default is the Hydrus default of 1, non-compensated. This parameter is labelled OmegaC in SELECTOR.IN.
#' @param P0,P2H,P2L,P3 numeric, length = 1, value < 0. In same length units as project. Default values for "Pasture (Wesseling, 1981)" are given
#' here with cm units. These values are, respectively, the pressure heads below which roots begin to extract water, fall below the maximum
#' transpiration rate r2H, fall below the maximum transpiration rate r2L, and cease to extract water (aka wilting point).
#' @param POptm numeric, length = number of materials, value < 0. Value of the pressure head, h2, below which roots start to extract
#' water at the maximum possible rate. Values must be given in the same order as the material hydraulic properties.
#' @param r2H,r2L potential transpiration rates, in project units of length/time, associated with pressure heads between P0 and P2H, and between P2H and P2L, respectively.
#' @author Trevor Baker <tbaker@iegconsulting.com>
#' @export

write.rwu.stress <- function(project.path,
                             model = 0,
                             compensated.uptake = 1,
                             P0 = -10, P2H = -200, P2L = -800, P3 = -8000,
                             POptm = -25,
                             r2H = 0.5, r2L = 0.1, ...){

  ########
  # check/fix args
  if(model != 0){
    stop("Currently not coded to process an S-shaped root water uptake model.")
  }
  if(compensated.uptake < 1){
    compensated.uptake <- 0
  } else if(compensated.uptake > 1){
    cat("The given value for compensated.uptake is > 1, and will be set to 1.")
    compensated.uptake <- 1
  }
  P0 <- -abs(P0)
  P2H <- -abs(P2H)
  P2L <- -abs(P2L)
  P3 <- -abs(P3)
  POptm <- -abs(POptm)


  #####
  # read data
  input_data = readLines(con = file.path(project.path, "SELECTOR.IN"),
                         n = -1L, encoding = "unknown")

  #get line indices of all blocks and the end
  block_lines <- which(grepl("\\*\\*\\* ", input_data))
  rwu_info_ind <- grep("BLOCK G", input_data[block_lines])
  rwu_start_ind <- block_lines[rwu_info_ind]
  rwu_end_ind <- block_lines[rwu_info_ind+1]-1


  ##########
  #edit Block G
  this_block = input_data[rwu_start_ind : rwu_end_ind]
  #this_block_len0 <- length(this_block) #need for resizing at the end.
  model_line_ind = grep("Model", this_block) #get index where model is declared. parameters are below this.

  #this line is where model number and OmegaC are declared
  model_line = this_block[model_line_ind + 1]
  model_line_split = unlist(strsplit(model_line, split = " ")) #need to split out the spaces from the values
  non_empty = which(model_line_split != "") #these are the locations where values will be written
  model_line_split[non_empty[1]] <- as.character(model)
  model_line_split[non_empty[2]] <- as.character(compensated.uptake)
  model_line_new = paste(model_line_split, collapse = " ") #paste it back together to preserve spacing structure
  this_block[model_line_ind+1] <- model_line_new #put the good values back in

  #this line is 1 of 2 where parameters are entered
  para_line_ind1 = grep("P0", this_block)+1 #the line below the one with param names
  para_name_fmt_vec1 = c("%9s", "%10s", "%10s", "%10s", "%12s", "%12s") #spacing format
  para_values1 <- c(P0, P2H, P2L, P3, r2H, r2L)
  para_line_fmt1 = mapply(FUN = sprintf, para_values1, fmt = para_name_fmt_vec1[1:length(para_values1)]) #format the param names
  para_line_new1 = paste(para_line_fmt1, collapse = "")#make it a new line to be subbed back into the block below
  this_block[para_line_ind1] <- para_line_new1

  #now line 2 of 2 that holds the POptm values
  para_line_ind2 = grep("POptm", this_block)+1 #the line below the one with param names
  para_name_fmt_vec2 = c("%8s", rep("%9s",100)) #spacing format. very long to ensure always longer than number of materials
  para_values2 <- POptm
  para_line_fmt2 = mapply(FUN = sprintf, para_values2, fmt = para_name_fmt_vec2[1:length(para_values2)]) #format the param names
  para_line_new2 = paste(para_line_fmt2, collapse = "") #make it a new line to be subbed back into the block below
  this_block[para_line_ind2] <- para_line_new2


  #now overwrite the original values with those set here
  input_data[rwu_start_ind : rwu_end_ind] <- this_block

  #save the file
  write(input_data,
        file =  file.path(project.path, "SELECTOR.IN"),
        append = F)

}
