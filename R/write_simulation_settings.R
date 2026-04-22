#' Write simulation settings to SELECTOR.IN (Block B)
#'
#' Write settings controlling the number of iterations (MaxIt) and solver tolerances (TolTh, TolH), as well as defining simulation settings:
#' TopInf, WLayer, KodTop, InitCond, BotInf, qGWLF, FreeD, SeepF, KodBot, DrainF, hSeep.
#' @param project.path path to project, where SELECTOR.IN file exists already (e.g. made by create.h1d.project() )
#' @param MaxIt Integer, length = 1. Maximum number of iterations allowed during any time step
#' @param TolTh Absolute water content tolerance for nodes in the unsaturated part of the flow region. Hydrus' recommended value is 0.0001, Default
#' here is 0.001 to allow quicker simulations. TolTh is the maximum desired absolute change in the value of the water content, theta, between two
#' successive iterations during a particular time step.
#' @param TolH Absolute pressure head tolerance for nodes in the saturated part of the flow region. Hydrus' recommended value is 0.1 cm, and the
#' default given here is 1 to allow quicker solutions (beware that the value given must be in the project's lenght units and will not automatically
#' be in cm). TolH represents the maximum desired absolute change in the value of the pressure head, h, between two successive iterations during
#' a particular time step.
#' @param TopInf,BotInf logical, length = 1. Default TopInf=TRUE, BotInf=FALSE. TRUE = time-dependent boundary condition is to be imposed at
#' the top/bottom of the profile, for which data are supplied via input file ATMOSPH.IN (created by write_atmosph_in function).
#' @param KodTop,KodBot integer, length = 1. Codes specifying type of boundary condition (BC) for water flow at the surface and bottom of profile,
#' respectively. Code number is positive for Dirichlet BC (i.e., boundary is at prescribed pressure head) and negative for Neumann BC (i.e.,
#' prescribed volumetric flux entering/leaving. e.g., precip).  Set KodTop to 0 when a prescribed BC can change from
#' Dirichlet BC to Neumann BC and vice versa. \cr
#' KodTop/KodBot values: +1 = constant surface/bottom head; -1 = constant surface (incl. atmospheric)/bottom flux; +3 = variable surface/bottom head;
#' -3 = variable surface/bottom flux; -4 variable atmospheric BC (KodTop only).
#' @param WLayer logical, length = 1. Default T. TRUE = water can accumulate at the surface with zero surface runoff.
#' @param InitCond llogical, length = 1. Default F. Unsure. Might be IInitW from manual: TRUE if the initial condition is given in terms of theta,
#' FALSE if given in terms of the pressure head.
#' @param qGWLF logical, length = 1. Default F. TRUE = discharge-groundwater level relationship q(GWL) is applied as bottom boundary condition
#' @param FreeD logical, length = 1. Default T.  TRUE = free drainage is to be considered as bottom boundary condition, i.e. unity gradient at bottom of
#' profile, meaning bottom layer is in matric equilibrium with the underlying material.
#' @param SeepF logical, length = 1. Default F. TRUE = seepage face is to be considered as the bottom boundary condition.
#' @param DrainF logical, length = 1. TRUE = a drain is to be simulated by means of a boundary condition
#' @param hSeep numeric, length = 1. Default = 0. Pressure head (i.e., 0) that initiates flow over the seepage face bottom boundary.
#' @export

write.sim.settings <- function(project.path,
                                MaxIt = 100,
                                TolTh = 0.001,
                                TolH = 1,
                                TopInf = TRUE,
                                BotInf = FALSE,
                                WLayer = TRUE,
                                KodTop = -1,
                                KodBot = -1,
                                InitCond = FALSE,
                                qGWLF = FALSE,
                                FreeD = TRUE,
                                SeepF = FALSE,
                                DrainF = FALSE,
                                hSeep = 0) {


  #load SELECTOR.IN
  input.file <- file.path(project.path, "SELECTOR.IN")
  input_data = readLines(con = input.file, n = -1L, encoding = "unknown")


  #get indices for the 4 types of blocks a water flow model will have (unless there is root growth)
  basic_inf_ind  = grep("BLOCK A", input_data) # A. Basic Information
  flow_block_ind = grep("BLOCK B", input_data) # B. Water Flow Information - the one to be worked on in here.
  time_block_ind = grep("BLOCK C", input_data) # C. Time Information
  rwu_block_ind  = grep("BLOCK G", input_data) # G. Root Water Uptake Information
  if(any(grep("BLOCK D", input_data))){ # D. Root Growth Information
    stop("Root growth currently not coded. See fn notes.") } #I could code this in, but have not put the work in and wans't part of original hydrusR
  # other blocks won't be needed for my work:
  # E. Heat Transport Information
  # F. Solute Transport Information
  # K. Carbon Dioxide Transport Information
  # L. Major Ion Chemistry Information

  #read Block B
  flow_block <- input_data[flow_block_ind : (time_block_ind - 1)]
  flow_block_len0 <- length(flow_block) #need for resizing at the end.

  #this line is where MaxIt, TolTh, TolH are given
  tol_line_ind = grep("MaxIt", flow_block) #get index of header row where first settings are declared: MaxIt, TolTh, TolH
  tol_line = flow_block[tol_line_ind + 1]
  tol_fmt_vec = c("%5s", "%9s", "%7s") #spacing format
  tol_line_fmt = mapply(FUN = sprintf, c(MaxIt, TolTh, TolH), fmt = tol_fmt_vec) #format the param names
  tol_line_new = paste(tol_line_fmt, collapse = "") #make it a new line to be subbed back into the block below
  flow_block[tol_line_ind+1] <- tol_line_new

  #first line of settings
  set1_line_ind = grep("TopInf", flow_block) #get index of header row where first settings are declared: MaxIt, TolTh, TolH
  set1_line = flow_block[set1_line_ind + 1]
  set1_line_split = unlist(strsplit(set1_line, split = " ")) #need to split out the spaces from the values
  non_empty = which(set1_line_split != "") #these are the locations where values will be written
  set1_line_split[non_empty[1]] <- substr(casefold(TopInf),1,1)
  set1_line_split[non_empty[2]] <- substr(casefold(WLayer),1,1)
  set1_line_split[non_empty[3]] <- KodTop
  set1_line_split[non_empty[4]] <- substr(casefold(InitCond),1,1)
  set1_line_new = paste(set1_line_split, collapse = " ") #paste it back together to preserve spacing structure
  flow_block[set1_line_ind+1] <- set1_line_new #put the good values back in

  #second line of settings
  set2_line_ind = grep("BotInf", flow_block) #get index of header row where first settings are declared: MaxIt, TolTh, TolH
  set2_line = flow_block[set2_line_ind + 1]
  set2_line_split = unlist(strsplit(set2_line, split = " ")) #need to split out the spaces from the values
  non_empty = which(set2_line_split != "") #these are the locations where values will be written
  set2_line_split[non_empty[1]] <- substr(casefold(BotInf),1,1)
  set2_line_split[non_empty[2]] <- substr(casefold(qGWLF),1,1)
  set2_line_split[non_empty[3]] <- substr(casefold(FreeD),1,1)
  set2_line_split[non_empty[4]] <- substr(casefold(SeepF),1,1)
  set2_line_split[non_empty[5]] <- KodBot
  set2_line_split[non_empty[6]] <- substr(casefold(DrainF),1,1)
  set2_line_split[non_empty[7]] <- hSeep
  set2_line_new = paste(set2_line_split, collapse = " ") #paste it back together to preserve spacing structure
  flow_block[set2_line_ind+1] <- set2_line_new #put the good values back in

  #put input_data together from scratch to fit everything
  output_data <- c(input_data[1:(basic_inf_ind-1)],
                    input_data[basic_inf_ind : (flow_block_ind-1)],
                    flow_block, #sub new flow_block into this spot
                    input_data[time_block_ind : (rwu_block_ind-1)],
                    input_data[rwu_block_ind : length(input_data)])

  #write to SELECTOR.IN file
  write(output_data, file =  file.path(project.path, "SELECTOR.IN"), append = F)

}
