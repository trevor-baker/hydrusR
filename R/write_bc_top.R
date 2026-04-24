#' Writes codes for top boundary conditions to SELECTOR.IN
#'
#' @param project.path path to the Hydrus project. where SELECTOR.IN file is saved.
#' @param atmos passed from create.bc function. logical - is atmospheric boundary data given? i.e., Precip, PET, rSoil, rRoot
#' @param constant.bc logical, length = 1. is this a time-variable (FALSE) or constant boundary (TRUE) condition? If this is a variable condition, then
#' additional data will need to be prepared for ATMOSPH.IN by write.atmopsh.in(). If this is a constant boundary condition, then no ATMOSPH.IN data needs
#' to be set.
#' @param bc.type character, length = 1. "flux" or "head". what type of boundary condition is being set?
#' @param bc.value numeric, length = 1. the value to be set for a constant flux condition at the profile surface, in project length/time units. Does not need to be
#' specified for constant head conditions because the Head value of the top Node in PROFILE.DAT will be used.
#' @author Subodh Acharya <https://github.com/shoebodh>; Trevor Baker <tbaker@iegconsulting.com>
#' @export

write.bc.top <- function(project.path,
                         constant.bc,
                         atmos,
                         bc.type,
                         bc.value) {

  #if there is atmos data, then there cannot also be top head data. one or the other. atmos takes precedent.
  # if it is atmos or flux it will go one way, if it is head it will go another
  if(atmos){
    bc.type <- "atmos"
  }
  if(bc.value == ""){
    bc.value <- NULL
  }


  ## Writes top boundary conditions "flux" or "head"
  input_data = readLines(con = file.path(project.path, "SELECTOR.IN"),
                         n = -1L, encoding = "unknown")
  flow_block_ind = grep("BLOCK B", input_data) #the top of the flow black being edited
  time_block_ind = grep("BLOCK C", input_data) #the bottom of the block is the start of the time block, Block C

  #get data from the flow block
  flow_block = input_data[flow_block_ind : (time_block_ind - 1)]
  topInf_ind = grep("TopInf", flow_block) #the header row above the values to be edited
  botInf_ind = grep("BotInf", flow_block) #the top row of 2 that will be preserved. BotInf header and value line belwo that
  rTop_ind = grep("rTop", flow_block) #the row of flux rates that may or may not exist
  hTab1_ind = grep("hTab1", flow_block) #the header row below the values


  botInf_data <- flow_block[botInf_ind+0:1]
  topInf_input = flow_block[topInf_ind+1] #the row of values starting with topInf
  topInf_input_split = unlist(strsplit(topInf_input, split = " "))
  topInf_input_split = topInf_input_split[topInf_input_split != ""]
  topInf_fmt_vec = c("%2s", "%6s", "%8s", "%8s", "%7s", "%7s", "%7s") #define the spacing format for this line

  if(bc.type %in% c("flux", "atmos")){

    if(isTRUE(constant.bc) |
       bc.type == "atmos"){ #this is confusing but Hydrus processes simulations with atmospheric data, no matter whether it is constant or variable, as
                             # constant fluxes in terms of entering 'f' and '-1' for TopInf and KodTop

      topInf_input_split[1] = 'f' #this is TopInf. TRUE would mean a time-dependent boundary condition is to be imposed at
      #                             the top of the profile, for which data would be supplied via input file ATMOSPH.IN in Prec (rT) or hT columns.
      #                             This created would be created by write_atmosph_in function.
      topInf_input_split[3] = '-1' #this is KodTop. -1 for constant flux
      #Kodtop values: +1 = constant head; -1 = constant flux;
      #               +3 = variable head; -3 = variable flux
      # - this is entered as -1 (constant) for simulaitons with atmos data, even if the flux (e.g. P or ET) is variable over time.

      #format the spacing and paste back together into one line
      topInf_input_fmt = sprintf(fmt = topInf_fmt_vec[1:length(topInf_input_split)], topInf_input_split)
      topInf_input_new = paste(topInf_input_fmt, collapse = "")

      #if this is a constant flux that is not P or ET, then its value gets entered as rTop in SELECTOR.IN
      # if it has a constant flux that is precipitation, then rTop is set as zero and the constant P is entered
      # in ATMOSPH.IN as a single value of 'Prec', which is given to all periods, and is entered simply on one row with
      # time = max(time) if there are no other variable fluxes. Likewise if the constant flux is evaporation (rSoil) or trans[iration
      # (rRoot), these are entered into ATMOSPH.IN.
      #
      #However a constant, non-atmospheric flux value needs to be entered in SELECTOR.IN as rTop

      #if this isn't atmospheric data, which is not entered as rTop, even if it is indeed one constant Prec or ET flux value for the whole simulation (which
      # would be entered as a single row of ATMOSPH.IN), then a top flux value (rate, L/T) needs to be entered.
      # - this should be flexible to whether the line exists already or not. i.e. being edited or being created.
      #two ways - one filling in a pre-existing line, the other making it new and inserting it
      if(length(rTop_ind)>0){

        if(atmos){ bc.value <- 0 } #fill zero if there is atmos data. overwrite whatever might be there already.

        #this is an edit of existing. don't want to overwrite because rTop or rRoot might be set by other fns and should be preserved
        newInf_head_fmt <- flow_block[rTop_ind] #the names. no formatting needed, just grab them
        old_r_values <- flow_block[rTop_ind+1]
        old_r_split <- unlist(strsplit(old_r_values, split = " "))
        new_r_values = old_r_split[old_r_split != ""]
        new_r_values[1] <- bc.value #posn 1 is the rTop position. pre-existing values are kept as-is.
        new_value_fmt = sprintf(fmt = c("%12s", "%13s", "%13s"), new_r_values)
        #new_value_fmt = sprintf(fmt = c("%12.6f", "%13.6f", "%13.6f"), new_r_values)
        new_bc_value_fmt = paste(new_value_fmt, collapse = "")

      } else {
        #don't bother making the line new if this has atmos data. the zero does not need to be forced by making anew row. only needs
        # to be entered if, as above, the line of rTop, rBot, rRoot exists already.
        if(!atmos){

          #making the line new
          new_input_names = sprintf(fmt = "%13s", c("rTop", "rBot", "rRoot"))
          new_input_values = as.character(c(bc.value, 0, 0)) #only assign non-zero value rTop, at index 1.
          # the others may be non-zero as well but they aren't covered by this fn. if they do no
          # exist already, then either the scrip to make them has yet to run, or it did run and they
          # weren't made intentionally. so either way, assigning zeroes to rBot and rRoot here is the right way.
          new_value_fmt = sprintf(fmt = c("%12s", "%13s", "%13s"), new_input_values)
          #new_value_fmt = sprintf(fmt = c("%12.6f", "%13.6f", "%13.6f"), new_input_values)
          new_bc_value_fmt = paste(new_value_fmt, collapse = "")
          newInf_head_fmt = paste(new_input_names, collapse = "")

        } else {

          #if there is atmos data, and we're not making a new line, then need to give these NULL so the c() below will work properly
          newInf_head_fmt <- NULL
          new_bc_value_fmt <- NULL

        } #end else
      } #end else, line needs to be made new


      #bind all of these parts together
      flow_block_new = c(flow_block[1:topInf_ind],
                         topInf_input_new, #topInf values
                         botInf_data,
                         newInf_head_fmt, #rTop names - these don't exist with top flux, because both constant and variable top fluxes are entered in atmos. there is no data to give here.
                         new_bc_value_fmt, #rTop values
                         flow_block[hTab1_ind:length(flow_block)])

    } else {
      #else this is not a constant boundary condition (i.e. it is variable)

      # -- even if this is a variable atmosph boundary, it will not be entered here; it will be entered as f and -1 above.
      topInf_input_split[1] = 't' #this is topInf. TRUE means a time-dependent boundary condition is to be imposed at
      #                             the top of the profile, for which data should be supplied via input file ATMOSPH.IN (created
      #                             by write_atmosph_in function).
      topInf_input_split[3] = '-3' #this is Kodtop. -3 for variable flux
      #Kodtop values: +1 = constant head; -1 = constant flux;
      #               +3 = variable  head; -3 = variable flux

      topInf_input_fmt = sprintf(fmt = topInf_fmt_vec[1:length(topInf_input_split)], topInf_input_split)
      topInf_input_new = paste(topInf_input_fmt, collapse = "")

      flow_block_new = c(flow_block[1:topInf_ind],
                         topInf_input_new,
                         flow_block[hTab1_ind:length(flow_block)])

      #if a simulation has variable flux data, even if it is not precipitation, it must be entered as 'Prec'. for a variable
      # bottom, there is an 'rB' column, but there is no 'rT' column for variable top. so if a site had 1 cm/hr of water being mechanically
      # pumped onto it for a period, or arriving via run-on, it would still be called 'Prec' in ATMOSPH.IN.
      cat("Time-variable top flux must be specified in the ATMOSPH.IN table under column 'Prec'.\n ")

    } #end else
  } #end if flux type
  ######################################

  if(bc.type == "head"){
    if(isTRUE(constant.bc)) {

      topInf_input_split[1] = 'f' #this is TopInf. TRUE would mean a time-dependent boundary condition is to be imposed at
      #                             the top of the profile, for which data would be supplied via input file ATMOSPH.IN (created
      #                             by write_atmosph_in function).
      topInf_input_split[3] = '1' #this is KodTop. 1 for constant top head
      #KodTop values: +1 = constant head; -1 = constant flux;
      #               +3 = variable head; -3 = variable flux

      #unlike with constant flux in the section above, a constant head value doesn't need to be specified here
      # because the initial assigned head value for the top Node in PROFILE.DAT will be kept.

    } else {
      #else this is a time-variable head

      topInf_input_split[1] = 't' #this is TopInf. TRUE would mean a time-dependent boundary condition is to be imposed at
      #                             the top of the profile, for which data would be supplied via input file ATMOSPH.IN (created
      #                             by write_atmosph_in function).
      topInf_input_split[3] = '3' #this is KodTop. 3 for variable head
      #KodTop values: +1 = constant head; -1 = constant flux;
      #               +3 = variable head; -3 = variable flux

      cat("Time-variable top head must be specified in the ATMOSPH.IN table as column 'hT'.\n")

    } #end else

    topInf_input_fmt = sprintf(fmt = topInf_fmt_vec, topInf_input_split)
    topInf_input_new = paste(topInf_input_fmt, collapse = "")

    #piece together the new flow block
    flow_block_new = c(flow_block[1:topInf_ind],
                       topInf_input_new,
                       botInf_data,
                       flow_block[hTab1:length(flow_block)])

  } #end if head type

  #bind the full file back together and save it
  input_data_new = c(input_data[1:(flow_block_ind-1)],
                     flow_block_new,
                     input_data[time_block_ind:length(input_data)])

  write(input_data_new,
        file = file.path(project.path, "SELECTOR.IN"),
        append = F)

} #end fn
