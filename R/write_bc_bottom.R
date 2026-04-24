#' Writes codes for bottom boundary conditions to SELECTOR.IN
#'
#' @param project.path path to the Hydrus project. where SELECTOR.IN file is saved.
#' @param freeD logical. is this a free-draining profile. i.e. untiy gradient at bottom.
#' @param constant.bc logical, length = 1. is this a time-variable (FALSE) or constant boundary (TRUE) condition? If this is a variable condition, then
#' additional data will need to be prepared for ATMOSPH.IN by write.atmopsh.in(). If this is a constant boundary condition, then no ATMOSPH.IN data needs
#' to be set.
#' @param bc.type character, length = 1. "flux" or "head". what type of boundary condition is being set?
#' @param bc.value numeric, length = 1. the value to be set for a constant flux bottom condition, in project length/time units. Does not need to be
#' specified for constant head conditions because the Head value of the bottom Node in PROFILE.DAT will be used.
#' @author Subodh Acharya <https://github.com/shoebodh>; Trevor Baker <tbaker@iegconsulting.com>
#' @export

write.bc.bottom <- function(project.path,
                            freeD = freeD,
                            constant.bc,
                            bc.type,
                            bc.value) {


  if(bc.value == ""){
    bc.value <- NULL
  }

  ## Writes bottom constant boundary conditions "flux" or "head"
  input_data = readLines(con = file.path(project.path, "SELECTOR.IN"),
                         n = -1L, encoding = "unknown")
  flow_block_ind = grep("BLOCK B", input_data) #the top of the flow black being edited
  time_block_ind = grep("BLOCK C", input_data) #the bottom of the block is the start of the time block, Block C

  #get data from the flow block
  flow_block = input_data[flow_block_ind : (time_block_ind - 1)]
  botInf_ind = grep("BotInf", flow_block) #the header row above the values to be edited
  rBot_ind = grep("rBot", flow_block) #the row of flux rates that may or may not exist
  hTab1_ind = grep("hTab1", flow_block) #the header row below the values

  botInf_input = flow_block[botInf_ind+1] #the row of values starting with BotInf
  botInf_input_split = unlist(strsplit(botInf_input, split = " "))
  botInf_input_split = botInf_input_split[botInf_input_split != ""]
  botInf_fmt_vec = c("%2s", "%6s", "%6s", "%6s", "%7s", "%7s", "%7s") #define the spacing format for this line

  if(bc.type == "flux"){

    if(isTRUE(constant.bc)) {

      botInf_input_split[1] = 'f' #this is BotInf. TRUE would mean a time-dependent boundary condition is to be imposed at
      #                             the top/bottom of the profile, for which data would be supplied via input file ATMOSPH.IN (created
      #                             by write_atmosph_in function).
      botInf_input_split[5] = '-1' #this is KodBot. -1 for constant flux
      #KodBot values: +1 = constant bottom head; -1 = constant bottom flux;
      #               +3 = variable bottom head; -3 = variable bottom flux

      #format the spacing and paste back together into one line
      botInf_input_fmt = sprintf(fmt = botInf_fmt_vec[1:length(botInf_input_split)], botInf_input_split)
      botInf_input_new = paste(botInf_input_fmt, collapse = "")

      #another row needs to be added where the flux rate is defined in rBot
      # - this should be flexible to whether the line exists already or not. i.e. being edited or being created.
      if(length(rBot_ind)>0){
        #this is an edit of existing. don't want to overwrite because rTop or rRoot might be set by other fns and should be preserved
        newInf_head_fmt <- flow_block[rBot_ind] #the names. no formatting needed, just grab them
        old_r_values <- flow_block[rBot_ind+1]
        old_r_split <- unlist(strsplit(old_r_values, split = " "))
        new_r_values = old_r_split[old_r_split != ""]
        new_r_values[2] <- bc.value
        new_value_fmt = sprintf(fmt = c("%12s", "%13s", "%13s"), new_r_values)
        #new_value_fmt = sprintf(fmt = c("%12.6f", "%13.6f", "%13.6f"), new_r_values)
        new_bc_value_fmt = paste(new_value_fmt, collapse = "")

      } else {
        #making the line new
        new_input_names = sprintf(fmt = "%13s", c("rTop", "rBot", "rRoot"))
        new_input_values = as.character(c(0, bc.value, 0)) #only assign non-zero value rBot. the others may be non-zero as well but they aren't covered by this fn.
        new_value_fmt = sprintf(fmt = c("%12s", "%13s", "%13s"), new_input_values)
        #new_value_fmt = sprintf(fmt = c("%12.6f", "%13.6f", "%13.6f"), new_input_values)
        new_bc_value_fmt = paste(new_value_fmt, collapse = "")
        newInf_head_fmt = paste(new_input_names, collapse = "")
      }

      flow_block_new = c(flow_block[1:botInf_ind],
                         botInf_input_new, #botInf values
                         newInf_head_fmt, #rBot names
                         new_bc_value_fmt, #rBot values
                         flow_block[hTab1_ind:length(flow_block)])

    } else {
      #else this is not a constant boundary condition
      botInf_input_split[1] = 't' #this is BotInf. TRUE would mean a time-dependent boundary condition is to be imposed at
      #                             the top/bottom of the profile, for which data would be supplied via input file ATMOSPH.IN (created
      #                             by write_atmosph_in function).
      botInf_input_split[5] = '-3' #this is KodBot. -3 for variable flux
      #KodBot values: +1 = constant bottom head; -1 = constant bottom flux;
      #               +3 = variable bottom head; -3 = variable bottom flux

      botInf_input_fmt = sprintf(fmt = botInf_fmt_vec[1:length(botInf_input_split)], botInf_input_split)
      botInf_input_new = paste(botInf_input_fmt, collapse = "")

      flow_block_new = c(flow_block[1:botInf_ind],
                         botInf_input_new,
                         flow_block[hTab1_ind:length(flow_block)])

      cat("Time-variable bottom flux must be specified in the ATMOSPH.IN table.\n ")

    } #end else
  } #end if flux type
  ######################################

  if(bc.type == "head"){
    if(isTRUE(constant.bc)){

      botInf_input_split[1] = 'f' #this is BotInf. TRUE would mean a time-dependent boundary condition is to be imposed at
      #                             the top/bottom of the profile, for which data would be supplied via input file ATMOSPH.IN (created
      #                             by write_atmosph_in function).
      botInf_input_split[5] = '1' #this is KodBot. 1 for constant bottom head
      #KodBot values: +1 = constant bottom head; -1 = constant bottom flux;
      #               +3 = variable bottom head; -3 = variable bottom flux

      #unlike with constant flux in the section above, a constant head value doesn't need to be specified here
      # because the initial assigned head value for the bottom Node in PROFILE.DAT will be kept.

    } else {
      #else this is a time-variable head

      botInf_input_split[1] = 't' #this is BotInf. TRUE would mean a time-dependent boundary condition is to be imposed at
      #                             the top/bottom of the profile, for which data would be supplied via input file ATMOSPH.IN (created
      #                             by write_atmosph_in function).
      botInf_input_split[5] = '3' #this is KodBot. 3 for variable bottom head
      #KodBot values: +1 = constant bottom head; -1 = constant bottom flux;
      #               +3 = variable bottom head; -3 = variable bottom flux

      #for reasons unknown, if the profile is free-draining, even though this is a variable head condition
      # - variable because the head at the bottom changes to match the pressur ehead of the lowermost layer
      # - head because it is the head that changes to match the lower layer to maintain the unity gradient.
      #however, Hydrus sets the values for freeD to f and -1, as though it were a constant flux. I need to just follow along:
      if(freeD){
        botInf_input_split[1] = 'f' #because Hydrus says so if this is freeD=TRUE.
        botInf_input_split[5] = '-1'
      } else {
        #else remind user
        # - this might not be nesscary because the etensive checks in create.bc might prevent a case where hB values weren't making
        #     it into ATMOSPH.IN
        cat("Time-variable bottom head must be specified in the ATMOSPH.IN table.\n")
      }



    } #end else

    botInf_input_fmt = sprintf(fmt = botInf_fmt_vec[1:length(botInf_input_split)], botInf_input_split)
    botInf_input_new = paste(botInf_input_fmt, collapse = "")

    #piece together the new flow block
    flow_block_new = c(flow_block[1:botInf_ind],
                       botInf_input_new,
                       flow_block[hTab1_ind:length(flow_block)])
  } #end if head type

  #bind the full file back together and save it
  input_data_new = c(input_data[1:(flow_block_ind-1)],
                     flow_block_new,
                     input_data[time_block_ind:length(input_data)])

  write(input_data_new,
        file = file.path(project.path, "SELECTOR.IN"),
        append = F)

}
