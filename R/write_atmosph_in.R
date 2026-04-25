#' Write atmospheric boundary condition inputs
#'
#' This writes ATMOSPH.IN which contains time-variable boundary conditions, including precipitation, potential evaporation and transpiration, boundary
#' heads and boundary fluxes. This function does not need to be run for all simulations, but most need it. It is not needed if the upper boundary
#' condition is either a constant head or constant flux (set in write.bottom.bc) and the lower boundary condition is either a constant head or
#' constant flux (set in write.bottom.bc) or is free draining (set in write.sim.settings). If the simulation has precipitation or evapotranspiration,
#' then this needs to be run.
#' @param project.path Hydrus project path.
#' @param atm.bc.data dataframe containing all time-variable atmosphere records. must have column 'tAtm', which is the time of each record. Full list
#' of columns is given in Hydrus manual Table 12.9. Currently accepted columns are listed next. Not all of these must be given for every simulation,
#' only those that are Prec (precipitation rate, L/T), rSoil (potential evaporation rate,
#' L/T), rRoot (potential transpiration rate, L/T), hCritA (minimum allowed pressure head at soil surface, dryness limit, L), rB (bottom flux rate,
#' L/T), hB (pressure head at soil bottom, L), hT (pressure head at soil top, L).
#' @param hCritS numeric, length = 1. the maximum allowed surface head. in project length units. Default = 0, meaning that watrer isn't allowed to
#' pond on the surface. Set this value > 0 if ponding is to be allowed. Do not set this < 0.
#' @export

write.atmosph.in <- function(project.path,
                             atm.bc.data,
                             hCritS = 0, ...){

  print("write.atmosph.in: this should check SELECTOR Block B for correct codes.")

  #load template
  out.file = "ATMOSPH.IN"
  # default.filename = "ATMOSPH.IN"
  template_atmosph_in = system.file("templates/ATMOSPH.IN", package = "hydrusR")
  atm_data = readLines(con = template_atmosph_in, n = -1L, encoding = "unknown")

  #remove pre-existing ATMOSPH file
  if(file.exists(file.path(project.path, out.file))){
          file.remove(file.path(project.path, out.file))
  }

  # #TDB: commenting this out. I think it is supposed to be setting the radiation extinction coefficient 'rExtinct', but in their
  # # template they seem to have it misnamed as Extinction. I could change template but this is only needed if PET is being calculated
  # # from met data and then needs to be split into rSoil (E) and rRoot (T) components by Hydrus. For now, I am specifying rRoot and rSoil directly.
  # #Also note that the variable called LAI here (formerly an argument of the fn) appears to be an alternate extinction coeff to the Hydrus default of 0.463, not an LAI value.
  # extinction_ind = grep("Extinction", atm_data)
  # if(input.pet == TRUE){
  #   atm_data[(extinction_ind + 1)] = sprintf("%8s", LAI) #replace value
  # } else {
  #   atm_data = atm_data[-c(extinction_ind, extinction_ind + 1)] #drop the coeff because PET is not being input
  # }
  #
  #actually I will just extinct it:
  extinction_ind = grep("Extinction", atm_data)
  if(length(extinction_ind)>0){
    atm_data <- atm_data[-(extinction_ind:(extinction_ind+1))]
  }

  #find row to set the number of records
  maxAL_ind = grep("MaxAL", atm_data)
  atm_data[maxAL_ind + 1] <- sprintf("%7.0f", nrow(atm.bc.data)) #set it by nrow of the dataframe given

  #ensure lLay is FALSE. Do not need Hydrus to split PET, I have rRoot and rSoil already
  lL_ind <- grep("lLay", atm_data)
  lL_head <- atm_data[lL_ind]
  lL_head_split <- strsplit(lL_head, " ")[[1]]
  lL_head_split <- lL_head_split[-which(lL_head_split == "")]
  lL_posn <- which(lL_head_split == "lLay")
  if(length(lL_posn)>0){ #lLay may not always be found, so put this in an if()
    lL_dat <- atm_data[lL_ind+1] #find which one is lLay
    mm <- gregexpr("\\S+", lL_dat)[[1]][lL_posn] #indexes of none blank characters. I want the one that marks the lLay value's position
    lL_dat_start0 <- substr(lL_dat,1, mm-1) #take the string from beginning to one before lLay posn
    lL_dat_start <- paste0(lL_dat_start0,"f") #force the value false
    lL_dat_good <- gsub(paste0("^",lL_dat_start0,"."), lL_dat_start, lL_dat, perl = T) #sub the initial string with any t/f value for the fixed string that has f for lLay
    atm_data[lL_ind+1] <- lL_dat_good #replace values
  }

  #replace hCritS value
  hcrits_ind = grep("hCritS", atm_data)
  atm_data[hcrits_ind + 1] = sprintf("%7.0f", hCritS)

  #find atm table indices for subbing finished table in later
  tAtm_ind = grep(" tAtm", atm_data) #this is the row of the table header
  end_line = atm_data[grep("end", atm_data)] #the line below the last data row

  #these are the currently supported variable names:
  bc_data_vars = c("tAtm", "Prec", "rSoil", "rRoot",  "hCritA", "rB", "hB", "ht")
  #these need to be arranged to match the order in SELECTOR.IN
  sel.header0 <- atm_data[tAtm_ind]
  sel.header <- strsplit(sel.header0, " ")[[1]]
  sel.header <- sel.header[-which(sel.header == "")]
  #re-arrange bc_data_vars, as these will dictate the processing below
  match.indx <- match(sel.header, bc_data_vars)
  if(any(is.na(match.indx))){
    #these are the variables in sel.header that the current data doesn't have. drop them from sel.header
    drop.var <- sel.header[which(is.na(match.indx))]
    match.indx <- match.indx[-which(is.na(match.indx))] #now drop the NAs and use this to re-order bc_data_vars
    #drop it from the original row
    for(xx in 1:length(drop.var)){
      #extra steps to preserve spacing
      # drop the correct number of spaces
      sel.posn <- which(sel.header == drop.var[xx])
      start.posn <- if(sel.posn == 1){ 1 } else {
        gregexpr(sel.header[sel.posn-1], sel.header0)[[1]][1] + nchar(sel.header[sel.posn-1])
      }
      end.posn <- if(sel.posn == length(sel.header)){ nchar(sel.header0) } else {
        gregexpr(sel.header[sel.posn], sel.header0)[[1]][1] + nchar(sel.header[sel.posn]) - 1
      }
      old.str <- substr(sel.header0, start.posn, end.posn)
      sel.header0 <- gsub(old.str,
                           "",
                           #paste(rep(" ",nchar(drop.var[xx])),collapse = ""), #replace every character with a blank space to keep spacing format
                           sel.header0)
    }
  }
  #subset to just the matches in the header
  bc_data_vars <- bc_data_vars[match.indx]
  #replace the header row after possibly dropping some not present in this data
  atm_data[tAtm_ind] <- sel.header0

  if(any(is.na(match(names(atm.bc.data), bc_data_vars)))){
    col.miss <- which(is.na(match(names(atm.bc.data), bc_data_vars)))
    name.miss <- names(atm.bc.data)[col.miss]
    for(a in length(name.miss)){
      header.line <- atm_data[tAtm_ind]
      header.line <- gsub(paste0(" *",name.miss[a]), "", header.line) #gsub out the name plus the preceding spaces
      atm_data[tAtm_ind] <- header.line
    }
    cat(paste0("write.atmosph.in: There are columns in atm.bc.data that are not currently supported. Check names or update code: ",
               paste(name.miss, collapse = ", "), "\n"))
  }

  #make a copy that is trimmed down to valid columns
  bc_data_new = atm.bc.data %>%
    dplyr::select(dplyr::any_of(bc_data_vars))
  row.names(bc_data_new) <- NULL

  fmt_vec0 = c("%11.0f", "%12.3f", "%12.4f", "%12.4f", "%12.0f", rep("%12.4f",8)) #rep 8 so it is always too long instead of too short

  bc_data_fmt <- lapply(1:nrow(bc_data_new), function(x){ unname(unlist(bc_data_new[x,])) }) #make copy as a list, to be replaced line by line below
  for(a in 1:length(bc_data_fmt)) {
    this.dec <- get.decimalplaces(bc_data_new$tAtm[a]) #how many decimals in this timestep?
    fmt_vec <- fmt_vec0 #make copy to keep original intact
    fmt_vec[1] = sub(pattern = "0", replacement = this.dec, fmt_vec[1]) #adjust for this time step
    bc_data_fmt[[a]] = sprintf(fmt = fmt_vec[1:length(bc_data_fmt[[a]])], bc_data_new[a, ])
  }
  bc_data_fmt <- lapply(bc_data_fmt, function(x){ paste(x,collapse = "") }) #each line becomes one text string
  bc_data_fmt <- do.call(c, bc_data_fmt) #make into a vector
  # bc_data_fmt = bc_data_new #make copy to be replaced line by line below
  # for(a in 1:nrow(bc_data_fmt)) {
  #   this.dec <- get.decimalplaces(bc_data_new$tAtm[a]) #how many decimals in this timestep?
  #   fmt_vec <- fmt_vec0 #make copy to keep original intact
  #   fmt_vec[1] = sub(pattern = "0", replacement = this.dec, fmt_vec[1]) #adjust for this time step
  #   bc_data_fmt[a, ] = sprintf(fmt = fmt_vec[1:ncol(bc_data_fmt)], bc_data_new[a, ])
  # }
  #bc_data_fmt = apply(bc_data_fmt, MARGIN = 1, FUN = paste, collapse = "")

  #piece back together the whole file
  atm_input1 = atm_data[1:tAtm_ind]
  atm_input2 = bc_data_fmt
  atm_input3 = end_line
  atmosph_input_new = c(atm_input1, atm_input2, atm_input3)

  #save it
  atmosph_in_file = file.path(project.path, out.file)
  write(atmosph_input_new, file = atmosph_in_file, append = F)

}
