#' READ outputs of NOD_INF.OUT
#'
#' Original from hydrusR package. Needed changes to read dual permeability outputs from Hydrus which have slightly different structure than single
#' porosity files. This version should be flexible to any type.
#' @param project.path Path to the H1D project. Where is the out.file saved?
#' @param out.file Name of the Nod_Inf.out file, in case saved to different name
#' @param output Vector of output types to be read (e.g., "Head", "Moisture", "Flux"). This is only used if drop.cols = TRUE, otherwise all columns
#' will be returned.
#' @param ...
#' @author Subodh Acharya <https://github.com/shoebodh>; Trevor Baker <tbaker@iegconsulting.com>
#' @return data.table of Nod_Inf.out results for all timesteps.
#' @export

read.nod_inf<- function(project.path,
                        out.file = "Nod_Inf.out",
                        output = c("Head", "Moisture", "K", "C", "Flux", "Sink",
                                   "Kappa", "v/KsTop", "Temp"),
                        drop.cols = FALSE, ...){

  warn.in <- options("warn")$warn #save incoming setting to put it back later

  #two edits needed to be made here from original function
  # - 1 - Hydrus 4.17 dual perm nod_inf.out file does not have column names for all, and therefore fread does not recognize the header row
  # - 2 - dual perm has a different file header than single porosity. there is one extra row, so a single value for fread's skip argument
  #         cannot be assumed. each file is read in full, gets its header row recognized (e.g. by finding text 'Node'), then is fread again
  #         with the proper skip argument.

  #check where first node row is - where to start the file reading, because single porosity models are different than
  # dual permeability (less rows in file header)
  node.read <- data.table::fread(input = file.path(project.path,
                                                   out.file),
                                 fill = TRUE,
                                 blank.lines.skip = FALSE,
                                 skip = 0)
  first.node <- which(node.read[,1] == "Node")[1]
  col.heads <- node.read[first.node,]
  col.heads <- as.character(col.heads)
  #seems that Temp is always last, and in some cases a blank column past Temp is detected and an error occurs trying to assign
  # a name to it later on.
  if(any(col.heads == "Temp")){ col.heads <- col.heads[1:which(col.heads == "Temp")] } #if any named Temp, cut it off there.
  blank.heads <- which(col.heads == "" | col.heads == "NA" |is.na(col.heads))
  #I know the last column is Temp. No idea what the other one is. I will name it Unknown rather than drop it.
  # This part is potentially very messy so user will be warned.
  if(length(blank.heads)>0){
    warning("Hydrus returned blank column names. These are filled with argument 'blank.names', which may not be correct.")
    blank.names <- c("Unknown", "Temp") #have only seen where there are two blanks, but lines below allow flexibility
    if(length(blank.names) != length(blank.heads)){
      #It seems Temp is always last. Fill others with Unknown#.
      blank.names <- c(paste0("Unknown",100:1), "Temp") #100 reps so it is always too long
      #this is ugly but it means that Temp is always last and a flexible number of Unknowns are added prior
      blank.index <- (length(blank.names)-length(blank.heads)+1) : (length(blank.names))
      blank.names <- blank.names[blank.index]
    }
  }


  #this is used to read in files with the correct skip argument. because of blank column names, I need to use the above process.
  # read in the data, which is separated into tables, one for each time. these tables get combined in the subsequent steps to make a
  # proper dataset.
  nod_inf0 = data.table::fread(input = file.path(project.path,
                                                 out.file),
                               fill = TRUE,
                               header = TRUE,
                               blank.lines.skip = FALSE,
                               skip = first.node-1)
  if(length(blank.heads)>0){
    names(nod_inf0)[blank.heads] <- blank.names
  }

  #find rows with timestamps, make a vector of times
  time_lines = nod_inf0[grepl("Time:", nod_inf0[["Node"]]), ]
  times = c(0, as.numeric(time_lines$Depth)) #these are the times. zero is appended because the original time=0 line is cutoff when file is read.
  #change all columns' values to numeric - this will set blanks and text (e.g. 'end') to NA
  nod_inf <- nod_inf0 #keep copy of the original - for debug, not necessary in the final copy
  options(warn = -1) #never want to see the NA warnings here
  for (col in colnames(nod_inf0)) data.table::set(nod_inf, j = col, value = as.numeric(nod_inf[[col]]))
  options(warn = warn.in)

  #nod_inf = na.omit(nod_inf) #drop NAs to leave only data rows

  ##trim time column for any data rows lost by na.omit
  #nod_inf[, `:=`(Time, rep(times, each = length(nodes)))] #make a new column of times

  #new code. the method above faisl if there are NAs anywhere in the data.
  nod_inf = na.omit(nod_inf) #drop NAs to leave only data rows
  nodes = sort(unique(nod_inf[["Node"]])) #get unique Node numbers
  time.vec <- rep(times, each = length(nodes))
  time.vec <- time.vec[1:nrow(nod_inf)]
  nod_inf$Time <- time.vec
  nod_split = split(nod_inf, f = nod_inf$Time) #split table intoa list, one element per time
  nrow_split = sapply(nod_split, nrow) #rows of each dataset per time

  #some of the times have more rows than nodes. this is because of the printing resolution (# decimals) of the timestamps,
  # which doesn't appear to be editable. the steps below just keep the first 1:len(nodes) rows, which makes sense since
  # these data are the closest to the printed timestamp (e.g. 0.00000012 and 0.00000018 hours may be both printed as 0.0000001, which
  # is closer to the first value). this might be solvable by changing time units in Hydrus (e.g. use minutes not hours), but
  # for this function, all I can do is warn.
  extra_index = which(nrow_split > length(nodes))
  if(length(extra_index)>0){
    warning(paste0("More than one results table per timestep at times: ",
                  paste(unique(times)[extra_index], collapse = ", "),
                  ". First table will be kept."))
  }
  for (i in extra_index) {
    nod_split[[i]] = nod_split[[i]][1:length(nodes), ] #keep only first set per duplicated
  }
  nod_inf = data.table::rbindlist(nod_split) #bind back together into table
  if(drop.cols){
    if(is.null(output)){
      output <- c("Head", "Moisture", "K", "C", "Flux", "Sink","Kappa", "v/KsTop", "Temp")
    }
    output_names = intersect(output, colnames(nod_inf)) #these are the ones to keep
    output_names = unique(c("Time", "Node", "Depth", output_names)) #plus these standards
    nod_out = nod_inf[, .SD, .SDcols = output_names]  #now subset columns
  } else {
    #if not dropping any, then just move Time to the front
    output_names = colnames(nod_inf)
    output_names = output_names[-which(output_names == "Time")]
    output_names = c("Time", output_names) #move Time to front
    nod_out = nod_inf[, .SD, .SDcols = output_names]  #rearrange columns
  }

  return(nod_out)

}
