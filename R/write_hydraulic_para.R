#' Write soil hydraulic parameters to selector.in
#'
#' @param project.path path to project, where SELECTOR.IN file exists already (e.g. made by create.h1d.project() )
#' @param model hydraulic model to use. integer, 0-7. 0 = "van Genuchten (VG)", 1 = "van Genuchten 6 para (VGM)", 2 = "Brooks Corey(BC)", 3 = "mod van Genuchten (VGM2)",
#' 4 = "Kosugi(KOS)", 5 = "dual Porosity (DUR)", 6 = "dual porosity 2 (DUR2)", 7 = "dual porosity 3 (DUR3)"
#' @param hysteresis include hysteresis in simulation? integer. 0 = No, 1 = Yes.
#' @param para list of hydraulic parameters, one entry per material. each entry is a named list with all parameters expected by the chosen model. \cr
#' 0 = c("thr", "ths", "Alfa", "n", "Ks", "l") \cr
#' 1 = c("thr", "ths", "Alfa", "n", "Ks", "l", "thm", "tha", "thk", "Kk") \cr
#' 2 = c("thr", "ths", "Alfa", "n", "Ks", "l") \cr
#' 3 = c("thr", "ths", "Alfa", "n", "Ks", "l") \cr
#' 4 = c("thr", "ths", "Alfa", "n", "Ks", "l") \cr
#' 5 = c("thr", "ths", "Alfa", "n", "Ks", "l", "w2", "Alfa2", "n2") \cr
#' 6 = c("thr", "ths", "Alfa", "n", "Ks", "l", "thrIm", "thsIm", "Omega") \cr
#' 7 = c("thr", "ths", "Alfa", "n", "Ks", "l", "thrIm", "thsIm", "AlfaIm", "nIm", "Omega")
#' @param vals list of dataframes containing head, K, and theta values. one list entry per material. each entry has a 3 column dataframe in the
#' project's length and time units, with names head, k, and theta, giving hydraulic conductivity (k) and volumetric water content (theta) values for
#' head values (matric potentials) ranging from 0 to at least 1e5 cm.
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
write.hydraulic.para<- function(project.path,
                                model = 0,
                                hysteresis = 0,
                                para,
                                vals, ...) {

  # #names of the permitted models - not used but keep for explicit IDs
  # smr_models<- c("van Genuchten (VG)",
  #                "van Genuchten 6 para (VGM)",
  #                "Brooks Corey(BC)",
  #                "mod van Genuchten (VGM2)",
  #                "Kosugi(KOS)",
  #                "dual Porosity (DUR)",
  #                "dual porosity 2 (DUR2)",
  #                "dual porosity 3 (DUR3)")
  # hydraulic_models <- data.frame(code = c(0:7),
  #                                model = smr_models,
  #                                stringsAsFactors = F)

  #make a dataframe of para
  if(!is.null(para)){
    if(length(para)==1){ #if only one material then this needs to be processed with cbind
      df.para <- do.call(cbind.data.frame, para)
    } else {
      df.para <- do.call(rbind.data.frame, para)
    }
  } else {
    if(is.null(vals)){
      stop("Must give either para or vals.")
    }
  }

  #parameter names for each model type
  para_name_list = list("0" = c("thr", "ths", "Alfa", "n", "Ks", "l"),
                        "1" = c("thr", "ths", "Alfa", "n", "Ks", "l", "thm", "tha", "thk", "Kk"),
                        "2" = c("thr", "ths", "Alfa", "n", "Ks", "l"),
                        "3" = c("thr", "ths", "Alfa", "n", "Ks", "l"),
                        "4" = c("thr", "ths", "Alfa", "n", "Ks", "l"),
                        "5" = c("thr", "ths", "Alfa", "n", "Ks", "l", "w2", "Alfa2", "n2"),
                        "6" = c("thr", "ths", "Alfa", "n", "Ks", "l", "thrIm", "thsIm", "Omega"),
                        "7" = c("thr", "ths", "Alfa", "n", "Ks", "l", "thrIm", "thsIm", "AlfaIm", "nIm", "Omega"))

  #read in the SELECTOR file. needs to be made already by create.h1d.project()
  input_data = readLines(con = file.path(project.path, "SELECTOR.IN"),
                         n = -1L,
                         encoding = "unknown")

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
  flow_block = input_data[flow_block_ind : (time_block_ind - 1)]
  flow_block_len0 <- length(flow_block) #need for resizing at the end.
  model_line_ind = grep("Model", flow_block) #get index where model is declared. parameters are below this.
  #input_model_name = hydraulic_models[hydraulic_models$code == model, "model"] #name of the model - not used again, delete?
  input_para_name = para_name_list[[as.character(model)]] #params expected by this model

  stopifnot(identical(input_para_name, names(df.para)))

  #this line is where model number and hysteresis are declared
  model_line = flow_block[model_line_ind + 1]
  model_line_split = unlist(strsplit(model_line, split = " ")) #need to split out the spaces from the values
  non_empty = which(model_line_split != "") #these are the locations where values will be written
  model_line_split[non_empty[1]] <- as.character(model)
  model_line_split[non_empty[2]] <- as.character(hysteresis)
  model_line_new = paste(model_line_split, collapse = " ") #paste it back together to preserve spacing structure
  flow_block[model_line_ind+1] <- model_line_new #put the good values back in

  #this line is where parameters are entered
  para_line_ind = grep("thr|ths", flow_block) #the line with param names
  para_name_fmt_vec = c("%6s", "%8s", "%8s", "%7s", "%11s", "%8s", "%9s", "%8s", "%11s", "%8s") #spacing format
  para_line_fmt = mapply(FUN = sprintf, input_para_name, fmt = para_name_fmt_vec[1:length(input_para_name)]) #format the param names
  para_line_new = paste(para_line_fmt, collapse = "") #make it a new line to be subbed back into the block below
  flow_block[para_line_ind] <- para_line_new

  # #old code that only permitted one material
  # para_values =  unlist(para[input_para_name])
  # value_format_vec = c("%7.4f", "%8.4f", "%8.4f", "%8.3f", "%11.5f", "%8.2f", "%8.3f", "%8.3f", "%11.3f", "%8.3f")
  # para_values_fmt = sprintf(fmt = value_format_vec[1:length(para_values)], para_values)
  # para_values_new = paste(para_values_fmt, collapse = "")


  value_format_vec = c("%7.4f", "%8.4f", "%8.4f", "%8.3f", "%11.5f", "%8.2f", "%8.3f", "%8.3f", "%11.3f", "%8.3f") #spacing format for parameter values
  #lapply through rows of df.para and apply formatting
  para_values_list <- lapply(1:nrow(df.para), function(x){
    para_val_fmt = sprintf(fmt = value_format_vec[1:ncol(df.para)], df.para[x,])
    para_val_new = paste(para_val_fmt, collapse = "")
  })
  para_values_new <- do.call(c, para_values_list)
  val.index <- (para_line_ind+1):(para_line_ind+1+(length(para_values_new)-1)) #flexible index because can be > 1 set of values
  flow_block[val.index] <- para_values_new

  if(length(flow_block) == flow_block_len0){
    # #old code that assumed always one row of param values
    input_data[flow_block_ind : (time_block_ind - 1)] <- flow_block
  } else {
    #put input_data together from scratch to fit everything
    input_data <- c(input_data[1:basic_inf_ind],
                    input_data[(basic_inf_ind+1) : flow_block_ind],
                    flow_block, #sub new flow_block into this spot
                    input_data[(time_block_ind+1) : rwu_block_ind],
                    input_data[(rwu_block_ind+1) : length(input_data)])
  }

  #write to SELECTOR.IN file
  write(input_data, file =  file.path(project.path, "SELECTOR.IN"), append = F)

}
