#' Write MATER.IN file of tabular theta-h-K data.
#'
#' This can be used rather than providing Hydrus with van Genuchten-Mualem parameters. This function is able to use a dataframe (df.texture) with
#' texture information to generate tables via ROSETTA, but can also be given the ready-made dataframe of theta, h, and K
#' values.
#' @param project.path the path to your Hydrus project.
#' @param df.val dataframe with theta, h, and K columns, and an 'id' column if more than one material is represented. all data must be in project
#' length and time units.
#' @param df.texture dataframe with sand, silt, clay, and (optional) Db columns, in that order. 3 or 4 column, any number of rows. These will be run
#' through version 3 of ROSETTA to get van Genuchten-Mualem parameters to generate theta-h-K tables.
#' @param TimeUnit
#' @param SpaceUnit
#' @return nothing returned to console. MATER.IN file is written to project.path
#' @export

write.mater.in <- function(project.path,
                           df.val,
                           df.texture = data.frame(sand = 70, silt = 20, clay = 5),...){

                           # TimeUnit,
                           # SpaceUnit
  #cm/d units converted to project units:
  T.multiple <- c(24*60*60, 24*60, 24, 1)[which(c("seconds","minutes","hours","days") == TimeUnit)]
  L.multiple <- c(10, 1, 0.01)[which(c("mm","cm","m") == TimeUnit)]
  LT.multiple <- L.multiple / T.multiple
  #example
  #T.project <- T.days * T.multiple
  #L.project <- L.cm  * L.multiple
  #LT.project <- LT.cmd * LT.multiple


  #if no df.val given, then create from ROSETTA
  if(is.null(df.val)){
    if(is.null(rosetta.vals)) stop("Must give either df.val or rosetta.vals")



    vars <- names(df.texture)

    vg <- soilDB::ROSETTA(df.texture, vars = vars, v = "3")
    names(vg)[which(is.na(names(vg)))] <- "k0"
    names(vg)[which(names(vg) == "NA.1")] <- "L"
    soil_para <- lapply(1:nrow(vg), function(x){
      this <- vg[x,]
      out.list <- list(thr = this$theta_r,
                       ths = this$theta_s,
                       Alfa = 10^this$alpha,
                       n = 10^this$npar,
                       Ks = 10^this$ksat * LT.multiple,
                       l = this$L)
      out.list <- lapply(out.list, function(x) round(x,4) )
      return(out.list)
    })

    #df.val is what I will pass to Hydrus
    df.val <- data.frame(theta = NA_real_,
                         h_cm = rep(su(c(-1e5, as.vector(sapply(c(-1,-2,-5), function(x){ x*10^(-3:4) })))), nrow(vg)),
                         K = NA_real_,
                         id = NA_real_)
    df.val$id <- as.vector(sapply(1:nrow(vg), function(x){ rep(x, nrow(df.val)/nrow(vg)) })) #for pulling correct vg params below
    df.val$h_kPa <- df.val$h_cm * -0.0981
    df.val$theta <- as.vector( sapply(1:nrow(vg), function(x) { IEGsoil::VWC_T.vG(vg[x,],
                                                                                  df.val$h_kPa[which(df.val$id == x)]) }) )
    df.val$K <- as.vector( sapply(1:nrow(vg), function(x) { IEGsoil::K_T.vG(vg[x,],
                                                                            df.val$h_kPa[which(df.val$id == x)],
                                                                            ksat.col = "ksat") }) )
    #conversions to project units
    df.val$h <- df.val$h_cm * L.multiple
    df.val$K <- df.val$K * LT.multiple


    # plot(-df.val$h, df.val$theta, log = "x") #check SWCC
    # plot(-df.val$h, df.val$K, log = "xy") #check K-h
    df.val <- df.val %>%
      #drop h_kPa was only needed for calculating
      # drop h_cm because h is now in project units , as required.
      dplyr::select(-h_kPa, -h_cm) %>%
      #put highest theta at top to match user manual
      dplyr::arrange(id, dplyr::desc(theta))

  } #end section to mkae df.val from df.texture


  ###########
  #all data formats get to here with same df.val.
  # - check minimum columns
  if(!all(c("theta","h","K") %in% names(df.val))){
    stop("df.val must have minimum columns: 'theta', 'h', and 'K' ")
  }
  if(!("id" %in% names(df.val))){
    cat("No 'id' column in df.val. Assuming all are same material (id = 1 for all). Check your data.")
    df.val$id <- 1
  }


  #there are 4 parts to a Mater.in file
  # 0) the uppermost 2 lines, does not get repeated for each material
  piece0 <- sprintf(fmt = rep("%6s",2), c("iCap", "0"))
  # 3) last line of the file is 'end'
  piece3 <- sprintf(fmt = "%4s", "end")
  # plus pieces 1 and 2 that get set in each loop below

  mat.list <- NULL
  for(xx in 1:lu(df.val$id)){

    this.id <- u(df.val$id)[xx]
    df.v <- df.val[which(df.val$id == this.id),]
    df.v <- df.v[,-which(names(df.v)=="id")] #don't need id column from here.


    #for each material, these lines repeat:
    # 1) header, 3 lines
    #   - 2 lines are "NTab"; nrow(data)
    piece1a <- sprintf(fmt = rep("%6s",2), c("NTab", nrow(df.v)))
    #   - 4th line is table header: theta, h, K
    piece1b <- sprintf(fmt = rep("%10s",3), c("theta", "h", "K"))
    piece1b <- paste(piece1b, collapse = "")
    piece1 <- c(piece1a, piece1b)
    #piece 2 is the formatted data, which is prepped below into val_mat_fmt


    #set up the table block's structure
    nrows <- nrow(df.v)
    df.fmt <- df.v
    df.fmt$theta <- as.character(round(df.v$theta, 4)) #MATER.IN formatting in user manual has 4 decimals. I am seeing if 5 will work to reduce duplicate theta values
    df.fmt$h <- hydrusR::format2sci(df.v$h, 2, 2)
    df.fmt$K <- hydrusR::format2sci(df.v$K, 2, 2)
    df.fmt <- df.fmt[,c("theta","h","K")]
    v1 <- unlist(df.fmt)
    val_mat = matrix(v1, nrow = nrows, ncol = 3, byrow = FALSE) #set up matrix for the main chunk of values

    #format the matrix
    #fmt_vec0 = c("%11.0f", rep("%12.0f", 5)) #the standard spacing for a line
    fmt_vec0 <- rep("%10s", 3)
    val_mat_fmt <- val_mat #make a copy of the matrix that will be formatted below
    for(p in 1:nrow(val_mat_fmt)){
      #adjust formatting if time step has decimals
      #this.decimals <- sapply(val_mat[p,], get.decimalplaces)
      #fmt_vec <- fmt_vec0 #preserve original for next iteration
      #fmt_vec = sapply(1:length(this.decimals), #change number formatting for correct number of decimals
      #                 function(x){ gsub(pattern = "0", replacement = this.decimals[x], fmt_vec[x]) })
      val_mat_fmt[p, ] = sprintf(fmt = fmt_vec0[1:ncol(val_mat)], val_mat[p, ])
    }
    val_mat_fmt = apply(val_mat_fmt, MARGIN = 1, FUN = paste, collapse = "") #paste each row into one string

    #output the header plus the values
    mat.list[[xx]] <- c(piece1, val_mat_fmt)

  } #end xx loop for all samples


  #unlist all of the per-material pieces
  mats <- do.call(c, mat.list)




  #put MATER.IN together from its pieces:
  new_mater.in <- c(piece0, #the top iCap header
                    mats, #all the material tables
                    piece3) # "end"


  #write it to the folder for the R project and for the Hydrus GUI project
  write(new_mater.in, file = paste0(project.path,"/MATER.IN"), append = F)

} #end mater.in fn
