#' @Description Reads observation point outputs of h, theta and flux, that is produced in the "Obs_Node.out" file
#' @title Read observation point outputs
#' @param project.path Directory of the model input/outputs
#' @param out.file   The output file name. Default is "Obs_Node.out", produced by the Hydrus 1D program
#' @param obs.output The output to be returned.Default = NULL, i.e., all the outputs are read
#'                   Other options are "h", "theta", "Flux". example, obs.output = c("h", "theta")
#' @param ...
#' @export
#' @examples
#' parent.dir <- "C:/Users/t/IEG Dropbox/Trevor Baker/Integral Ecology/calculation tools/awsc/r scripts/ieg awsc/under development/vG PTF/Hydrus/"
#' project.path <- paste0(parent.dir,"plotA")
#' oo <- read.obs_node(project.path)

read.obs_node <-  function(project.path, out.file = "Obs_Node.out",
                           obs.output = NULL, ...) {
      # obs_node_out = read.table(file.path(simulation.path, "Obs_Node.out"),
      #                           header = F, sep = "", dec = ".",
      #                           na.strings = "NA", colClasses = NA, as.is = TRUE,
      #                           skip = 10, check.names = FALSE, fill = T,
      #                           strip.white = FALSE, blank.lines.skip = TRUE,
      #                           comment.char = "#",
      #                           allowEscapes = FALSE, flush = FALSE,
      #                           stringsAsFactors = default.stringsAsFactors())
      # #
      #   output_names = obs_node_out[1, ]
      #   obs_node_out = obs_node_out[-c(1, nrow(obs_node_out)), ]


      warn.in <- options("warn") #to reset at the end
      options(warn = -1)
      obs_node_out = data.table::fread(
            file = file.path(project.path, out.file),
            fill = TRUE,
            blank.lines.skip = FALSE)

      node_ind = grep("Node", data.frame(obs_node_out)[, 1])
      #get depth values from Node() id row
      node_dep <- obs_node_out[node_ind]
      node_dep <- gsub("Node\\(","",node_dep)
      node_dep <- gsub("\\)","",node_dep)
      node_dep <- node_dep[which(node_dep != "")]
      node_dep <- as.numeric(node_dep)

      #get names cleaned up
      output_names1 = unlist(unclass(obs_node_out[node_ind + 2]))
      output_names1 = output_names1[!is.na(output_names1)]
      output_names1 = output_names1[output_names1 != ""]
      output_names = unique(output_names1)
      output_names = output_names[2:length(output_names)] #this drops time column

      obs_node_out = obs_node_out[-c(1:(node_ind + 2), nrow(obs_node_out)),]
      obs_node_out = obs_node_out[, colnames(obs_node_out) := lapply(.SD, as.numeric), .SDcols = colnames(obs_node_out)]

      all_na_ind =   sapply(X = obs_node_out,
                            function(x) ! all(is.na(x)))
      obs_node_out = obs_node_out[, (all_na_ind), with = F]

      # #if left NULL then take all.
      # if(is.null(obs.nodes)){
      #   #calculate node number by col names.
      #   # - (# all columns minus Time column) divided by the (number of columns that repeat for every node (h,theta,Flux)) gives the number of nodes
      #   obs.nodes <- (length(output_names1)-1)/length(output_names)
      #   obs.nodes <- 1:obs.nodes #sequence from 1 to n nodes
      # }

      output_names_rep = rep(output_names, times = length(node_dep))
      obs_nodes_rep = rep(node_dep, each = length(output_names))
      output_names_all = paste(output_names_rep, obs_nodes_rep, sep = "_")
      colnames(obs_node_out) = c("Time", output_names_all)
      obs_node_out = data.frame(obs_node_out, row.names = NULL, check.names = F)

      if (is.null(obs.output) | missing(obs.output)) {
            obs_node_out = obs_node_out
      } else {
            output_cols = grepl(obs.output, names(obs_node_out))
            # output_ind = grepl(pattern = paste(c("Time", obs.output), collapse = "|"), x = names(obs_node_out))
            in_cols = c("Time", names(obs_node_out)[output_cols])
            obs_node_out =  obs_node_out[, in_cols]
      }

      ##TDB, commented this out. not sure why a t=0 step had to be added. also, it wasn't working for my irregular timesteps. i want ot keep all the
      ## times, all the rows in the dataset.
      # t1 = obs_node_out[1,]
      # t1$Time = 0
      # tstep = diff(obs_node_out$Time)
      # tstep = tstep[length(tstep)]
      # remainder = obs_node_out$Time %% tstep
      # rem_ind = which(remainder == 0)
      #
      # obs_node_out = rbind(t1, obs_node_out[rem_ind,])

      #make the dataframe into long format
      df.obs <- as.data.frame(obs_node_out)
      df.obs <- df.obs %>%
        tidyr::pivot_longer(cols = -Time,
                            names_sep = "_",
                            names_to = c('var','Depth')) %>%
        tidyr::pivot_wider(id_cols = c(Time,Depth),
                           names_from = var,
                           values_from = value)
      #during pivoting this became a character. need num
      df.obs$Depth <- as.numeric(df.obs$Depth)

      options(warn = as.numeric(warn.in))

      return(df.obs)

}
