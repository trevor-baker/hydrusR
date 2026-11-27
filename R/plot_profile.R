#' Plot pressure head profiles
#'
#' Reads Nod_Inf.out from a Hydrus-1D project and plots pressure head against depth for every 20th output time.
#' Not a true S3 method; the name only resembles one.
#' @param project.path Directory of the Hydrus-1D project.
#' @param output Name of the Nod_Inf.out column to plot. Default "Head".
#' @param ... Currently unused.
#' @return Called for its side effect of drawing a plot. Returns NULL invisibly.
#' @author Subodh Acharya <https://github.com/shoebodh>
#' @rawNamespace export(plot.pressure.profile)
#' @exportS3Method NULL
plot.pressure.profile = function(project.path, output = "Head", ...){

      h1d_output = read.nod_inf(project.path, out.file = "Nod_Inf.out", output = output)
      Depth = unique(h1d_output$Depth)

 output_split = split(x = h1d_output, f = h1d_output$Time)

 total_times = length(output_split)

 tindex = seq(0, by = 20, total_times)

 output_in = output_split[tindex]

 output_in = do.call("cbind", output_in)

 output_names = names(output_in)

 output_names_p = grep(paste(output, collapse = "|"), output_names, value = T)

output_p = as.matrix(output_in[, ..output_names_p])

plot_range = range(max(output_p), -(max(output_p)))

matplot(x = output_p, y = Depth, xlim = plot_range, type = "l", xlab = "Hean (L)")

abline(v = 0, lwd = 2, col = "grey40")

}

##
#' Plot moisture profiles
#'
#' Reads Nod_Inf.out from a Hydrus-1D project and plots volumetric water content against depth for every 20th output time.
#' Not a true S3 method; the name only resembles one.
#' @param project.path Directory of the Hydrus-1D project.
#' @param output Name of the Nod_Inf.out column to plot. Default "Moisture".
#' @param ... Currently unused.
#' @return Called for its side effect of drawing a plot. Returns NULL invisibly.
#' @author Subodh Acharya <https://github.com/shoebodh>
#' @rawNamespace export(plot.moisture.profile)
#' @exportS3Method NULL
plot.moisture.profile = function(project.path, output = "Moisture", ...){

      h1d_output = read.nod_inf(project.path, out.file = "Nod_Inf.out", output = output)
      Depth = unique(h1d_output$Depth)

      output_split = split(x = h1d_output, f = h1d_output$Time)

      total_times = length(output_split)

      tindex = seq(0, by = 20, total_times)

      output_in = output_split[tindex]

      output_in = do.call("cbind", output_in)

      output_names = names(output_in)

      output_names_p = grep(paste(output, collapse = "|"), output_names, value = T)

      output_p = as.matrix(output_in[, ..output_names_p])

      plot_range = range(0, (max(output_p)))

      matplot(x = output_p, y = Depth, xlim = plot_range, type = "l",
      xlab = parse(text = "Theta~(L^3~L^-3)"))
}
