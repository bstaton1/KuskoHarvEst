#' Make a single histogram showing distribution of a quantity across interviews
#'

make_histogram = function(interview_data, gear, variable, n_bins = 10) {

  # set the variables that are accepted, and perform error check
  accepted_variables = c("total_salmon", "chinook", "chum", "sockeye", "chum+sockeye", "soak_duration", "trip_duration", "p_chinook")
  if (!(variable %in% accepted_variables)) {
    stop ("supplied value for variable argument ('", variable, "') not accepted.\nAccepted values are:\n", paste0("  '", accepted_variables, "'\n"))
  }

  # keep only values for the specified gear and only completed trips
  x_data = interview_data[is_complete_trip(interview_data) & interview_data$gear == gear,]

  # drop any data collected by ADFG -- this hasn't been historically reported here
  x_data = x_data[x_data$source != "ADFG",]

  # prepare the information: total_salmon
  if (variable == "total_salmon") {
    x = rowSums(x_data[,c("chinook", "chum", "sockeye")])
    main = "Total Salmon Catch/Trip"
  }

  # prepare the information: chinook
  if (variable == "chinook") {
    x = x_data[,"chinook"]
    main = "Chinook Salmon Catch/Trip"
  }

  # prepare the information: chum
  if (variable == "chum") {
    x = x_data[,"chum"]
    main = "Chum Salmon Catch/Trip"
  }

  # prepare the information: sockeye
  if (variable == "sockeye") {
    x = x_data[,"sockeye"]
    main = "Sockeye Salmon Catch/Trip"
  }

  # prepare the information: chum + sockeye
  if (variable == "chum+sockeye") {
    x = rowSums(x_data[,c("chum", "sockeye")])
    main = "Chum+Sockeye Salmon Catch/Trip"
  }

  # prepare the information: soak duration
  if (variable == "soak_duration") {
    x = as.numeric(lubridate::as.duration(x_data$soak_duration), "hours")
    main = "Soak Duration (Hours)"
  }

  # prepare the information: trip duration
  if (variable == "trip_duration") {
    x = as.numeric(lubridate::as.duration(x_data$trip_duration), "hours")
    main = "Trip Duration (Hours)"
  }

  # prepare the information: % chinook
  if (variable == "p_chinook") {
    x = x_data$chinook/rowSums(x_data[,c("chinook", "chum", "sockeye")])
    x = x * 100
    main = "% Chinook Salmon"
  }

  # determine the break points
  breaks = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length = n_bins)

  # calculate the mean by data source
  means = tapply(x, x_data$source, mean, na.rm = TRUE)
  means = round(c(All = mean(x, na.rm = TRUE), means), 1)
  means_text = paste0(names(means), " Mean = ", unname(means))

  # make the plot
  par(mgp = c(2,0.35,0), tcl = -0.15, xaxs = "i", yaxs = "i")
  bin_counts = hist(x, breaks = breaks, plot = FALSE)$counts
  hist(x, breaks = breaks, ylim = c(0, max(bin_counts)) * 1.4, main = main, xlab = "Value", ylab = "Frequency",
       col = "grey70", border = "white", xaxt = "n", yaxt = "n")
  axis(side = 1); axis(side = 2, las = 2)
  legend("topleft", x.intersp = -0.5, ncol = 2, legend = means_text, bty = "n", cex = 1.15)
  usr = par("usr")
  segments(usr[1], usr[3], usr[2], usr[3], xpd = T)
}

#' Make a multi-panel figure showing multiple histograms
#'
#' @export

make_histograms = function(interview_data, gear, variables, n_bins = 10) {

  # error check to make sure not more than 6 variables were supplied
  if (length(variables) > 6) {
    stop ("A maximum of 6 variables can be plotted.")
  }

  # graphics device settings
  par(mfrow = c(2,3), mar = c(1.5,1,2,1), oma = c(0,2,0,0))

  # loop through variables creating the histogram for each
  junk = sapply(variables, function(v) make_histogram(interview_data, gear, v, n_bins))

  # add a shared y-axis label
  mtext(side = 2, outer = T, "Frequency", line = 0.5)
}

#' Make a figure caption for multi-panel histograms
#'

make_histogram_caption = function(interview_data, gear) {
  # start the caption
  hist_fig_cap = paste0("Distributions of relevant quantities from all completed trips using ", gear, " nets")

  # include a bit about dropping LE interviews, but only if they are present
  if (any(interview_data$source == "LE")) {
    hist_fig_cap = paste0(hist_fig_cap, " (law enforcement interviews excluded).")
  } else {
    hist_fig_cap = paste0(hist_fig_cap, ".")
  }

  # determine which sources are present, and include definitions for their abbreviations
  srcs = unique(interview_data$source)
  srcs = srcs[srcs %in% c("BBH", "CBM", "FC")]
  hist_fig_cap2 = paste0("The mean quantity by primary data source is shown in the top right; ", paste(paste0(srcs, " = ", source_names[srcs,"source_long"]), collapse = ", "), ".")

  # make the final caption
  hist_fig_cap = paste0(hist_fig_cap, " ", hist_fig_cap2)

  # return it
  return(hist_fig_cap)
}
