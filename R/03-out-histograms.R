#' Make a single histogram
#'
#' Histogram shows the distribution of a variable across interviews
#'
#' @inheritParams estimate_harvest
#' @param variable a variable to show the distribution for. Accepted options are:
#'   * `"total_salmon"`
#'   * `"chinook"`
#'   * `"chum"`
#'   * `"sockeye"`
#'   * `"chum+sockeye"`
#'   * `"trip_start"`
#'   * `"trip_end"`
#'   * `"soak_duration"`
#'   * `"trip_duration"`
#'   * `"p_chinook"`
#' @param n_bins Numeric; the number of bars to draw for the histogram
#'

make_histogram = function(interview_data, gear, variable, n_bins = 10) {

  # set the variables that are accepted, and perform error check
  spp_accept = c(unlist(paste(species_names$species)), "chum+sockeye", "total_salmon")
  duration_accept = c("trip_duration", "soak_duration")
  time_accept = c("trip_start", "trip_end")
  p_accept = c("p_chinook")
  accepted_variables = c(spp_accept, duration_accept, time_accept, p_accept)

  # determine what kind of variable it is
  is_catch = variable %in% spp_accept
  is_duration = variable %in% duration_accept
  is_time = variable %in% time_accept
  is_p = variable %in% p_accept

  # which species are found in the interview data
  spp_found = unname(unlist(species_in_data(interview_data)))

  # if more than one variable supplied, stop
  if (length(variable) > 1) {
    stop ("only one variable can be supplied at once")
  }

  # if the supplied variable isn't in the list of those accepted, stop
  if (!(variable %in% accepted_variables)) {
    stop ("supplied value for variable argument ('", variable, "') not accepted.\nAccepted values are:", knitr::combine_words(accepted_variables, before = "  \n'", after = "'", and = " or "))
  }

  # add a total_salmon variable
  if (any(species_names$species[species_names$is_salmon] %in% spp_found)) {
    interview_data$total_salmon = rowSums(as.matrix(interview_data[,species_in_data(interview_data)$salmon]))
  } else {
    interview_data$total_salmon = 0
  }

  # if both chum and sockeye data are found in data, add a chum+sockeye variable
  if (all(c("chum", "sockeye") %in% spp_found)) {
    interview_data = cbind(interview_data, "chum+sockeye" = rowSums(interview_data[,c("chum", "sockeye")]))
  }

  # if it is a catch and the species name isn't in data, stop
  if ((is_catch | is_p) & (!stringr::str_remove(variable, "_rate$|^p_") %in% c(spp_found, "total_salmon", "chum+sockeye"))) {
    stop ("species '", stringr::str_remove(variable, "_rate$|^p_"), "' is not contained in interview_data")
  }

  # subset out only relevant data for the histogram: completed trips using this gear type
  x_data = interview_data[is_complete_trip(interview_data) & interview_data$gear == gear,]

  # drop any data collected by ADFG -- this hasn't been historically reported here
  x_data = x_data[x_data$source != "ADFG",]

  # prepare information: catch/trip
  if (is_catch) {
    x = x_data[,variable]
    if (variable %in% species_names$species) {
      main = paste0(stringr::str_to_title(species_names$in_text[species_names$species == variable]), " Catch/Trip")
    } else {
      if (variable == "total_salmon") main = "Total Salmon Catch/Trip"
      if (variable == "chum+sockeye") main = "Chum+Sockeye Salmon Catch/Trip"
    }
  }

  # prepare information: trip_start or trip_end
  if (is_time) {
    x = lubridate::hour(x_data[,variable])
    main = variable |>
      stringr::str_replace("_", " ") |>
      stringr::str_to_title() |>
      paste0(" Time")
  }

  # prepare information: soak_duration or trip_duration
  if (is_duration) {
    x = as.numeric(lubridate::as.duration(x_data[,variable]), "hours")
    main = variable |>
      stringr::str_remove("_.+$") |>
      stringr::str_to_title() |>
      paste0(" Duration (Hours)")
  }

  # prepare information: p_chinook
  if (is_p) {
    p_spp = stringr::str_remove(variable, "^p_")
    salmon_spp = spp_found[spp_found %in% species_names$species[species_names$is_salmon]]
    numerator = x_data[,p_spp]
    denominator = rowSums(as.matrix(x_data[,salmon_spp]))
    x = numerator/denominator
    x = x * 100
    main = "% Chinook Salmon"
  }

  # determine the break points
  breaks = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length = n_bins)

  dec_hour_to_time = function(dec_time) {
    hr = floor(dec_time)
    min = stringr::str_pad(round((dec_time - hr) * 60), 2, "left", "0")
    paste0(hr, ":", min)
  }

  # calculate the mean by data source
  means = tapply(x, x_data$source, mean, na.rm = TRUE)
  means = c(All = mean(x, na.rm = TRUE), means)

  # format means differently if a time-of-day variable
  if (variable %in% c("trip_start", "trip_end")) {
    nms = names(means)
    means = dec_hour_to_time(means)
    names(means) = nms
  } else {
    means = round(means, 1)
  }
  means_text = paste0(names(means), " Mean: ", unname(means))

  # make the plot
  par(mgp = c(2,0.35,0), tcl = -0.15, xaxs = "i", yaxs = "i")
  bin_counts = hist(x, breaks = breaks, plot = FALSE)$counts
  hist(x, breaks = breaks, ylim = c(0, max(bin_counts)) * 1.4, main = main, xlab = "Value", ylab = "Frequency",
       col = "grey70", border = "white", xaxt = "n", yaxt = "n")

  # handle the x-axis. use special formatting if plotting a time-of-day variable
  if (variable %in% c("trip_start", "trip_end")) {
    at_x = axisTicks(par("usr")[1:2], log = FALSE, nint = 3)
    lab_x = dec_hour_to_time(at_x)
    axis(side = 1, at = at_x, labels = lab_x)
  } else {
    axis(side = 1)
  }

  axis(side = 2, las = 2)
  legend("top", x.intersp = -0.5, ncol = 2, legend = means_text, bty = "n", cex = 1.15)
  usr = par("usr")
  segments(usr[1], usr[3], usr[2], usr[3], xpd = T)
}

#' Make a multi-panel figure showing multiple histograms
#'
#' Calls [make_histogram()] several times to build a multipanel figure showing
#'   summaries of several variables.
#'
#' @inheritParams estimate_harvest
#' @param variables Character; vector indicating which
#'   variables to draw histograms for. See [make_histogram()] for accepted options
#' @param mfrow Numeric; vector of length 2 specifying how to organize the histogram panels in `c(rows,colums)`.
#'   Supplied to [graphics::par()] and defaults to `c(2,3)`.
#' @param n_bins Numeric; the number of bars to draw for the histogram
#' @note If 6 or fewer variables are supplied, the panels will be organized as
#'
#' @export

make_histograms = function(interview_data, gear, variables, mfrow = c(2,3), n_bins = 10) {

  # graphics device settings
  par(mfrow = mfrow, mar = c(1.5,1,2,1), oma = c(0,2,0,0))

  # loop through variables creating the histogram for each
  junk = sapply(variables, function(v) make_histogram(interview_data, gear, v, n_bins))

  # add a shared y-axis label
  mtext(side = 2, outer = T, "Frequency", line = 0.5)
}

#' Make a figure caption for multi-panel histograms
#'
#' Automates the generation of the histogram caption for the PDF output.
#'   The text varies depending on the gear and the data sources available and
#'   this function prevents the user from needing to edit it by hand.
#'
#' @inheritParams estimate_harvest
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
