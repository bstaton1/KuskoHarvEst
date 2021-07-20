#' Create a plot summarizing effort estimation
#'
#' @inheritParams estimate_effort
#' @inheritParams estimate_harvest
#' @param trips_only Logical; should figure only show trip/flight times, or should
#'   effort estimation summaries be displayed at the bottom of the figure?
#'
#' @export

make_effort_plot = function(flight_data, effort_info, trips_only = FALSE) {

  # count how many flights were done
  n_flights = nrow(flight_data)

  # name this variable, since it will be used a lot
  trips = effort_info$trips

  # sort the trips by start time, then by end time
  trips = trips[order(trips$trip_start, trips$trip_end),]

  ### PLOT 1: THE TRIPS ###
  # set up the plotting region
  if (!trips_only) layout(matrix(c(1,2), nrow = 2), heights = c(1, 0.3))
  par(mar = c(2.5,2.5,0.25,1), tcl = -0.15, mgp = c(1.5,0.25,0),
      cex.axis = 0.6, cex.lab = 0.8, lend = "square", xaxs = "i")

  # empty plot with correct labels and dimensions
  plot(trips$trip_start, type = "n", las = 2, xaxt = "n",
       ylab = "Time", xlab = "% of Interviews Starting at Same Time or Earlier",
       ylim = range(trips$trip_start, trips$trip_end), xlim = c(0, nrow(trips) + 1))
  usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])

  # draw the x-axis
  axis(side = 1, at = round(seq(0, 1, 0.1) * nrow(trips)), labels = paste0(seq(0, 100, 10), "%"), las = 2)

  # draw the times each flight was conducted and label them
  rect(usr[1], flight_data$start_time, usr[2], flight_data$end_time, col = "grey90", border = "grey")
  text(x = usr[2] + xdiff * 0.015, y = with(flight_data, start_time + (end_time - start_time)/2),
       labels = latex2exp::TeX(paste0("$F_{", 1:n_flights, "}")), xpd = T, cex = 0.8)

  # sum up the number of times each trip was counted
  if (n_flights > 1) {
    times_counted = rowSums(trips[,stringr::str_detect(colnames(trips), "^f[:digit:]$")])
  } else {
    times_counted = as.numeric(trips[,"f1"])
  }

  # if no flight was made, flight data will have all NAs
  # change not_counted to all TRUEs and times_counted to all zeros
  if (all(is.na(flight_data[,4:ncol(flight_data)]))) {
    trips$not_counted = rep(TRUE, nrow(trips))
    times_counted = rep(0, length(times_counted))
  }

  # draw on each interviewed trips' start and end times
  segments(1:nrow(trips), trips$trip_start, 1:nrow(trips), trips$trip_end,
           col = ifelse(trips$not_counted, "blue", "black"),
           lty = ifelse(times_counted == 0, 1, times_counted), lwd = 0.8
  )

  # draw on the legend that separates line types
  legend("bottomright", horiz = T, seg.len = 1.2, legend = c(0:3), lty = c(1,1,2,3), col = c("blue", "black", "black", "black"),
         bty = "n", cex = 0.6, title = c("Times Counted"))
  box()

  ### PLOT 2: ESTIMATION INFORMATION ###
  if (!trips_only) {
    # a blank plot
    par(mar = c(0.25, 0.25, 0.25, 0.25))
    plot(1,1, type = "n", axes = F)
    box()

    # create nice TeX #1: flight counts
    flight_counts = rowSums(flight_data[,stringr::str_detect(colnames(flight_data), paste0("_", effort_info$gear))])
    flight_TeX = paste0("$F_{", 1:n_flights, "}:\\,", flight_counts)

    # create nice TeX #2: marginal probabiltites
    total_interviews = nrow(trips)
    trip_counts = effort_info$trip_counts
    marginal_counts = trip_counts[stringr::str_detect(names(trip_counts), "^f[:digit:]+?$")]
    at_all_counts = trip_counts["yes_counted"]
    marginal_p = marginal_counts/total_interviews
    at_all_p = at_all_counts/total_interviews
    if (n_flights > 1) {
      marginal_subscrpts = c(1:n_flights, "Any")
      marginal_p = c(marginal_p, at_all_p)
    } else {
      marginal_subscrpts = 1:n_flights
    }
    marginal_TeX = paste0("$Pr(F_{", marginal_subscrpts, "}):\\,", percentize(marginal_p))

    # create nice TeX #3: conditional probabilities
    if (n_flights > 1) {
      conditional_p = effort_info$p_T1_given_T2
      conditional_labels = sapply(stringr::str_extract_all(names(conditional_p), "[:digit:]"), function(x) {
        paste0("$Pr(F_{", x[1], "}", "\\,|\\,F_{", x[2], "}):\\,")
      })
      conditional_TeX = paste0(conditional_labels, percentize(conditional_p), "$")
    }

    # draw on the information using the legend() function
    usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
    legend(x = usr[1], y = usr[4], title = "Interviews Used", legend = total_interviews, bty = "n", cex = 0.7)
    legend(x = usr[1], y = usr[4] - ydiff * 0.4, title = "Estimated Effort/Interview", legend = round(effort_info$effort_per_interview, 2), bty = "n", cex = 0.7)
    legend(x = usr[1] + xdiff * 0.2, y = usr[4], title = "Marginal Probs.", legend = latex2exp::TeX(marginal_TeX), bty = "n", cex = 0.7)
    if (n_flights > 1) legend(x = usr[1] + xdiff * 0.4, y = usr[4], title = "Conditional Probs.", legend = latex2exp::TeX(conditional_TeX), bty = "n", cex = 0.7)
    legend(x = usr[1] + xdiff * 0.6, y = usr[4], title = "Flight Counts", legend = latex2exp::TeX(flight_TeX), bty = "n", cex = 0.7)
    legend(x = usr[1] + xdiff * 0.8, y = usr[4] - ydiff * 0.25, title = "Estimated Effort", legend = effort_info$effort_est_total, bty = "n", cex = 0.9)
  }
}
