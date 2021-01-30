
# the optimal dimensions of these figures is 4 inches by 6 inches.

##### FUNCTION TO DO EFFORT SENSITIVITY WITH THREE FLIGHTS #####
effort_sensitivity_3 = function(trip.dat, flight.dat, main = NULL, ylim = NULL) {
  
  ### DATA PREP ###
  
  # remove records with missing trip times
  trip.dat = trip.dat[!is.na(trip.dat$trip.start) & !is.na(trip.dat$trip.end),]
  
  # calculate trip duration
  trip.dat$duration = trip.dat$trip.end - trip.dat$trip.start
  
  # order it and add an interview order variable
  trip.dat = trip.dat[order(trip.dat$trip.start, trip.dat$duration),]
  trip.dat$int = 1:nrow(trip.dat)
  
  # determine if each trip would have been available to have been counted on each flight
  trip.dat$f1 = trips.available(trip.start = trip.dat$trip.start, trip.end = trip.dat$trip.end, f.times = flight.dat$f1.times)
  trip.dat$f2 = trips.available(trip.start = trip.dat$trip.start, trip.end = trip.dat$trip.end, f.times = flight.dat$f2.times)
  trip.dat$f3 = trips.available(trip.start = trip.dat$trip.start, trip.end = trip.dat$trip.end, f.times = flight.dat$f3.times)
  trip.dat$counted = ifelse(trip.dat$f1 == 1 | trip.dat$f2 == 1 | trip.dat$f3 == 1, 1, 0)
  trip.dat$double = ifelse(rowSums(trip.dat[,c("f1", "f2", "f3")]) > 1, 1, 0)
  
  trip.ests = estimate.trips(trips = trip.dat, f1.times = flight.dat$f1.times, num.f1 = flight.dat$drift.tot[1],
                             f2.times = flight.dat$f2.times, num.f2 = flight.dat$drift.tot[2],
                             f3.times = flight.dat$f3.times, num.f3 = flight.dat$drift.tot[3])
  
  ### PLOT ###
  if (is.null(ylim)) {
    ylim = c(min(floor(trip.dat$trip.start)), max(ceiling(trip.dat$trip.end)))
  }

  par(mar = c(0.5,4.5,2,0.5))
  plot(trip.start ~ int, data = trip.dat,
       xlim = range(trip.dat$int), ylim = ylim,
       type = "n", xaxt = "n", yaxt = "n", 
       main = main, ylab = "Time", cex.main = 1.4, cex.lab = 1.4)
  usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
  rect(ybottom = flight.dat$f1.times[1], xleft = usr[1], ytop = flight.dat$f1.times[2], xright = usr[2], col = "grey90", border = NA)
  rect(ybottom = flight.dat$f2.times[1], xleft = usr[1], ytop = flight.dat$f2.times[2], xright = usr[2], col = "grey90", border = NA)
  rect(ybottom = flight.dat$f3.times[1], xleft = usr[1], ytop = flight.dat$f3.times[2], xright = usr[2], col = "grey90", border = NA)
  abline(h = flight.dat$f1.times, col = "grey")
  abline(h = flight.dat$f2.times, col = "grey")
  abline(h = flight.dat$f3.times, col = "grey")
  
  with(trip.dat, segments(int, trip.start, int, trip.end, lwd = 1, col = ifelse(counted == 1, "blue", "red"), lty = ifelse(double == 1, 2, 1)))
  
  text(x = usr[2], y = usr[3] + ydiff * 0.295, labels = paste("F1 Boat Count =", round(flight.dat$drift.tot[1])), pos = 2, font = 1)
  text(x = usr[2], y = usr[3] + ydiff * 0.265, labels = paste("F2 Boat Count =", round(flight.dat$drift.tot[2])), pos = 2, font = 1)
  text(x = usr[2], y = usr[3] + ydiff * 0.235, labels = paste("F3 Boat Count =", round(flight.dat$drift.tot[3])), pos = 2, font = 1)
  text(x = usr[2], y = usr[3] + ydiff * 0.205, labels = paste("P(F1 & F2) = ", round(trip.ests["p.old1"], 2)), pos = 2, font = 1)
  text(x = usr[2], y = usr[3] + ydiff * 0.175, labels = paste("P(F2 & F3) = ", round(trip.ests["p.old2"], 2)), pos = 2, font = 1)
  text(x = usr[2], y = usr[3] + ydiff * 0.145, labels = paste("Counted Interviews =", round(trip.ests["n.counted"])), pos = 2, font = 1)
  text(x = usr[2], y = usr[3] + ydiff * 0.115, labels = paste("Uncounted Interviews =", round(trip.ests["n.not.counted"])), pos = 2, font = 1)
  text(x = usr[2], y = usr[3] + ydiff * 0.085, labels = paste("Counted Trips =", round(trip.ests["total.a"])), pos = 2, font = 1)
  text(x = usr[2], y = usr[3] + ydiff * 0.055, labels = paste("Uncounted Trips =", round(trip.ests["trips.not.counted"])), pos = 2, font = 1)
  text(x = usr[2], y = usr[3] + ydiff * 0.025, labels = paste("Total Trips =", round(trip.ests["total.b"])), pos = 2, font = 1)
  
  text(x = (usr[1] + xdiff * 0.01)/2, y = sum(flight.dat$f1.times)/2, "F1", font = 2)
  text(x = (usr[1] + xdiff * 0.01)/2, y = sum(flight.dat$f2.times)/2, "F2", font = 2)
  text(x = (usr[1] + xdiff * 0.01)/2, y = sum(flight.dat$f3.times)/2, "F3", font = 2)
  
  axis(side = 2, at = seq(0, 24, 2), labels = seq(0, 24, 2), las = 2, lwd = 4, cex.axis = 1.4)
  
  
  box(lwd = 4)
  
  trip.ests
}

##### FUNCTION TO DO EFFORT SENSITIVITY WITH TWO FLIGHTS #####
effort_sensitivity_2 = function(trip.dat, flight.dat, main = NULL, ylim = NULL) {
  
  ### DATA PREP ###
  
  # remove records with missing trip times
  trip.dat = trip.dat[!is.na(trip.dat$trip.start) & !is.na(trip.dat$trip.end),]
  
  # calculate trip duration
  trip.dat$duration = trip.dat$trip.end - trip.dat$trip.start
  
  # order it and add an interview order variable
  trip.dat = trip.dat[order(trip.dat$trip.start, trip.dat$duration),]
  trip.dat$int = 1:nrow(trip.dat)
  
  # determine if each trip would have been available to have been counted on each flight
  trip.dat$f1 = trips.available(trip.start = trip.dat$trip.start, trip.end = trip.dat$trip.end, f.times = flight.dat$f1.times)
  trip.dat$f2 = trips.available(trip.start = trip.dat$trip.start, trip.end = trip.dat$trip.end, f.times = flight.dat$f2.times)
  trip.dat$counted = ifelse(trip.dat$f1 == 1 | trip.dat$f2 == 1, 1, 0)
  trip.dat$double = ifelse(rowSums(trip.dat[,c("f1", "f2")]) > 1, 1, 0)
  
  trip.ests = estimate.trips(trips = trip.dat, f1.times = flight.dat$f1.times, num.f1 = flight.dat$drift.tot[1],
                             f2.times = flight.dat$f2.times, num.f2 = flight.dat$drift.tot[2]
                             )
  
  ### PLOT ###
  if (is.null(ylim)) {
    ylim = c(min(floor(trip.dat$trip.start)), max(ceiling(trip.dat$trip.end)))
  }

  par(mar = c(0.5,4.5,2,0.5))
  plot(trip.start ~ int, data = trip.dat,
       xlim = range(trip.dat$int), ylim = ylim,
       type = "n", xaxt = "n", yaxt = "n", 
       main = main, ylab = "Time", cex.main = 1.4, cex.lab = 1.4)
  usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
  rect(ybottom = flight.dat$f1.times[1], xleft = usr[1], ytop = flight.dat$f1.times[2], xright = usr[2], col = "grey90", border = NA)
  rect(ybottom = flight.dat$f2.times[1], xleft = usr[1], ytop = flight.dat$f2.times[2], xright = usr[2], col = "grey90", border = NA)
  abline(h = flight.dat$f1.times, col = "grey")
  abline(h = flight.dat$f2.times, col = "grey")
  
  with(trip.dat, segments(int, trip.start, int, trip.end, lwd = 1, col = ifelse(counted == 1, "blue", "red"), lty = ifelse(double == 1, 2, 1)))
  
  text(x = usr[2], y = usr[3] + ydiff * 0.235, labels = paste("F1 Boat Count =", round(flight.dat$drift.tot[1])), pos = 2, font = 1)
  text(x = usr[2], y = usr[3] + ydiff * 0.205, labels = paste("F2 Boat Count =", round(flight.dat$drift.tot[2])), pos = 2, font = 1)
  text(x = usr[2], y = usr[3] + ydiff * 0.175, labels = paste("P(F1 & F2) = ", round(trip.ests["p.old1"], 2)), pos = 2, font = 1)
  text(x = usr[2], y = usr[3] + ydiff * 0.145, labels = paste("Counted Interviews =", round(trip.ests["n.counted"])), pos = 2, font = 1)
  text(x = usr[2], y = usr[3] + ydiff * 0.115, labels = paste("Uncounted Interviews =", round(trip.ests["n.not.counted"])), pos = 2, font = 1)
  text(x = usr[2], y = usr[3] + ydiff * 0.085, labels = paste("Counted Trips =", round(trip.ests["total.a"])), pos = 2, font = 1)
  text(x = usr[2], y = usr[3] + ydiff * 0.055, labels = paste("Uncounted Trips =", round(trip.ests["trips.not.counted"])), pos = 2, font = 1)
  text(x = usr[2], y = usr[3] + ydiff * 0.025, labels = paste("Total Trips =", round(trip.ests["total.b"])), pos = 2, font = 1)
  
  text(x = (usr[1] + xdiff * 0.01)/2, y = sum(flight.dat$f1.times)/2, "F1", font = 2)
  text(x = (usr[1] + xdiff * 0.01)/2, y = sum(flight.dat$f2.times)/2, "F2", font = 2)
  
  axis(side = 2, at = seq(0, 24, 2), labels = seq(0, 24, 2), las = 2, lwd = 4, cex.axis = 1.4)
  
  box(lwd = 4)
  
  trip.ests
}

##### FUNCTION TO DO EFFORT SENSITIVITY WITH ONE FLIGHT #####
effort_sensitivity_1 = function(trip.dat, flight.dat, main = NULL, ylim = NULL) {
  
  ### DATA PREP ###
  
  # remove records with missing trip times
  trip.dat = trip.dat[!is.na(trip.dat$trip.start) & !is.na(trip.dat$trip.end),]
  
  # calculate trip duration
  trip.dat$duration = trip.dat$trip.end - trip.dat$trip.start
  
  # order it and add an interview order variable
  trip.dat = trip.dat[order(trip.dat$trip.start, trip.dat$duration),]
  trip.dat$int = 1:nrow(trip.dat)
  
  # determine if each trip would have been available to have been counted on each flight
  trip.dat$f1 = trips.available(trip.start = trip.dat$trip.start, trip.end = trip.dat$trip.end, f.times = flight.dat$f1.times)

  trip.ests = estimate.trips(trips = trip.dat, f1.times = flight.dat$f1.times, num.f1 = flight.dat$drift.tot[1])
  
  ### PLOT ###
  if (is.null(ylim)) {
    ylim = c(min(floor(trip.dat$trip.start)), max(ceiling(trip.dat$trip.end)))
  }
  
  par(mar = c(0.5,4.5,2,0.5))
  plot(trip.start ~ int, data = trip.dat,
       xlim = range(trip.dat$int), ylim = ylim,
       type = "n", xaxt = "n", yaxt = "n", 
       main = main, ylab = "Time", cex.main = 1.4, cex.lab = 1.4)
  usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
  rect(ybottom = flight.dat$f1.times[1], xleft = usr[1], ytop = flight.dat$f1.times[2], xright = usr[2], col = "grey90", border = NA)
  abline(h = flight.dat$f1.times, col = "grey")

  with(trip.dat, segments(int, trip.start, int, trip.end, lwd = 1, col = ifelse(f1 == 1, "blue", "red")))
  
  text(x = usr[2], y = usr[3] + ydiff * 0.175, labels = paste("F1 Boat Count =", round(flight.dat$drift.tot[1])), pos = 2, font = 1)
  text(x = usr[2], y = usr[3] + ydiff * 0.145, labels = paste("Counted Interviews =", round(trip.ests["n.counted"])), pos = 2, font = 1)
  text(x = usr[2], y = usr[3] + ydiff * 0.115, labels = paste("Uncounted Interviews =", round(trip.ests["n.not.counted"])), pos = 2, font = 1)
  text(x = usr[2], y = usr[3] + ydiff * 0.085, labels = paste("Counted Trips =", round(trip.ests["total.a"])), pos = 2, font = 1)
  text(x = usr[2], y = usr[3] + ydiff * 0.055, labels = paste("Uncounted Trips =", round(trip.ests["trips.not.counted"])), pos = 2, font = 1)
  text(x = usr[2], y = usr[3] + ydiff * 0.025, labels = paste("Total Trips =", round(trip.ests["total.b"])), pos = 2, font = 1)
  
  text(x = (usr[1] + xdiff * 0.01)/2, y = sum(flight.dat$f1.times)/2, "F1", font = 2)

  axis(side = 2, at = seq(0, 24, 2), labels = seq(0, 24, 2), las = 2, lwd = 4, cex.axis = 1.4)
  
  box(lwd = 4)
  
  trip.ests
}
