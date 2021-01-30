# GOAL.PLOT()

# AUTHOR: BEN STATON
# LAST UPDATED: 6/8/2017

# DESCRIPTION
# generates a 3 panel plot with the percent of interviewed fishers at or above a given goal level

# INPUTS
# goals: a data frame with three columns: chinook.goal, chum.goal, sockeye.goal
  # each column is populated with 1 = Not at All, 2 = Under Half, 3 = Half, 4 = Over Half, 5 = Done for each fisher

# OUTPUTS
# the plot
# goals = cbm.prep(dat = read.csv(paste(data_dir, "CBM_6_24_17.csv", sep = "/"), stringsAsFactors = F))$goals
goal.plot = function(goals) {
  status.names = c("Under Half", "Half", "Over Half", "Done")
  at.x = seq(1,4)
  
  # CHINOOK PLOT
  empty = rep(0, 5); names(empty) = 1:5
  counts = table(goals$chinook.goal)
  empty[names(counts)] = counts
  counts = empty
  percent = rev(cumsum(rev(counts))/sum(counts)) * 100
 
  percent = percent[2:5]
  # par(xaxs = "i", yaxs = "i", mfrow = c(3,1), mar = c(2,2,2,2), xpd = T, oma = c(0, 2, 0, 0))
  par(xaxs = "i", yaxs = "i", mfrow = c(1,3), mar = c(2,2,2,2), xpd = T, oma = c(0, 2, 0, 0))
  plot(percent ~ at.x, type = "n", pch = 15, las = 1, ylab = "",
       xaxt = "n", xlab = "", ylim = c(0, 105), xlim = range(at.x), main = "Chinook Salmon")
  axis(side = 1, at = at.x, labels = status.names)
  usr = par("usr")
  polygon(x = c(rev(at.x), at.x), y = c(rev(rep(usr[3], length(at.x))), percent), col = "grey90", border = NA)
  segments(at.x[1], percent[1], at.x[1], usr[3], col = "grey")
  segments(at.x[4], percent[4], at.x[4], usr[3], col = "grey")
  lines(percent ~ at.x, col = "black", lwd = 1, type = "o", pch = 16, cex = 1.5)
  box()
  
  # CHUM PLOT
  empty = rep(0, 5); names(empty) = 1:5
  counts = table(goals$chum.goal)
  empty[names(counts)] = counts
  counts = empty
  percent = rev(cumsum(rev(counts))/sum(counts)) * 100
  
  percent = percent[2:5]
  plot(percent ~ at.x, type = "n", pch = 15, las = 1, ylab = "",
       xaxt = "n", xlab = "", ylim = c(0, 105), xlim = range(at.x), main = "Chum Salmon")
  axis(side = 1, at = at.x, labels = status.names)
  usr = par("usr")
  polygon(x = c(rev(at.x), at.x), y = c(rev(rep(usr[3], length(at.x))), percent), col = "grey90", border = NA)
  segments(at.x[1], percent[1], at.x[1], usr[3], col = "grey")
  segments(at.x[4], percent[4], at.x[4], usr[3], col = "grey")
  lines(percent ~ at.x, col = "black", lwd = 1, type = "o", pch = 16, cex = 1.5)
  box()
  
  # SOCKEYE PLOT
  empty = rep(0, 5); names(empty) = 1:5
  counts = table(goals$sockeye.goal)
  empty[names(counts)] = counts
  counts = empty
  percent = rev(cumsum(rev(counts))/sum(counts)) * 100
  
  percent = percent[2:5]
  plot(percent ~ at.x, type = "n", pch = 15, las = 1, ylab = "",
       xaxt = "n", xlab = "", ylim = c(0, 105), xlim = range(at.x), main = "Sockeye Salmon")
  axis(side = 1, at = at.x, labels = status.names)
  usr = par("usr")
  polygon(x = c(rev(at.x), at.x), y = c(rev(rep(usr[3], length(at.x))), percent), col = "grey90", border = NA)
  segments(at.x[1], percent[1], at.x[1], usr[3], col = "grey")
  segments(at.x[4], percent[4], at.x[4], usr[3], col = "grey")
  lines(percent ~ at.x, col = "black", lwd = 1, type = "o", pch = 16, cex = 1.5)
  box()
  
  mtext(side = 2, outer = T, "% of Fishers at or Above Category", line = 0.5)
  
}

goal.plot.1 = function(dat, village, spp, main = NULL, xaxt = T) {
  status.names = c("Under Half", "Half", "Over Half", "Done")
  at.x = seq(1,4)
  
  if (village == "All") {
    goals = dat[,paste(tolower(spp), "goal", sep = ".")]
  } else {
    goals = dat[dat$village == village, paste(tolower(spp), "goal", sep = ".")]
  }
  
  if (length(goals) != 0) {
    empty = rep(0, 5); names(empty) = 1:5
    counts = table(goals)
    empty[names(counts)] = counts
    counts = empty
    percent = rev(cumsum(rev(counts))/sum(counts)) * 100
    
    percent = percent[2:5]
    # par(xaxs = "i", yaxs = "i", mfrow = c(3,1), mar = c(2,2,2,2), xpd = T, oma = c(0, 2, 0, 0))
    par(xaxs = "i", yaxs = "i", xpd = T)
    plot(percent ~ at.x, type = "n", pch = 15, las = 1, ylab = "",
         xaxt = "n", xlab = "", ylim = c(0, 100), xlim = range(at.x))
    if(xaxt) axis(side = 1, at = at.x, labels = status.names)
    usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
    polygon(x = c(rev(at.x), at.x), y = c(rev(rep(usr[3], length(at.x))), percent), col = "grey90", border = NA)
    segments(at.x[1], percent[1], at.x[1], usr[3], col = "grey")
    segments(at.x[4], percent[4], at.x[4], usr[3], col = "grey")
    lines(percent ~ at.x, col = "black", lwd = 1, type = "o", pch = 16, cex = 1.5)
    text(x = usr[2], y = usr[4] - ydiff * 0.1, labels = main, pos = 2, font = 2, cex = 1.2)
    box()
  } else {
    par(xaxs = "i", yaxs = "i", xpd = T)
    plot(1,1, type = "n", pch = 15, las = 1, ylab = "",
         xaxt = "n", xlab = "", ylim = c(0, 100), xlim = range(at.x))
    if (xaxt) axis(side = 1, at = at.x, labels = status.names)
    usr = par("usr"); xdiff = diff(usr[1:2]); ydiff = diff(usr[3:4])
    text(x = usr[2], y = usr[4] - ydiff * 0.1, labels = main, pos = 2, font = 2, cex = 1.2)
    text(x = sum(usr[1:2])/2, y = sum(usr[3:4])/2, labels = "NO DATA", font = 4, col = "grey")
    box()
  }
}


# goal.plot.1(dat = dat_18, village = "x", spp = "chinook")
# ppi = 600
# png("GoalPlot.png", h = 6 * ppi, w = 4 * ppi, res = ppi)
# goal.plot(goals = cbm.goals)
# dev.off()
