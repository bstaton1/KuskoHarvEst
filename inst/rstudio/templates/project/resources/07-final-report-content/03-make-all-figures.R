# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
# SCRIPT TO CREATE OUTPUT FOR BUILDING FINAL REPORT FIGURES #
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::: #

# clear the workspace
rm(list = ls(all = TRUE))

# load the info about odd openers
source("00-specify-odd-openers.R")

# input and output directories
in_dir = "compiled-output"
out_dir = "figures-for-report"
if (!dir.exists(out_dir)) dir.create(out_dir)

# resolution of output figures
ppi = 600

##### DRIFT EFFORT #####

# load output
ests = readRDS(file.path(in_dir, "all_drift_effort_estimates.rds"))

# reshape the effort estimates
ests = reshape2::dcast(ests, date ~ stratum, value.var = "estimate")

# reformat the date variable
ests$date = KuskoHarvEst:::basic_date(ests$date)
dates = ests$date
dates = substr(dates, 1, nchar(dates) - 5)

# remove unneeded columns
ests = ests[,-which(colnames(ests) == "date")]

# calculate the proportion of all effort found in each stratum
p = apply(ests[,c("A", "B", "C", "D1")], 1, function(x) x/sum(x))

# which bar to label with stratum labels
label_bar_A = 1
label_bar_B = 1
label_bar_C = 1
label_bar_D = 1

# open a graphics device
png(file.path(out_dir, "driftnet-effort-by-opener.png"), h = 5 * ppi, w = 7 * ppi, res = ppi)

# set graphics parameters
par(mfrow = c(1,2), mar = c(2,2,1,1), oma = c(0,2,0,2), xaxs = "i", yaxs = "i", tcl = -0.15, mgp = c(2,0.35,0))

# make barplot: total effort estimate by date
mp = barplot(ests$total, ylim = c(0, max(ests$total, na.rm = TRUE)), las = 1, col = "grey75", space = 0.1)
usr = par("usr")
segments(usr[1], usr[3], usr[2], usr[3], xpd = TRUE)
axis(side = 1, at = mp, labels = dates, las = 2)

# make stacked barplot: proportion of effort in each stratum by date
par(mar = c(2,1,1,2))
mp = barplot(p, beside = F, ylim = c(0,1), col = c("grey45", "grey65", "grey85", "grey95"), space = 0.1, yaxt = "n", xaxt = "n")
usr = par("usr")
axis(side = 4, at = seq(0,1,0.2), labels = seq(0,1,0.2), las = 2)
segments(usr[1], usr[3], usr[2], usr[3], xpd = TRUE)
axis(side = 1, at = mp, labels = dates, las = 2)

# label the bar with stratum
text(x = mp[label_bar_A], y = p[1,label_bar_A]/2, labels = "A", font = 2)
text(x = mp[label_bar_B], y = (sum(p[1:2,label_bar_B]) + p[1,label_bar_B])/2, labels = "B", font = 2)
text(x = mp[label_bar_C], y = (sum(p[1:3,label_bar_C]) + sum(p[1:2,label_bar_C]))/2, labels = "C", font = 2)
text(x = mp[label_bar_D], y = (sum(p[1:4,label_bar_D]) + sum(p[1:3,label_bar_D]))/2, labels = "D", font = 2)

# add yaxis labels
mtext(side = 2, outer = T, line = 0.5, "Total Estimated Drift Boat Trips")
mtext(side = 4, outer = T, line = 0.5, "Proportion of Drift Boat Trips Per Stratum")

# close the graphics device
dev.off()

##### SET EFFORT #####

# load output
ests = readRDS(file.path(in_dir, "all_set_effort_estimates.rds"))

# reshape the effort estimates
ests = reshape2::dcast(ests, date ~ stratum, value.var = "estimate")

# reformat the date variable
ests$date = KuskoHarvEst:::basic_date(ests$date)
dates = ests$date
dates = substr(dates, 1, nchar(dates) - 5)

# remove unneeded columns
ests = ests[,-which(colnames(ests) == "date")]

# calculate the proportion of all effort found in each stratum
p = apply(ests[,c("A", "B", "C", "D1")], 1, function(x) x/sum(x))

# which bar to label with stratum labels
label_bar_A = 1
label_bar_B = 1
label_bar_C = 1
label_bar_D = 1

# open a graphics device
png(file.path(out_dir, "setnet-effort-by-opener.png"), h = 5 * ppi, w = 7 * ppi, res = ppi)

# set graphics parameters
par(mfrow = c(1,2), mar = c(2,2,1,1), oma = c(0,2,0,2), xaxs = "i", yaxs = "i", tcl = -0.15, mgp = c(2,0.35,0))

# make barplot: total effort estimate by date
mp = barplot(ests$total, ylim = c(0, max(ests$total, na.rm = TRUE)), las = 1, col = "grey75", space = 0.1)
usr = par("usr")
segments(usr[1], usr[3], usr[2], usr[3], xpd = TRUE)
axis(side = 1, at = mp, labels = dates, las = 2)

# bake stacked barplot: proportion of effort in each stratum by date
par(mar = c(2,1,1,2))
mp = barplot(p, beside = F, ylim = c(0,1), col = c("grey45", "grey65", "grey85", "grey95"), space = 0.1, yaxt = "n", xaxt = "n")
usr = par("usr")
axis(side = 4, at = seq(0,1,0.2), labels = seq(0,1,0.2), las = 2)
segments(usr[1], usr[3], usr[2], usr[3], xpd = TRUE)
axis(side = 1, at = mp, labels = dates, las = 2)

# label the bar with stratum
text(x = mp[label_bar_A], y = p[1,label_bar_A]/2, labels = "A", font = 2)
text(x = mp[label_bar_B], y = (sum(p[1:2,label_bar_B]) + p[1,label_bar_B])/2, labels = "B", font = 2)
text(x = mp[label_bar_C], y = (sum(p[1:3,label_bar_C]) + sum(p[1:2,label_bar_C]))/2, labels = "C", font = 2)
text(x = mp[label_bar_D], y = (sum(p[1:4,label_bar_D]) + sum(p[1:3,label_bar_D]))/2, labels = "D", font = 2)

# add yaxis labels
mtext(side = 2, outer = T, line = 0.5, "Total Estimated Set Net Trips")
mtext(side = 4, outer = T, line = 0.5, "Proportion of Set Net Trips Per Stratum")

# close the graphics device
dev.off()

