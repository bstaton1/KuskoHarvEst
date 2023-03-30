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
ests$date = KuskoHarvUtils::basic_date(ests$date)
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
ests$date = KuskoHarvUtils::basic_date(ests$date)
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

##### INTERVIEWS #####

# load output
interviews = readRDS(file.path(in_dir, "all_interview_data.rds"))

# calculate the number of interviews by opener and data source
by_source = table(lubridate::date(interviews$trip_start), interviews$source)
total = table(lubridate::date(interviews$trip_start))
ests = cbind(by_source, total)
ests = as.data.frame(ests)

# extract/format date
dates = KuskoHarvUtils::basic_date(rownames(ests))
dates = substr(dates, 1, nchar(dates) - 5)
rownames(ests) = NULL

# calculate the proportion of all effort found in each stratum
p = apply(ests[,c("BBH", "CBM", "FC")], 1, function(x) x/sum(x))

# which bar to label with stratum labels
label_bar_BBH = 1
label_bar_CBM = 1
label_bar_FC = 1

# open a graphics device
png(file.path(out_dir, "interviews-by-opener.png"), h = 5 * ppi, w = 7 * ppi, res = ppi)

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
text(x = mp[label_bar_BBH], y = p[1,label_bar_BBH]/2, labels = "BBH", font = 2, srt = 90)
text(x = mp[label_bar_CBM], y = (sum(p[1:2,label_bar_CBM]) + p[1,label_bar_CBM])/2, labels = "CBM", font = 2, srt = 90)
text(x = mp[label_bar_FC], y = (sum(p[1:3,label_bar_FC]) + sum(p[1:2,label_bar_FC]))/2, labels = "FC", font = 2, srt = 90)

# add yaxis labels
mtext(side = 2, outer = T, line = 0.5, "Total Interviews")
mtext(side = 4, outer = T, line = 0.5, "Proportion of Interviews by Source")

# close the graphics device
dev.off()

##### ALL HISTOGRAMS #####

# load output
interviews = readRDS(file.path(in_dir, "all_interview_data.rds"))

# specify which variables to make histograms for
hist_variables = c("total_salmon", "chinook", "chum", "sockeye", "trip_start", "trip_duration", "soak_duration", "p_chinook")

# format the dates to make histograms for
dates = unique(lubridate::date(interviews$trip_start))
dates = dates[!is.na(dates)]

# a function to make one histogram file for one opener
hist_function = function(plot_date) {

  cat("\rMaking Histogram File for", as.character(plot_date))

  gear = ifelse(as.character(plot_date) %in% set_only_openers, "set", "drift")

  file_date = KuskoHarvEst:::file_date(plot_date)
  file_name = file.path(out_dir, paste0("histograms_", gear, "_", file_date, ".png"))

  png(file_name, h = 8 * ppi, w = 5 * ppi, res = ppi)
  KuskoHarvEst::make_histograms(subset(interviews, lubridate::date(trip_start) == plot_date), gear, hist_variables, mfrow = c(4,2))
  dev.off()
}

# loop through dates, making the histogram figure file for each
junk = sapply(dates, hist_function); rm(junk)

##### HARVEST BY SPECIES AND OPENER #####

# read in the output
ests = read.csv(file.path(in_dir, "all-harvest-summaries.csv"))

# subset only estimates that will be plotted
ests = subset(ests, stratum == "total" & gear == "total" & date != "total" & species != "total")

# format the dates
dates = unique(KuskoHarvUtils::basic_date(ests$date))
dates = substr(dates, 1, nchar(dates) - 5)

# extract the mean, lwr, and upr estimates by species and stratum, formatted for barplotting
means = as.matrix(reshape2::dcast(ests, species ~ date, value.var = "mean")[,-1])
lwrs = as.matrix(reshape2::dcast(ests, species ~ date, value.var = "lwr95ci")[,-1])
uprs = as.matrix(reshape2::dcast(ests, species ~ date, value.var = "upr95ci")[,-1])

# open a graphics device
png(file.path(out_dir, "harvest-by-species-and-opener.png"), h = 5 * ppi, w = 7 * ppi, res = ppi)

# set graphics parameters
par(mar = c(2,4,2,2), tcl = -0.15, mgp = c(2,0.35,0))

# make the grouped barplot
mp = barplot(means, beside = TRUE, ylim = c(0, max(uprs, na.rm = TRUE) * 1.05), names.arg = dates, yaxt = "n", col = c("grey50", "grey70", "grey90"))

# handle axes
usr = par("usr")
segments(usr[1], usr[3], usr[2], usr[3], xpd = T)
axis(side = 1, at = mp[2,], labels = FALSE)
at_y = axisTicks(usr[3:4], log = FALSE)
axis(side = 2, at = at_y, labels = prettyNum(at_y, big.mark = ",", scientific = FALSE), las = 2)
mtext(side = 2, line = 3, "Harvest")

# add error bars (suppress warnings about NA length arrows)
suppressWarnings(arrows(mp, lwrs, mp, uprs, code = 3, angle = 90, length = 0.05))

# add a legend
legend("topleft", legend = c("Chinook", "Chum", "Sockeye"), pch = 22, pt.cex = 2.5, pt.bg = c("grey50", "grey70", "grey90"), bty = "n")

# close the device
dev.off()

##### TOTAL HARVEST BY SPECIES #####
# read in the output
ests = read.csv(file.path(in_dir, "all-harvest-summaries.csv"))

# subset only estimates that will be plotted
ests = subset(ests, stratum == "total" & gear == "total" & date == "total")
ests$species = factor(ests$species, levels = c("total", "chinook", "chum", "sockeye"))
ests = ests[order(ests$species),]
ests = ests[,-which(colnames(ests) %in% c("date", "gear", "stratum"))]

# open a graphics device
png(file.path(out_dir, "harvest-by-species.png"), h = 5 * ppi, w = 7 * ppi, res = ppi)

# set graphics parameters
par(mar = c(2,4,2,2), tcl = -0.15, mgp = c(2,0.35,0))

# make the barplot
mp = barplot(ests$mean, beside = TRUE, ylim = c(0, max(ests$upr95ci, na.rm = TRUE) * 1.05),
             names.arg = c("Total", "Chinook", "Chum", "Sockeye"), yaxt = "n")

# add error bars (suppress warnings about NA length arrows)
suppressWarnings(arrows(mp, ests$lwr95ci, mp, ests$upr95ci, code = 3, angle = 90, length = 0.05))

# handle axes
usr = par("usr")
segments(usr[1], usr[3], usr[2], usr[3], xpd = TRUE)
at_y = axisTicks(usr[3:4], log = FALSE)
axis(side = 1, at = mp, labels = FALSE)
axis(side = 2, at = at_y, labels = prettyNum(at_y, big.mark = ",", scientific = FALSE), las = 2)
mtext(side = 2, line = 3, "Harvest")

# close the graphics device
dev.off()

##### TOTAL HARVEST BY SPECIES AND STRATUM #####

# read in the output
ests = read.csv(file.path(in_dir, "all-harvest-summaries.csv"))

# subset only estimates that will be plotted
ests = subset(ests, gear == "total" & date == "total" & stratum != "total" & species != "total")

# extract the mean, lwr, and upr estimates by species and stratum, formatted for barplotting
means = as.matrix(reshape2::dcast(ests, species ~ stratum, value.var = "mean")[,-1])
lwrs = as.matrix(reshape2::dcast(ests, species ~ stratum, value.var = "lwr95ci")[,-1])
uprs = as.matrix(reshape2::dcast(ests, species ~ stratum, value.var = "upr95ci")[,-1])

# open a graphics device
png(file.path(out_dir, "harvest-by-species-and-stratum.png"), h = 5 * ppi, w = 7 * ppi, res = ppi)

# set graphics parameters
par(mar = c(2,4,2,2), tcl = -0.15, mgp = c(2,0.35,0))

# make the grouped barplot
mp = barplot(means, beside = TRUE, ylim = c(0, max(uprs, na.rm = TRUE) * 1.05), names.arg = c("A", "B", "C", "D"), yaxt = "n", col = c("grey50", "grey70", "grey90"))

# handle axes
usr = par("usr")
segments(usr[1], usr[3], usr[2], usr[3], xpd = T)
axis(side = 1, at = mp[2,], labels = FALSE)
at_y = axisTicks(usr[3:4], log = FALSE)
axis(side = 2, at = at_y, labels = prettyNum(at_y, big.mark = ",", scientific = FALSE), las = 2)
mtext(side = 2, line = 3, "Harvest")

# add error abrs
arrows(mp, lwrs, mp, uprs, code = 3, angle = 90, length = 0.05)

# add a legend
legend("topleft", legend = c("Chinook", "Chum", "Sockeye"), pch = 22, pt.cex = 2.5, pt.bg = c("grey50", "grey70", "grey90"), bty = "n")

# close the device
dev.off()
