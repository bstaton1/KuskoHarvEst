# :::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
# SCRIPT TO CREATE OUTPUT FOR BUILDING FINAL REPORT TABLES #
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::: #

# clear the workspace
rm(list = ls(all = TRUE))

# load the info about odd openers
source("00-specify-odd-openers.R")

# input and output directories
in_dir = "compiled-output"
out_dir = "tables-for-report"
if (!dir.exists(out_dir)) dir.create(out_dir)

##### TABLE 1: ALL FLIGHT COUNTS OF DRIFT NETS #####

# load output
flight_df = readRDS(file.path(in_dir, "all_flight_data.rds"))

# format dates/times
flight_df$date = KuskoHarvUtils::basic_date(flight_df$start_time)
flight_df$start_time = paste0(stringr::str_pad(lubridate::hour(flight_df$start_time), 2, "left", "0"),
                              ":", stringr::str_pad(lubridate::minute(flight_df$start_time), 2, "left", "0"))
flight_df$end_time = paste0(stringr::str_pad(lubridate::hour(flight_df$end_time), 2, "left", "0"),
                            ":", stringr::str_pad(lubridate::minute(flight_df$end_time), 2, "left", "0"))

# extract/format only important variables
flight_df = flight_df[,c("date", "start_time", "end_time", "A_drift", "B_drift", "C_drift", "D1_drift")]
colnames(flight_df) = stringr::str_remove(colnames(flight_df), "_drift")

# calculate the total count per flight
flight_df$total = rowSums(flight_df[,c("A", "B", "C", "D1")])

# export the output file
write.csv(flight_df, file.path(out_dir, "all-driftnet-counts.csv"), row.names = FALSE)

##### TABLE 2: ALL FLIGHT COUNTS OF SET NETS #####

# load output
flight_df = readRDS(file.path(in_dir, "all_flight_data.rds"))

# format dates/times
flight_df$date = KuskoHarvUtils::basic_date(flight_df$start_time)
flight_df$start_time = paste0(stringr::str_pad(lubridate::hour(flight_df$start_time), 2, "left", "0"),
                              ":", stringr::str_pad(lubridate::minute(flight_df$start_time), 2, "left", "0"))
flight_df$end_time = paste0(stringr::str_pad(lubridate::hour(flight_df$end_time), 2, "left", "0"),
                            ":", stringr::str_pad(lubridate::minute(flight_df$end_time), 2, "left", "0"))

# extract/format only important variables
flight_df = flight_df[,c("date", "start_time", "end_time", "A_set", "B_set", "C_set", "D1_set")]
colnames(flight_df) = stringr::str_remove(colnames(flight_df), "_set")

# calculate the total count per flight
flight_df$total = rowSums(flight_df[,c("A", "B", "C", "D1")])

# export the output file
write.csv(flight_df, file.path(out_dir, "all-setnet-counts.csv"), row.names = FALSE)

##### TABLE 3: ALL EFFORT ESTIMATES FOR DRIFT NETS #####

# load output
effort_df = readRDS(file.path(in_dir, "all_drift_effort_estimates.rds"))

# reshape the effort estimates
effort_df = reshape2::dcast(effort_df, date ~ stratum, value.var = "estimate")

# reformat the date variable
effort_df$date = KuskoHarvUtils::basic_date(effort_df$date)

# export the output file
write.csv(effort_df, file.path(out_dir, "all-driftnet-effort-estimates.csv"), row.names = FALSE)

##### TABLE 4: ALL EFFORT ESTIMATES FOR SET NETS #####

# load output
effort_df = readRDS(file.path(in_dir, "all_set_effort_estimates.rds"))

# reshape the effort estimates
effort_df = reshape2::dcast(effort_df, date ~ stratum, value.var = "estimate")

# reformat the date variable
effort_df$date = KuskoHarvUtils::basic_date(effort_df$date)

# export the output file
write.csv(effort_df, file.path(out_dir, "all-setnet-effort-estimates.csv"), row.names = FALSE)

##### TABLES 5, 6, 7: SUMMARIZED HARVEST ESTIMATES BY OPENER, STRATUM, AND SPECIES #####
# ONE FILE FOR EACH OF DRIFT, SET, AND DRIFT+SET
# ALSO A NUMERIC VERSION FOR CREATING BARPLOTS LATER

# load output
boot_out = readRDS(file.path(in_dir, "all_bootstrap_harvest_estimates.rds"))

# empty containers for output
out_character = NULL
out_numeric = NULL

# the pools to create combos for calculating individual summaries for
date_pool = unique(boot_out$date)
gear_pool = c("drift", "set", "total")
species_pool = c("chinook", "chum", "sockeye", "total")
strata_pool = c("A", "B", "C", "D1", "total")

# loop through each combination
for (date in date_pool) {
  for (gear in gear_pool) {
    for (species in species_pool) {
      for (stratum in strata_pool) {

        # store summarized estimates as characters: mean (lwr95 -- upr95)
        tmp_character = data.frame(date = date, gear = gear, species = species, stratum = stratum,
                                   estimate = KuskoHarvEst::report(spp = species, stratum = stratum, date = date, gear = gear))
        out_character = rbind(out_character, tmp_character)

        # store summarized estimates as numeric: columns for mean, lwr95, upr95
        tmp_numeric = data.frame(date = date, gear = gear, species = species, stratum = stratum)
        tmp_ests = KuskoHarvEst::report(spp = species, stratum = stratum, date = date, gear = gear, return_numeric = TRUE)
        tmp_numeric$mean = tmp_ests["mean"]
        tmp_numeric$lwr95ci = tmp_ests["2.5%"]
        tmp_numeric$upr95ci = tmp_ests["97.5%"]
        out_numeric = rbind(out_numeric, tmp_numeric)

      }
    }
  }
}

# coerce appropriate cases to NA
out_numeric[out_numeric$mean == "NaN","mean"] = NA
out_numeric[out_numeric$date %in% no_flight_openers,c("mean", "lwr95ci", "upr95ci")] = NA
out_character[out_character$estimate == "NaN (NA -- NA)","estimate"] = NA
out_character[out_character$date %in% no_flight_openers,"estimate"] = NA

# reformat the date
out_character$date[out_character$date != "total"] = KuskoHarvUtils::basic_date(out_character$date[out_character$date != "total"])
out_character$date = factor(out_character$date, levels = c(unique(flight_df$date), "total"))

# reformat the lwr -- upr separator
out_character$estimate = stringr::str_replace(out_character$estimate, " -- ", "-")

# capitalize columns
out_character$date = KuskoHarvEst:::capitalize(out_character$date)
out_character$species = KuskoHarvEst:::capitalize(out_character$species)
out_character$stratum = KuskoHarvEst:::capitalize(out_character$stratum)

# subset out each gear and reshape
total_table = reshape2::dcast(subset(out_character, gear == "total"), date + species ~ stratum, value.var = "estimate")
drift_table = reshape2::dcast(subset(out_character, gear == "drift"), date + species ~ stratum, value.var = "estimate")
set_table = reshape2::dcast(subset(out_character, gear == "set"), date + species ~ stratum, value.var = "estimate")

# export each table
write.csv(total_table, file.path(out_dir, "total-harvest-table.csv"), row.names = FALSE)
write.csv(drift_table, file.path(out_dir, "driftnet-harvest-table.csv"), row.names = FALSE)
write.csv(set_table, file.path(out_dir, "setnet-harvest-table.csv"), row.names = FALSE)

# export numeric version -- useful in plotting later
write.csv(out_numeric, file.path(in_dir, "all-harvest-summaries.csv"), row.names = FALSE)
