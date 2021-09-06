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
flight_df$date = KuskoHarvEst:::basic_date(flight_df$start_time)
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
flight_df$date = KuskoHarvEst:::basic_date(flight_df$start_time)
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

