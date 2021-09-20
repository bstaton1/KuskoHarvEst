# ::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
# SCRIPT TO COMPILE ALL ESTIMATES AND DATA ACROSS OPENERS #
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::: #

# clear the workspace
rm(list = ls(all = TRUE))

# load the info about odd openers
source("00-specify-odd-openers.R")

# set the necessary KuskoHarvEst options
options(soak_sd_cut = 3, net_length_cut = 350, catch_per_trip_cut = 0.1, central_fn = mean, pooling_threshold = 10)

# extract the names of all openers with data
dirs = dir("raw-data-files", full.names = TRUE, pattern = "_[0-9][0-9]$")

# convert to dates
dates = stringr::str_replace_all(basename(dirs), "_", "-")

# output directory
out_dir = "compiled-output"
if (!dir.exists(out_dir)) dir.create(out_dir)

# number of bootstrap iterations
n_boot = 1000

# empty containers to store output across openers
set_effort_df = NULL
drift_effort_df = NULL
interview_df = NULL
flight_df = NULL
boot_out = NULL

# start a timer
starttime = Sys.time()

# loop through openers and generate harvest and effort estimates for each
for (i in 1:length(dirs)) {

  # print a progress message
  cat("\rOpener: ", as.character(dates[i]), " (", stringr::str_pad(i, width = nchar(length(dates)), pad = " "), "/", length(dates), ")", sep = "")

  # extract and categorize file names for this opener
  files = list.files(dirs[i], full.names = TRUE)
  interview_files = files[stringr::str_detect(files, "BBH|CBM|FC")]
  flight_file = files[stringr::str_detect(files, "Flight")]

  # prepare raw interview data files for this opener
  interview_data = suppressWarnings(KuskoHarvEst::prepare_interviews(interview_files))
  interview_df = rbind(interview_df, interview_data)

  # prepare raw flight data files for this opener: treat a bit different if missing
  flight_raw = read.csv(flight_file)
  if (dates[i] %in% no_flight_openers) {
    flight_data = KuskoHarvEst::prepare_flights(flight_file)
    flight_data$start_time = flight_data$end_time = KuskoHarvEst:::combine_datetime(flight_raw$date[1], "0:00")
  } else {
    flight_data = KuskoHarvEst::prepare_flights(flight_file)
  }

  # combine flight data with output from other openers
  flight_df = rbind(flight_df, flight_data)

  # produce set effort estimates
  if (!(dates[i] %in% c(no_set_openers, no_flight_openers))) {
    set_effort_info = KuskoHarvEst::estimate_effort(
      interview_data = interview_data,
      flight_data = flight_data,
      gear = "set", method = "max_per_stratum"
    )
  } else {
    set_effort_info = list(gear = "set", effort_est_total = NA, effort_est_stratum = c(A = NA, B = NA, C = NA, D1 = NA))
  }

  # combine effort estimates with those from other openers
  set_effort_tmp = c(set_effort_info$effort_est_stratum, total = set_effort_info$effort_est_total)
  set_effort_tmp = data.frame(date = dates[i], gear = "set", stratum = names(set_effort_tmp), estimate = unname(set_effort_tmp))
  set_effort_df = rbind(set_effort_df, set_effort_tmp); rm(set_effort_tmp)

  # produce drift effort estimates
  if (!(dates[i] %in% c(set_only_openers, no_flight_openers))) {
    # effort estimate
    drift_effort_info = KuskoHarvEst::estimate_effort(
      interview_data = interview_data,
      flight_data = flight_data,
      gear = "drift", method = "dbl_exp"
    )
  } else {
    drift_effort_info = list(gear = "drift", effort_est_total = NA, effort_est_stratum = c(A = NA, B = NA, C = NA, D1 = NA))
  }

  # combine effort estimates with those from other openers
  drift_effort_tmp = c(drift_effort_info$effort_est_stratum, total = drift_effort_info$effort_est_total)
  drift_effort_tmp = data.frame(date = dates[i], gear = "drift", stratum = names(drift_effort_tmp), estimate = unname(drift_effort_tmp))
  drift_effort_df = rbind(drift_effort_df, drift_effort_tmp); rm(drift_effort_tmp)

  # estimate set net harvest
  tmp_boot_out_set = KuskoHarvEst::bootstrap_harvest(
    interview_data = interview_data,
    effort_info = set_effort_info,
    gear = "set",
    n_boot = n_boot,
    stratify_interviews = FALSE
  )

  # estimate drift net harvest
  tmp_boot_out_drift = KuskoHarvEst::bootstrap_harvest(
    interview_data = interview_data,
    effort_info = drift_effort_info,
    gear = "drift",
    n_boot = n_boot,
    stratify_interviews = TRUE
  )

  # sum up across gears
  tmp_boot_out_total = data.frame(iter = tmp_boot_out_drift$iter,
                                  date = tmp_boot_out_drift$date,
                                  gear = "total",
                                  stratum = tmp_boot_out_drift$stratum,
                                  chinook = rowSums(cbind(tmp_boot_out_drift[,"chinook"], tmp_boot_out_set[,"chinook"]), na.rm = TRUE),
                                  chum = rowSums(cbind(tmp_boot_out_drift[,"chum"], tmp_boot_out_set[,"chum"]), na.rm = TRUE),
                                  sockeye = rowSums(cbind(tmp_boot_out_drift[,"sockeye"], tmp_boot_out_set[,"sockeye"]), na.rm = TRUE),
                                  total = rowSums(cbind(tmp_boot_out_drift[,"total"], tmp_boot_out_set[,"total"]), na.rm = TRUE)
  )

  # combine drift, set, and total into one data frame of bootstrap output
  tmp_boot_out = rbind(tmp_boot_out_total, tmp_boot_out_drift, tmp_boot_out_set)

  # combine bootstrap output with output from other openers
  boot_out = rbind(boot_out, tmp_boot_out)
}

# end the timer and print a message
stoptime = Sys.time()
cat("\nEstimation Time Elapsed:", format(round(stoptime - starttime, 2)))

##### CALCULATE SEASON-WIDE TOTALS FOR EACH BOOTSTRAP SAMPLE #####
library(magrittr)

# obtain season totals by species, gear, and stratum and combine with other bootstrapped output
total_boot = boot_out %>%
  # group as needed
  dplyr::group_by(iter, gear, stratum) %>%

  # calculate season totals for each group
  dplyr::summarize(chinook = sum(chinook, na.rm = TRUE), chum = sum(chum, na.rm = TRUE), sockeye = sum(sockeye, na.rm = TRUE), total = sum(total, na.rm = TRUE)) %>%

  # add the "date" variable
  dplyr::mutate(date = "total") %>%

  # reorder the columns
  dplyr::select(iter, date, gear, stratum, chinook, chum, sockeye, total) %>%

  # change object type
  as.data.frame

# combine with the opener-specific bootstrap output
boot_out$date = as.character(boot_out$date)
boot_out = rbind(boot_out, total_boot)

##### EXPORT OUTPUT FILES #####

# total set net effort estimates by opener and stratum
saveRDS(set_effort_df, file.path(out_dir, "all_set_effort_estimates.rds"))

# total drift net effort estimates by opener and stratum
saveRDS(drift_effort_df, file.path(out_dir, "all_drift_effort_estimates.rds"))

# compiled raw interview data
saveRDS(interview_df, file.path(out_dir, "all_interview_data.rds"))

# compiled raw flight data
saveRDS(flight_df, file.path(out_dir, "all_flight_data.rds"))

# bootstrap estimates
saveRDS(boot_out, file.path(out_dir, "all_bootstrap_harvest_estimates.rds"))

# print a message when complete
cat("\n\nAll Calculations Complete")
