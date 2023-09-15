# ::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
# SCRIPT TO COMPILE ALL ESTIMATES AND DATA ACROSS OPENERS #
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::: #

# clear the workspace
rm(list = ls(all = TRUE))

# load the info about odd openers
source("00-specify-odd-openers.R")

# extract the names of all openers with data
dirs = dir("raw-data-files", full.names = TRUE, pattern = "_[0-9][0-9]")

# convert to dates
dates = stringr::str_remove(basename(dirs), "_[:alpha:]+$") |>
  stringr::str_replace_all("_", "-")

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

# create placeholders to handle cases where some strata were missing
flight_empty = c(NA, NA, NA, rep(NA, length(KuskoHarvEst:::strata_names$stratum) * 2))
names(flight_empty) = c(
  c("flight", "start_time", "end_time"),
  paste0(KuskoHarvEst:::strata_names$stratum, "_drift"),
  paste0(KuskoHarvEst:::strata_names$stratum, "_set")
)
flight_empty = flight_empty[order(names(flight_empty))]
flight_emtpy = as.data.frame(t(flight_empty))
strata_empty = rep(NA, length(KuskoHarvEst:::strata_names$stratum))
names(strata_empty) = KuskoHarvEst:::strata_names$stratum

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
  interview_data = suppressWarnings(KuskoHarvEst::prepare_interviews(interview_files, include_salmon = "all", include_nonsalmon = "none"))
  interview_df = rbind(interview_df, interview_data)

  # prepare raw flight data files for this opener: treat a bit different if missing
  flight_raw = read.csv(flight_file)
  if (dates[i] %in% no_flight_openers) {
    flight_data = KuskoHarvEst::prepare_flights(flight_file)
    flight_data$start_time = flight_data$end_time = KuskoHarvUtils::combine_datetime(flight_raw$date[1], "0:00")
  } else {
    flight_data = KuskoHarvEst::prepare_flights(flight_file)
  }

  # combine flight data with output from other openers
  for (j in 1:nrow(flight_data)) {
    new_row = flight_empty
    new_row[colnames(flight_data)] = flight_data[j,]
    flight_df = rbind(flight_df, as.data.frame(new_row))
  }

  # produce set effort estimates
  if (!(dates[i] %in% c(no_set_openers, no_flight_openers))) {
    set_effort_info = KuskoHarvEst::estimate_effort(
      interview_data = interview_data,
      flight_data = flight_data,
      gear = "set", method = "max_per_stratum"
    )
    to_save = strata_empty
    to_save[names(set_effort_info$effort_est_stratum)] = set_effort_info$effort_est_stratum
  } else {
    to_save = strata_empty
    set_effort_info = list(gear = "set", effort_est_total = NA, effort_est_stratum = strata_empty)
  }

  # combine effort estimates with those from other openers
  set_effort_tmp = c(to_save, total = sum(to_save, na.rm = TRUE))
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
    to_save = strata_empty
    to_save[names(drift_effort_info$effort_est_stratum)] = drift_effort_info$effort_est_stratum
  } else {
    to_save = strata_empty
    drift_effort_info = list(gear = "drift", effort_est_total = NA, effort_est_stratum = strata_empty)
  }

  # combine effort estimates with those from other openers
  drift_effort_tmp = c(to_save, total = sum(to_save, na.rm = TRUE))
  drift_effort_tmp = data.frame(date = dates[i], gear = "drift", stratum = names(drift_effort_tmp), estimate = unname(drift_effort_tmp))
  drift_effort_df = rbind(drift_effort_df, drift_effort_tmp); rm(drift_effort_tmp)

  # estimate set net harvest
  if (!(dates[i] %in% c(no_set_openers, no_flight_openers))) {
    tmp_boot_out_set = KuskoHarvEst::bootstrap_harvest(
      interview_data = interview_data,
      effort_info = set_effort_info,
      gear = "set",
      n_boot = n_boot,
      stratify_interviews = FALSE
    )
  } else {
    tmp_boot_out_set = NULL
  }

  # estimate drift net harvest
  if(!(dates[i] %in% c(set_only_openers, no_flight_openers))) {
    tmp_boot_out_drift = KuskoHarvEst::bootstrap_harvest(
      interview_data = interview_data,
      effort_info = drift_effort_info,
      gear = "drift",
      n_boot = n_boot,
      stratify_interviews = TRUE
    )
  } else {
    tmp_boot_out_drift = NULL
  }

  # combine drift, set, and total into one data frame of bootstrap output
  if (!is.null(tmp_boot_out_drift) | !is.null(tmp_boot_out_set)) {
    tmp_boot_out = KuskoHarvEst::combine_boot_out(boot_out_drift = tmp_boot_out_drift, boot_out_set = tmp_boot_out_set)
  } else {
    tmp_boot_out = expand.grid(iter = NA, date = dates[i], gear = unique(boot_out$gear), stratum = unique(boot_out$stratum))
    tmp_boot_out = data.frame(tmp_boot_out, chinook = NA, chum = NA, sockeye = NA, coho = NA, total = NA)
    tmp_boot_out = tmp_boot_out[order(tmp_boot_out$gear, tmp_boot_out$stratum),]
    rownames(tmp_boot_out) = NULL
  }

  # combine bootstrap output with output from other openers
  boot_out = rbind(boot_out, tmp_boot_out)
}

# end the timer and print a message
stoptime = Sys.time()
cat("\nEstimation Time Elapsed:", format(round(stoptime - starttime, 2)))

##### CALCULATE SEASON-WIDE TOTALS FOR EACH BOOTSTRAP SAMPLE #####

# obtain season totals by species, gear, and stratum and combine with other bootstrapped output
total_boot = boot_out |>
  # group as needed
  dplyr::group_by(iter, gear, stratum) |>

  # calculate season totals for each group
  dplyr::summarize(chinook = sum(chinook, na.rm = TRUE), chum = sum(chum, na.rm = TRUE), sockeye = sum(sockeye, na.rm = TRUE), coho = sum(coho, na.rm = TRUE), total = sum(total, na.rm = TRUE)) |>

  # add the "date" variable
  dplyr::mutate(date = "total") |>

  # reorder the columns
  dplyr::select(iter, date, gear, stratum, chinook, chum, sockeye, coho, total) |>

  # change object type
  as.data.frame()

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
