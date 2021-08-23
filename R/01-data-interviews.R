#' Prepare interview data files
#'
#' Calls [prepare_interviews_one()] multiple times, once per interview data file
#'   and performs data quality checks
#'
#' @param input_files Character; vector of file names that contain interview data
#' @param ... Optional arguments passed to [prepare_interviews_one()]
#' @export

prepare_interviews = function(input_files, ...) {

  # check to make sure all global options are set
  check_options()

  # read in and format raw interview data
  interview_data_list = lapply(input_files, function(file) prepare_interviews_one(file, ...))

  # combine individual list elements into a data frame
  interview_data = unlist_dfs(interview_data_list)

  # check to make sure only one unique start date is found in the data
  start_dates = unique_start_dates(interview_data)
  if (length(start_dates) > 1) {
    stop ("More than one unique start date was found in the interview data:\n(", paste(start_dates, collapse = "; "), ")\nYou must edit the raw data to ensure all interviews are from trips that started on the same day.")
  }

  # discard any trips lacking gear type information
  # if the interview is lacking gear, it is totally useless.
  # trip times can't be used for effort estimation b/c gear uncertainty
  # catch rate and average net length and soak time same reason
  no_gear = !has_gear(interview_data)
  if (any(no_gear)) {
    interview_data = interview_data[-which(no_gear),]
    warning("\n", sum(no_gear), " interview(s) had missing gear type information.\nThese records have been discarded since they\ncannot be used for anything.")
  }

  # perform suitability checks and combine logical flags with the data
  tasks = c("effort", "catch_rate_info", "catch_rate_info_reliable", "avg_soak", "avg_net_length")
  suitable = sapply(tasks, suitable_for, interview_data = interview_data)
  colnames(suitable) = paste0("suit_", c("effort", "cr_info", "cr_reliable", "avg_soak", "avg_net"))
  interview_data = cbind(interview_data, suitable)

  # extract notes on suitability
  suitability_notes = suitable_for(interview_data, task = "notes")

  # create empty note objects for checks performed within this function
  impossible_trip_notes = rep(NA, nrow(interview_data))
  impossible_soak_notes = rep(NA, nrow(interview_data))
  outlier_cpt_notes = rep(NA, nrow(interview_data))

  # perform checks for impossible trip times
  impossible_trips = !is_possible_trip(interview_data)
  if (any(impossible_trips)) {
    impossible_trip_notes[impossible_trips] = paste0("Impossible trip times reported, trip times set to NA.")
    interview_data$suit_effort[impossible_trips] = FALSE
    interview_data$trip_duration[impossible_trips] = NA
    interview_data$trip_start[impossible_trips] = NA
    interview_data$trip_end[impossible_trips] = NA
    warning("\n", sum(impossible_trips), " interview(s) had impossible trip times reported.\nFor these records, the trip times have been set to NA,\nand a note has been included in the output.")
  }

  # perform checks for impossible soak times
  # if any are found, change the soak time to be the same as the trip duration, and include a note
  impossible_soaks = !is_possible_soak(interview_data)
  if (any(impossible_soaks)) {
    impossible_soak_notes[impossible_soaks] = paste0("Impossible soak duration (", interview_data[impossible_soaks,"soak_duration"], ") edited to trip duration")
    interview_data$soak_duration[impossible_soaks] = interview_data$trip_duration[impossible_soaks]
    warning("\n", sum(impossible_soaks), " interview(s) had soak duration reported longer than trip duration.\nFor these records, the soak duration has been set to the trip duration,\nand a note has been included in the output.")
  }

  # perform checks for if the average catch per trip is an outlier
  cpt_outliers = is_catch_per_trip_outlier(interview_data)
  if (any(cpt_outliers)) {
    outlier_cpt_notes[cpt_outliers] = "Interview has a large influence on the average catch per trip, its catch rate info, soak time, and net length have been deemed unsuitable"
    interview_data$suit_cr_reliable[cpt_outliers] = FALSE
    interview_data$suit_avg_soak[cpt_outliers] = FALSE
    interview_data$suit_avg_net[cpt_outliers] = FALSE
    warning("\n", sum(cpt_outliers), " interview(s) had a large influence on the average catch per trip.\nFor these records, the catch rate info has been deemed unreliable,\nand the soak time and net length will not be used in the average.")
  }

  # combine the notes from each record
  notes = sapply(1:nrow(interview_data), function(i) {
    paste(suitability_notes[i], impossible_trip_notes[i], impossible_soak_notes[i], outlier_cpt_notes[i], sep = "; ")
  })
  notes = stringr::str_remove_all(notes, "NA; ")
  notes = stringr::str_remove_all(notes, "NA")
  notes = stringr::str_remove_all(notes, "; $")
  notes = stringr::str_remove_all(notes, "^; ")
  notes[notes == ""] = NA
  interview_data$note = notes

  # return the output
  return(interview_data)
}
