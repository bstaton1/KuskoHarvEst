#' Prepare interview data files
#'
#' Calls [prepare_interviews_one()] multiple times, once per interview data file
#'   and performs data quality checks
#'
#' @param input_files Character; vector of file names that contain interview data
#' @param include_salmon Character; accepted options are `"all"`, `"none"`, or any combination of acceptable salmon species names
#' @param include_nonsalmon Character; accepted options are `"all"`, `"none"`, or any combination of acceptable nonsalmon species names
#' @param include_goals Logical; should the fisher's reported progress towards meeting their season-wide harvest goals be returned?
#' @param include_village Logical; should the village of the fisher be included in the output?
#' @note For the list of acceptable species names, please see `KuskoHarvEst:::species_names`.
#' @export

prepare_interviews = function(input_files, include_salmon = "all", include_nonsalmon = "none", include_goals = FALSE, include_village = FALSE) {

  # get the value of all non-filename arguments in a list to pass to prepare_interviews_one()
  args = as.list(environment())
  args = args[-which(names(args) == "input_files")]

  # read in and format raw interview data
  interview_data_list = lapply(input_files, function(file) do.call(prepare_interviews_one, append(list(input_file = file), args)))

  # combine individual list elements into a data frame
  interview_data = do.call(rbind, interview_data_list)

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

  # create empty note objects for checks performed within this function
  impossible_trip_notes = rep(NA, nrow(interview_data))
  impossible_soak_notes = rep(NA, nrow(interview_data))
  outlier_cpt_notes = rep(NA, nrow(interview_data))

  # perform checks for impossible trip times
  impossible_trips = !is_possible_trip(interview_data)
  if (any(impossible_trips)) {
    impossible_trip_notes[impossible_trips] = paste0("Impossible trip times reported so have been discarded.")
    interview_data$suit_effort[impossible_trips] = FALSE
    interview_data$trip_duration[impossible_trips] = NA
    interview_data$trip_start[impossible_trips] = NA
    interview_data$trip_end[impossible_trips] = NA
    warning("\n", sum(impossible_trips), " interview(s) had impossible trip times reported.\nFor these records, the trip times have been set to NA,\nand a note has been included in the output.\nYou may wish to see if there is a typo in the raw data for these records.")
  }

  # perform checks for impossible soak times
  # if any are found, change the soak time to be the same as the trip duration, and include a note
  impossible_soaks = !is_possible_soak(interview_data)
  if (any(impossible_soaks)) {
    impossible_soak_notes[impossible_soaks] = paste0("Long soak duration (", interview_data[impossible_soaks,"soak_duration"], ") edited to trip duration (", interview_data[impossible_soaks,"trip_duration"], ")")
    interview_data$soak_duration[impossible_soaks] = interview_data$trip_duration[impossible_soaks]
    interview_data$suit_cr_reliable = suitable_for(interview_data, "catch_rate_info_reliable")
    interview_data$suit_avg_soak = suitable_for(interview_data, "avg_soak")
    warning("\n", sum(impossible_soaks), " interview(s) had soak duration reported longer than trip duration.\nFor these records, the soak duration has been set to the trip duration,\nand a note has been included in the output.")
  }

  # perform checks for if the average catch per trip is an outlier
  # only perform for salmon
  if (!("none" %in% include_salmon)) {
    cpt_outliers = is_catch_per_trip_outlier(interview_data)
    if (any(cpt_outliers)) {
      outlier_cpt_notes[cpt_outliers] = "Catch per trip highly influential, catch rate rate, soak time, and net length deemed unsuitable for average"
      interview_data$suit_cr_reliable[cpt_outliers] = FALSE
      interview_data$suit_avg_soak[cpt_outliers] = FALSE
      interview_data$suit_avg_net[cpt_outliers] = FALSE
      warning("\n", sum(cpt_outliers), " interview(s) had a large influence on the average catch per trip.\nFor these records, the catch rate info has been deemed unreliable,\nand the soak time and net length will not be used in the average.")
    }
  }

  # extract notes on suitability
  suitability_notes = suitable_for(interview_data, task = "notes")

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
