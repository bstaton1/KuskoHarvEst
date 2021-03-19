#' Prepare all interview data
#'
#' @export

prepare_interviews_all = function(input_files, ...) {
  # read in and format raw interview data
  interview_data_list = lapply(input_files, function(file) prepare_interviews(file, ...))

  # combine individual list elements into a data frame
  interview_data = unlist_dfs(interview_data_list)

  # perform suitability checks and combine logical flags with the data
  tasks = c("effort", "catch_rate_info", "catch_rate_info_reliable", "avg_soak", "avg_net_length")
  suitable = sapply(tasks, suitable_for, interview_data = interview_data)
  colnames(suitable) = paste0("suit_", c("effort", "cr_info", "cr_reliable", "avg_soak", "avg_net"))
  interview_data = cbind(interview_data, suitable)

  # discard any trips lacking gear type information
  # if the interview is lacking gear, it is totally useless.
  # trip times can't be used for effort estimation b/c gear uncertainty
  # catch rate and average net length and soak time same reason
  no_gear = !has_gear(interview_data)
  if (any(no_gear)) {
    interview_data = interview_data[-which(no_gear),]
    warning("\n", sum(no_gear), " interview(s) had missing gear type information.\nThese records have been discarded since they\ncannot be used for anything.")
  }

  # perform checks for impossible soak times
  # if any are found, change the soak time to be the same as the trip duration, and include a note
  interview_data$note = NA
  impossible_soaks = !is_possible_soak(interview_data)
  if (any(impossible_soaks)) {
    interview_data$note[impossible_soaks] = paste0("Impossible soak duration (", interview_data[impossible_soaks,"soak_duration"], ") edited to trip duration")
    interview_data$soak_duration[impossible_soaks] = interview_data$trip_duration[impossible_soaks]
    warning(sum(impossible_soaks), " interview(s) had soak duration reported longer than trip duration.\nFor these records, the soak duration has been set to the trip duration,\nand a note has been included in the output.")
  }

  # perform checks for impossible trip times
  impossible_trips = !is_possible_soak(interview_data)
  if (any(impossible_trips)) {
    interview_data$note[impossible_trips] = paste0("Impossible trip times reported, record is excluded from effort estimation.")
    interview_data$suit_effort[impossible_trips] = FALSE
    warning(sum(impossible_trips), " interview(s) had impossible trip times reported.\nFor these records, the trip times will not be used in effort estimation,\nand a note has been included in the output.")
  }

  # return the output
  return(interview_data)
}
