
##### FUNCTIONS TO CHECK WHETHER EACH CRITICAL DATA SOURCE EXISTS #####

#' Check if the gear was recorded
has_gear = function(interview_data) {
  !is.na(interview_data$gear)
}

#' Check if the trip times were recorded
has_trip_times = function(interview_data) {
  !is.na(interview_data$trip_start) & !is.na(interview_data$trip_end)
}

#' Check if the net length was recorded
has_net_length = function(interview_data) {
  !is.na(interview_data$net_length)
}

#' Check if soak duration was recorded
has_soak = function(interview_data) {
  !is.na(interview_data$soak_duration)
}

##### FUNCTIONS TO CHECK FOR TIME ANOMALIES #####

#' Check for impossible trip times
#'
#' @details If the trip end time is before the trip start time, this is impossible and an error.
#' The user should fix this entry in the data set before proceeding.

is_possible_trip = function(interview_data) {
  ifelse(!has_trip_times(interview_data), TRUE, ifelse(interview_data[,"trip_start"] < interview_data[,"trip_end"], TRUE, FALSE))
}

#' Check for impossible soak times
#'
#' @details If the soak duration is longer than the trip duration, this is impossible and an error.
#' The user should fix this entry in the data set before proceeding.
#' @export

is_possible_soak = function(interview_data) {
  ifelse(!has_trip_times(interview_data) | !has_soak(interview_data), TRUE,
         ifelse(interview_data[,"soak_duration"] <= interview_data[,"trip_duration"], TRUE, FALSE))
}

##### MISC CHECKS FOR DATA ACCURACY #####

#' Check if interview is for a completed trip
#'
#' @details Data need special care if from an incomplete trip.
#'   E.g., soak time should not be used in calculation of the average across trips,
#'   and if the soak time is excessively short at the time of the interview relative to completed
#'   trips, it shouldn't be used to inform the average catch rate.
#' @export

is_complete_trip = function(interview_data) {
  interview_data$source != "LE"
}

#' Check if the net characteristics are as expected
#'
#' @details Nets that are extremely long are probably mis-recorded and should not be included.
#' Could feasibly include a mesh cutoff here if there was interest.

is_normal_net = function(interview_data, length_cut = 350) {
  ifelse(!has_net_length(interview_data), TRUE, ifelse(interview_data[,"net_length"] <= length_cut, TRUE, FALSE))
}

#' Check soak time for outliers
#'
#' @details Consider soak time an outlier if its value is larger than
#'   the mean plus XSDs of the remaining values for that gear type. The user
#'   chooses the value of X with the argument `sd_cut`. This function applies
#'   to completed trips only -- records that are for incomplete trips or that
#'   do not have soak time recorded will have `FALSE` returned.

is_soak_outlier = function(interview_data, sd_cut = 3) {
  # extract the soak hours
  soak_hrs = as.numeric(interview_data$soak_duration, "hours")

  # determine which trips were completed trip interviews
  is_complete = is_complete_trip(interview_data)

  # container object
  out = logical(nrow(interview_data))

  # for completed trips only, is the soak time greater than XSDs from the mean?
  # remove the sample in question before calculating mean and sd
  # and do this on a gear-specific basis
  for (i in 1:nrow(interview_data)) {
    if (is_complete[i] & has_soak(interview_data)[i]) {
      # extract the soak hours for all relevant interviews except this one
      soak_hrs_without = soak_hrs[-i][interview_data$gear[-i] == interview_data$gear[i] & is_complete[-i]]

      # calculate the mean and SD of remaining soak times
      sd_without = sd(soak_hrs_without, na.rm = T)
      mn_without = mean(soak_hrs_without, na.rm = T)

      # perform the test
      out[i] = soak_hrs[i] > (mn_without + (sd_cut * sd_without))
    } else {
      next()
    }
  }

  return(out)
}
