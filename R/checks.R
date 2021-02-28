
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
