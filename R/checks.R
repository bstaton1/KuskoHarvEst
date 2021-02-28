
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

