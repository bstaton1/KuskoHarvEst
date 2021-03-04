#' Determine which records are suitable for particular tasks
#'
#' A wrapper around a variety of "checking" functions, all of which are non-exported.
#'
#' @export

suitable_for = function(interview_data, task) {

  accepted_tasks = c("nothing", "effort", "catch_rate_info", "catch_rate_info_reliable", "avg_soak", "avg_net_length")
  if (!(task %in% accepted_tasks)) stop ("task must be one of: ", paste(paste0("'", accepted_tasks, "'"), collapse = ", "))

  # if the interview is lacking gear, it is totally useless.
  # trip times can't be used for effort estimation b/c gear uncertainty
  # catch rate and average net length and soak time same reason
  if (task == "nothing") {
    suitable = !has_gear(interview_data)
    #  | (!has_trip_times(interview_data) & (!has_soak(interview_data) | !has_net_length(interview_data)))
  }

  # to be useful for effort estimation (method = "dbl_exp"), it must have gear, trip times, and be a complete trip
  if (task == "effort") {
    suitable = has_gear(interview_data) & has_trip_times(interview_data) & is_complete_trip(interview_data)
  }

  # does the interview have the sufficient data to calculate a catch rate?
  # this is regardless of the potential "reliability" of the information
  if (task == "catch_rate_info") {
    suitable = has_gear(interview_data) & has_soak(interview_data) & has_net_length(interview_data)
  }

  # does the interview have information that is not out of the ordinary, or otherwise deemed to
  # render its information insuitable for estimation?
  if (task == "catch_rate_info_reliable") {
    suitable = !is_short_incomplete_soak(interview_data) & !is_soak_outlier(interview_data) & is_normal_net(interview_data)
    suitable = suitable & has_gear(interview_data) & has_soak(interview_data) & has_net_length(interview_data)
  }

  # is the soak time usable in calculating the average for any expected trip?
  # the soak time *must* be from a completed trip to be used
  if (task == "avg_soak") {
    suitable = has_soak(interview_data) & is_complete_trip(interview_data)
  }

  # is the net length usable in calculating the average for any expected trip?
  # excludes extremely long or missing net lengths
  if (task == "avg_net_length") {
    suitable = has_net_length(interview_data) & is_normal_net(interview_data)
  }

  # return the output
  return(suitable)
}
