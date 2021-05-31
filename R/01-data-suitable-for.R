#' Determine which interview records are suitable for specific tasks
#'
#' A wrapper around a variety of data checking functions.
#'
#' @inheritParams estimate_harvest
#' @param task Character; the type of task to determine whether each interview record is suitable for. Five options are accepted:
#'   * `task = "effort"`
#'   * `task = "catch_rate_info"`
#'   * `task = "catch_rate_info_reliable"`
#'   * `task = "avg_soak"`
#'   * `task = "avg_net_length"`
#' @details The checks for each task include:
#'   * `"effort"`: Interview must have gear type, trip start and end times, and be from a completed trip
#'   * `"catch_rate_info"`: Checks if interview has all necessary info for calculating catch rate: gear, soak time, and net length
#'   * `"catch_rate_reliable"`: Checks if catch rate data is reliable; must be: a not very short incomplete trip, not a soak outlier, and not a abnormally long net
#'   * `"avg_soak"`: Checks if soak time is available, that it is a completed trip, and that it is not a soak outlier
#'   * `"avg_net_length"`: Checks if the net length is of a normal length
#'

suitable_for = function(interview_data, task) {

  accepted_tasks = c("effort", "catch_rate_info", "catch_rate_info_reliable", "avg_soak", "avg_net_length")
  if (!(task %in% accepted_tasks)) stop ("task must be one of: ", paste(paste0("'", accepted_tasks, "'"), collapse = ", "))

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
    # suitable = suitable & has_gear(interview_data) & has_soak(interview_data) & has_net_length(interview_data)
  }

  # is the soak time usable in calculating the average for any expected trip?
  # the soak time *must* be from a completed trip to be used
  if (task == "avg_soak") {
    suitable = has_soak(interview_data) & is_complete_trip(interview_data) & !is_soak_outlier(interview_data)
  }

  # is the net length usable in calculating the average for any expected trip?
  # excludes extremely long or missing net lengths
  if (task == "avg_net_length") {
    suitable = is_normal_net(interview_data)
  }

  # return the output
  return(suitable)
}
