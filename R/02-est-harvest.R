#' Estimate harvest
#'
#' @export

estimate_harvest = function(interview_data, effort_est, gear, include_whitefishes = FALSE, central_fn = mean) {

  # estimate catch per trip
  catch_per_trip = estimate_catch_per_trip(interview_data, gear = gear, include_whitefishes = include_whitefishes, central_fn = central_fn)

  # estimate harvest: average catch per trip * number of trips
  harvest_est = round(catch_per_trip * effort_est, 0)

  # return the output
  return(harvest_est)
}
