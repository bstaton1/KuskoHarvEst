#' Estimate harvest
#'
#' @export


# interview_data = idat
# include_whitefishes = F
# central_fn = mean
# gear = "drift"

estimate_harvest = function(interview_data, effort_est, gear, include_whitefishes = FALSE, central_fn = mean) {
  # keep only records for this gear type
  interview_data = interview_data[interview_data$gear == gear,]

  # decide on species to estimate harvest for
  keep_spp = c("chinook", "chum", "sockeye")
  if (include_whitefishes) keep_spp = c(keep_spp, "whitefish", "sheefish")

  # calculate the trip level effort: feet of net * soaking hours
  trip_effort = with(interview_data, net_length * as.numeric(soak_duration, "hours"))

  # calculate the trip level catch rate by species: fish/foothour
  catch_rate = apply(interview_data[,keep_spp], 2, function(spp_catch) spp_catch/trip_effort)

  # estimate harvest: average_trip_effort * average_catch_rate * total_trips
  harvest_est = apply(catch_rate, 2, function(spp) {
    round(
      central_fn(trip_effort, na.rm = TRUE) * central_fn(spp, na.rm = TRUE) * effort_est
      )
  })

  # return the output
  return(harvest_est)
}
