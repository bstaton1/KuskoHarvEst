#' Estimate expected catch for the average trip
#'
#' @export

estimate_catch_per_trip = function(interview_data, gear, include_whitefishes = F, central_fn = mean) {

  # set the species
  # decide on species to estimate catch_per_trip for
  keep_spp = c("chinook", "chum", "sockeye")
  if (include_whitefishes) keep_spp = c(keep_spp, "whitefish", "sheefish")

  # subset only the data for this gear
  interview_data = interview_data[interview_data$gear == gear,]

  # extract the soak hours
  soak_hrs = as.numeric(interview_data$soak_duration, "hours")

  # extract the net length
  net_length = as.numeric(interview_data$net_length)

  # extract the catches
  catches = interview_data[,keep_spp]

  # determine which are suitable for calculating reliable catch rates
  suitable_catch_rate = interview_data[,"suit_cr_reliable"]

  # determine which are suitable for calculating average soak time
  suitable_avg_soak = interview_data[,"suit_avg_soak"]

  # determine which are suitable for calculating average net length
  suitable_avg_net = interview_data[,"suit_avg_net"]

  # calculate the catch rate for all records (doesn't matter if suitable or not yet)
  catch_rates = apply(catches, 2, function(catch) catch/(net_length * soak_hrs))

  # calculate the average catch per trip for the requested gear and only the suitable records
  out = apply(catch_rates[suitable_catch_rate,], 2, central_fn) * central_fn(net_length[suitable_avg_net]) * central_fn(soak_hrs[suitable_avg_soak])

  # return the output
  return(out)
}
