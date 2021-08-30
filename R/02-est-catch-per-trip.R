#' Estimate expected catch for the average trip
#'
#' Calculates the expected salmon catch by species for the average trip
#'   after filtering out only usable records
#'
#' @inheritParams estimate_harvest
#' @param central_fn Function; used to calculate central tendency
#'

estimate_catch_per_trip = function(interview_data, gear, randomize = FALSE, central_fn = getOption("central_fn")) {

  # set the species to keep
  keep_spp = c("chinook", "chum", "sockeye")

  # subset only the data for this gear
  interview_data = interview_data[interview_data$gear == gear,]

  # if randomizing the data, do so
  if (randomize) {
    interview_data = randomize_data(interview_data)
  }

  # extract the soak hours
  soak_hrs = as.numeric(interview_data$soak_duration, "hours")

  # extract the net length
  net_length = as.numeric(interview_data$net_length)

  # extract the catches
  catches = interview_data[,keep_spp]

  # determine which are suitable for calculating reliable catch rates
  suitable_catch_rate = interview_data[,"suit_cr_reliable"] & interview_data[,"suit_cr_info"]

  # determine which are suitable for calculating average soak time
  suitable_avg_soak = interview_data[,"suit_avg_soak"]

  # determine which are suitable for calculating average net length
  suitable_avg_net = interview_data[,"suit_avg_net"]

  # return all NAs if any of the data types are all unsuitable
  if (all(!suitable_catch_rate) | all(!suitable_avg_soak) | all(!suitable_avg_net)) {
    out = rep(NA, length(keep_spp)); names(out) = keep_spp
  } else {
    # calculate the catch rate for all records (doesn't matter if suitable or not yet)
    catch_rates = apply(catches, 2, function(catch) catch/(net_length * soak_hrs))

    # if there is only one catch rate record, make a 1 row matrix so the apply calls below work
    if (is.vector(catch_rates)) {
      catch_rates = matrix(catch_rates, nrow = 1, ncol = length(catch_rates))
      colnames(catch_rates) = colnames(catches)
    }

    # calculate the average catch per trip for the requested gear and only the suitable records
    if (sum(suitable_catch_rate) == 1) {
      out = sapply(catch_rates[suitable_catch_rate,], central_fn, na.rm = TRUE) * central_fn(net_length[suitable_avg_net], na.rm = TRUE) * central_fn(soak_hrs[suitable_avg_soak], na.rm = TRUE)
    } else {
      out = apply(catch_rates[suitable_catch_rate,], 2, central_fn, na.rm = TRUE) * central_fn(net_length[suitable_avg_net], na.rm = TRUE) * central_fn(soak_hrs[suitable_avg_soak], na.rm = TRUE)
    }
  }

  # return the output
  return(out)
}
