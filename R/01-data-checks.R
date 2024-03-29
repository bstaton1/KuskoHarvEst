
##### FUNCTIONS TO CHECK WHETHER EACH CRITICAL DATA SOURCE EXISTS #####

#' Check if the gear was recorded
#'
#' @inheritParams estimate_harvest

has_gear = function(interview_data) {
  !is.na(interview_data$gear)
}

#' Check if the trip times were recorded
#'
#' @inheritParams estimate_harvest

has_trip_times = function(interview_data) {
  !is.na(interview_data$trip_start) & !is.na(interview_data$trip_end)
}

#' Check if the net length was recorded
#'
#' @inheritParams estimate_harvest

has_net_length = function(interview_data) {
  !is.na(interview_data$net_length)
}

#' Check if soak duration was recorded
#'
#' @inheritParams estimate_harvest

has_soak = function(interview_data) {
  !is.na(interview_data$soak_duration)
}

##### FUNCTIONS TO CHECK FOR TIME ANOMALIES #####

#' Check for impossible trip times
#'
#' @inheritParams estimate_harvest
#' @details If the trip end time is before the trip start time, this is impossible and an error.
#'   The user should fix this entry in the data set before proceeding.

is_possible_trip = function(interview_data) {
  ifelse(!has_trip_times(interview_data), TRUE, ifelse(interview_data[,"trip_start"] < interview_data[,"trip_end"], TRUE, FALSE))
}

#' Check for impossible soak times
#'
#' @inheritParams estimate_harvest
#' @details If the soak duration is longer than the trip duration, this is impossible and an error.

is_possible_soak = function(interview_data) {
  ifelse(!has_trip_times(interview_data) | !has_soak(interview_data), TRUE,
         ifelse(interview_data[,"soak_duration"] <= interview_data[,"trip_duration"], TRUE, FALSE))
}

##### MISC CHECKS FOR DATA ACCURACY #####

#' Check if interview is for a completed trip
#'
#' @inheritParams estimate_harvest
#' @details Data need special care if from an incomplete trip.
#'   E.g., soak time should not be used in calculation of the average across trips,
#'   and if the soak time is excessively short at the time of the interview relative to completed
#'   trips, it shouldn't be used to inform the average catch rate.

is_complete_trip = function(interview_data) {
  interview_data$source != "LE"
}

#' Check if the net characteristics are as expected
#'
#' @inheritParams estimate_harvest
#' @param net_length_cut Numeric; maximum net length (in feet) allowed, records with net length longer
#'   than this will have their net length deemed unsuitable for any purpose
#' @details Nets that are extremely long are probably mis-recorded and should not be included.

is_normal_net = function(interview_data, net_length_cut = KuskoHarvEst_opts("net_length_cut")) {
  ifelse(!has_net_length(interview_data), TRUE, ifelse(interview_data[,"net_length"] <= net_length_cut, TRUE, FALSE))
}

#' Check soak time for outliers
#'
#' @inheritParams estimate_harvest
#' @param soak_sd_cut Numeric; maximum number of standard deviations the soak time can be from the mean
#'   without being deemed an outlier
#' @details Consider soak time an outlier if its value is larger than
#'   the mean plus XSDs of the remaining values for that gear type. The user
#'   chooses the value of X with the argument `soak_sd_cut`. This function applies
#'   to completed trips only -- records that are for incomplete trips or that
#'   do not have soak time recorded will have `FALSE` returned.

is_soak_outlier = function(interview_data, soak_sd_cut = KuskoHarvEst_opts("soak_sd_cut")) {
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

      # if there no other soak times for this gear, can't be an outlier
      # otherwise, base choice on SD of remaining soak times
      if (length(soak_hrs_without) == 0) {
        out[i] = FALSE
      } else {
        # calculate the mean and SD of remaining soak times
        sd_without = sd(soak_hrs_without, na.rm = T)
        mn_without = mean(soak_hrs_without, na.rm = T)

        # perform the test
        out[i] = soak_hrs[i] > (mn_without + (soak_sd_cut * sd_without))
      }
    } else {
      next()
    }
  }

  return(out)
}

#' Check incomplete trips for very short soak times
#'
#' @inheritParams estimate_harvest
#' @details For incomplete trips, if the interview was conducted early into an incomplete
#'   trip, its catch rate information is not likely representative of that from completed trips.
#'   This function looks for incomplete interviews that have soak time shorter than the shortest recorded
#'   soak time among completed trips for that gear type. Records with a `TRUE` value returned should
#'   not be used in harvest estimation

is_short_incomplete_soak = function(interview_data) {

  # extract the soak hours
  soak_hrs = as.numeric(interview_data$soak_duration, "hours")

  # determine which trips were completed trip interviews
  is_complete = is_complete_trip(interview_data)

  # container object
  out = logical(nrow(interview_data))

  # determine the shortest soak time in complete trips of each gear type
  shortest_complete_trip = with(interview_data[is_complete,], tapply(as.numeric(soak_duration, "hours"), gear, function(soak) {
    if (sum(!is.na(soak)) > 0) {
      out = min(soak, na.rm = T)
    } else {
      out = NA
    }
    out
  }))

  # for each interview, if it is incomplete, determine whether it is shorter than the shortest completed trip of that gear type
  for (i in 1:nrow(interview_data)) {
    if (!is_complete[i] & has_soak(interview_data)[i]) {
      out[i] = soak_hrs[i] < shortest_complete_trip[interview_data[i,"gear"]]
    } else {
      next()
    }
  }

  return(out)
}

#' Determine if an interview is an outlier in terms of catch per trip
#'
#' @inheritParams estimate_harvest
#' @param catch_per_trip_cut Numeric; the maximum proportional change in average catch per trip allowed
#'   when an interview is discarded for that interview to be considered reliable
#' @details This function recalculates the average catch per trip (non-stratified)
#'   by leaving out each interview sequentially. It then determines the proportional change
#'   from the original estimate with all data included. If the absolute proportional change when
#'   leaving out an interview is greater than `catch_per_trip_cut` the catch rate data from that
#'   interview will be deemed unreliable.
#'   Calculations done only for drift net interviews -- set net interviews are skipped.

is_catch_per_trip_outlier = function(interview_data, catch_per_trip_cut = KuskoHarvEst_opts("catch_per_trip_cut")) {

  # container (cpt = average catch of all salmon per trip)
  loo_cpt = rep(NA, nrow(interview_data))

  # loop through interviews
  for (i in 1:nrow(interview_data)) {
    # if the interview is from a set net trip, don't perform the check
    if (interview_data$gear[i] == "set") {
      next()
    } else {
      # estimate average catch per trip without this interview
      loo_cpt[i] = sum(estimate_catch_per_trip(interview_data[-i,], "drift"))
    }
  }

  # calculate average cpt with all interviews
  original_cpt = sum(estimate_catch_per_trip(interview_data, "drift"))

  # calculate percent change
  loo_pdiff = (loo_cpt - original_cpt)/original_cpt

  # create a flag for if the interview is an outlier
  is_outlier = ifelse(abs(loo_pdiff) > catch_per_trip_cut, TRUE, FALSE)
  is_outlier[is.na(is_outlier)] = FALSE

  # return the flag
  return(is_outlier)
}

#' Determine the unique start dates of all interviews
#'
#' @inheritParams estimate_harvest

unique_start_dates = function(interview_data) {
  start_dates = lubridate::date(interview_data$trip_start)
  unique(start_dates[!is.na(start_dates)])
}
