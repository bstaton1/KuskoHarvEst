#' Estimate effort
#' ID Trips that Overlapped Aerial Survey(s)
#'
#' Determines whether each trip was active (based on reported trip times)
#' at the time of each flight, and therefore assumed to have been counted via aerial survey
#'
#' @param trip_times A [`data.frame`][base::data.frame] storing the start and end times of individual trips (one row per interviewed trip).
#'   Must have columns `trip_start` and `trip_end`.
#' @param flight_times A [`data.frame`][base::data.frame] storing the start and end times of each flight (one row per flight).
#'   Must have columns `start_time` and `end_time`
#' @note All records must be supplied in the `datetime` format, ready to be processed by, e.g.,
#'   [lubridate::duration()], [lubridate::interval()], and [lubridate::int_overlaps()]
#' @details 5 seconds are added to the start time and subtracted from the end time before any calculation.
#'   This prevents mistakenly identifying trips that technically overlap (i.e., start/end immediately as the flight was taking off/landing), but almost certainly weren't counted.
#' @return A [`data.frame`][base::data.frame] with columns labeled `"X1"`, `"X2"`, etc., where the number represents the flight number of the day.
#'   Columns store logical (i.e., `TRUE` or `FALSE`) values that indicate whether each trip (individual rows) was active during each flight.

was_counted = function(trip_times, flight_times) {

  # number of flights
  n_flights = nrow(flight_times)

  # convert the flight times to intervals
  fint = lubridate::interval(
    flight_times$start_time + lubridate::duration(5, "seconds"),
    flight_times$end_time - lubridate::duration(5, "seconds")
  )

  # convert the interview times to intervals
  iint = lubridate::interval(trip_times$trip_start, trip_times$trip_end)

  # for each flight, determine if each interview recorded a trip that overlapped it
  was_counted = sapply(1:nrow(flight_times), function(f) lubridate::int_overlaps(iint, fint[f]))

  # assign names to the flights
  colnames(was_counted) = paste0("X", 1:nrow(flight_times))

  # convert to data.frame -- this was the required input format, should be output as well
  was_counted = as.data.frame(was_counted)

  # return the output
  return(was_counted)
}

#' Count Types of Interview/Flight Outcomes
#'
#' Tallies how many trips were counted on each flight and consecutive flights.
#' @param interview_data A [`data.frame`][base::data.frame] storing the data from each interview, constructed via [prepare_interviews()]
#' @param flight_data A [`data.frame`][base::data.frame] storing the times and counts on each flight, constructed via [prepare_flights()]
#' @note Any interviews that (a) are `FALSE` for `suit_effort` or that (b) do not have `gear == "drift"` will be discarded prior to tallies.
#' @return A [`list`][base::list] storing the tallies as elements:
#'   * `$Y`: the number of trips interviewed
#'   * `$X`: the number of trips counted on each flight (e.g., element `"X1"` stores the first flight total)
#'   * `$XnY`: the number of interviewed trips active at various times. This includes elements storing totals for:
#'       * Individual flights (e.g., element `"X1&Y"` is the number of trips that were interviewed and active during flight 1)
#'       * Consecutive flights (e.g., element `"X1&X2&Y"`) is the number of trips that were interviewed and active during both flights 1 and 2)
#'       * Not any flights (element `"!Xany&Y"`)
#'       * All flights (element `"Xall&Y"`)

tally_effort_data = function(interview_data, flight_data) {

  # how many flights
  n_flights = nrow(flight_data)

  # subset only suitable interview records
  interview_data = interview_data[interview_data$suit_effort,]

  # subset only records using drift gear. another method used for set nets
  interview_data = interview_data[interview_data$gear == "drift",]

  # sum the number of trips counted on each flight (i.e., across strata)
  X = rowSums(flight_data[order(flight_data$start_time),stringr::str_which(colnames(flight_data), "_drift")])
  names(X) = paste0("X", 1:nrow(flight_data))

  # assess whether each interviewed trip was active during the time of each flight
  # assume that if so, it was counted by the flight
  x = was_counted(
    trip_times = interview_data[,c("trip_start", "trip_end")],
    flight_times = flight_data[,c("start_time", "end_time")]
  )

  # if more than one flight, tally those counted on consecutive flights
  if (n_flights > 1) {
    consec_names = cbind(colnames(x)[1:(n_flights-1)], colnames(x)[2:n_flights])
    if (n_flights == 2) {
      consec = matrix(x$X1 & x$X2, ncol = 1)
      consec_names = "X1&X2"
    } else {
      consec = apply(consec_names, 2, function(f) x[,f[1]] & x[,f[2]])
      consec_names = apply(consec_names, 2, paste, collapse = "&")
    }
    colnames(consec) = consec_names
    x = cbind(x, consec)

    # add an indicator for interviews not counted on any flight
    not_any = apply(x[,colnames(x) %in% names(X)], 1, function(i) !any(i))
    x = cbind(x, "!Xany" = not_any)

    # add an indicator for interviews counted on all flights
    all = apply(x[, colnames(x) %in% names(X)], 1, function(i) all(i))
    x = cbind(x, "Xall" = all)
  }

  # count the total number of interviews
  Y = nrow(x)

  # count the number of interviews that were counted by each flight
  # or by two consecutive flights
  # the "n" represents "intersection" -- these are joint counts of counted trips (X) and interviewed trips (Y)
  XnY = colSums(x)
  names(XnY) = paste0(names(XnY), "&Y")

  # build the output
  list(
    Y = Y,
    X = X,
    XnY = XnY
  )
}

#' Estimate the Number of Trips that Occurred in a Fishing Day
#'
#' Based on the number of interviewed trips that
#' reported being active during each aerial survey
#'
#' @param effort_data A [`list`][base::list] storing the number of trips reported being active on individual, consecutive, and not any flights.
#'   Constructed via [`tally_effort_data()`].
#' @details Performs the calculations as specified by estimators derived in the appendix of an in-prep manuscript.
#'   Suffice it to say that the estimator combines the interview data and flight data to estimate the fraction
#'   of all trips that were interviewed and counted by aerial survey.
#'   These form the basis for expanding the partial counts to the whole. In the case of multiple flights,
#'   the estimator accounts for trips that would have been counted on more than one flight.
#' @return A numeric vector with named elements for:
#' *  `pi_hat` (or `piF_hat`, where `F` is a flight number) -- the estimated proportion (probability) of all trips counted on each flight
#' *  `psi_hat` -- the estimated proportion (probability) of all trips that were interviewed
#' *  `N_hat` -- the estimated total number of trips that occurred that day

N_estimator = function(effort_data) {

  out = with(effort_data, {

    if (length(X) == 1) {
      ### ONE FLIGHT ###
      # estimate Pr(counted on flight)
      pi1_hat = XnY["X1&Y"]/Y

      # expand those counted by the probability of being counted
      N_hat = X["X1"]/pi1_hat

      # estimate Pr(interviewed)
      psi_hat = Y/N_hat

      # build output
      out = c(pi1_hat, psi_hat, N_hat)
      names(out) = c("pi1_hat", "psi_hat", "N_hat")
      out
    } else {
      ### MORE THAN ONE FLIGHT ###
      # extract the counts of interviews counted on two consecutive flights
      consec_joint = XnY[stringr::str_count(names(XnY), "&") == 2]

      # extract the counts of interviews counted on the second flight of each pair
      second_marginal = XnY[stringr::str_detect(names(XnY), "^X[2-9]&Y$")]

      # calculate the conditional probability
      # of being counted on two consecutive flight counts (F) and (F+1)
      # this represents the fraction of (F+1) that was also counted on (F)
      # 1 - this is the fraction of (F+1) that were new trips
      conditional = consec_joint/second_marginal
      names(conditional) = stringr::str_replace(tolower(names(conditional)), "&", "|")
      names(conditional) = paste0("[", names(conditional), "]")

      # calculate the number of trips counted on all non-first flights that weren't double counted
      new = X[stringr::str_detect(names(X), "^X[2-9]$")] * (1 - conditional)

      # estimate abundance: sum all first-time counts and expand by probability of being counted at all
      N_hat = sum(c(X["X1"], new))/(1 - XnY["!Xany&Y"]/Y)

      # estimate Pr(counted on each flight)
      pi_hat = X/unname(N_hat)

      # build output
      names(pi_hat) = paste0(stringr::str_replace(names(pi_hat), "X", "pi"), "_hat")
      out = c(pi_hat, psi_hat = Y/unname(N_hat), N_hat = unname(N_hat))
    }
  })

  # return the output
  return(out)
}

#'
#' Estimates total effort (completed trips) that occurred in a day of fishing
#'   for a given gear type
#'
#' @inheritParams estimate_harvest
#' @param flight_data Data frame storing flight data; created using [prepare_flights()]
#' @param method Character; which effort estimator should be applied? Only two options are accepted:
#'   * `method = "dbl_exp"` to perform corrections for trips counted on consecutive flights and trips not counted at all - generally used for drift nets
#'   * `method = "max_per_stratum"` to perform a simple calculation of the maximum number of trips counted in a given stratum - generally used for set nets. If this method is used, a further constraint is added to force the total effort estimate to be at least as large as the total number of interviews for that gear type.
#'
#' @export

estimate_effort = function(interview_data, flight_data, gear, method = "dbl_exp") {

  if (method == "dbl_exp") {
    # STEP 1: how many flights were performed, and give them names
    n_flights = nrow(flight_data)
    flight_names = paste0("f", 1:n_flights)

    # STEP 2: count up total effort counted during each flight
    flight_counts = rowSums(flight_data[,stringr::str_detect(colnames(flight_data), gear)])
    names(flight_counts) = flight_names

    # STEP 3: discard interview records that do not have both start and end times
    # the suppress messages is to prevent a message saying that lubridate was loaded.
    # really not sure why this line triggers lubridate to be loaded
    # but this message may confuse users and it is not helpful, so we suppress it
    suppressMessages({
      trips = interview_data[interview_data$suit_effort, ]
    })

    # STEP 4: discard opposite gear and keep only trip times
    trips = trips[trips$gear == gear,c("trip_start", "trip_end")]

    # STEP 5: convert start/end times to intervals
    # the 5 seconds part is to prevent an interview from being counted
    # solely because the start/end dates are the same
    # this 5 second part forces the overlap to be more substantial than just having the same endpoints time
    fint = lubridate::interval(flight_data$start_time + lubridate::duration(5, "seconds"),
                               flight_data$end_time - lubridate::duration(5, "seconds"))
    iint = lubridate::interval(trips$trip_start, trips$trip_end)

    # STEP 6: which trips were available to be counted on each flight
    trips_available = sapply(1:n_flights, function(f) lubridate::int_overlaps(iint, fint[f]))
    colnames(trips_available) = flight_names
    trips = cbind(trips, trips_available)

    # STEP 7: calculate critical summary statistics
    # account for effort that was likely double counted
    if (n_flights > 1) {

      # add indicators for whether each trip was counted on any flight, or on no flights
      trips = cbind(
        trips,
        yes_counted = apply(trips[,flight_names], 1, function(x) any(x)),
        not_counted = apply(trips[,flight_names], 1, function(x) !any(x))
      )

      # create pairs of consecutive flights: only these need correcting
      m = cbind(flight_names[(1:n_flights)[-n_flights]], flight_names[2:n_flights])
      combos = apply(m, 1, function(x) paste(x, collapse = "&"))

      # count how many interviews were available to be counted on each set of consecutive flights
      joint_counts = sapply(combos, function(combo) {
        joint_flight_names = unlist(stringr::str_split(combo, "&"))
        apply(trips[,joint_flight_names], 1, function(counted) counted[1] & counted[2])
      })
      colnames(joint_counts) = combos

      # add this to the rest of the trip info
      trips = cbind(trips, joint_counts)

      # add up the number of outcomes in each type
      trip_counts = colSums(trips[,-c(1,2)])

      # calculate the proportion of interviews available to be counted on flight T2
      # that were also counted on flight T1
      p_T1_given_T2 = trip_counts[combos]/trip_counts[flight_names[2:n_flights]]

      # calculate the number of unique trips counted: removes trips that were double counted
      new_trips = round(flight_counts[flight_names[2:n_flights]] * (1 - p_T1_given_T2))

      # unique trips counted on each flight
      unique_counts = c(flight_counts[1], new_trips)
    } else {
      # if only one flight was conducted, skip most of this mess
      trips = cbind(trips, yes_counted = trips$f1, not_counted = !trips$f1)
      trip_counts = colSums(trips[,-c(1,2)])
      p_T1_given_T2 = NULL
      unique_counts = flight_counts
    }

    # STEP 8: apply expansion for trips occurring outside of flight times
    # total trips counted in flights
    effort_count = sum(unique_counts)

    # trips per interview
    effort_per_interview = unname(effort_count/trip_counts["yes_counted"])

    # effort unaccounted for by flights
    effort_not_count = round(unname(effort_per_interview * trip_counts["not_counted"]))

    # total effort estimate
    effort_est_total = effort_count + effort_not_count

    # STEP 9: stratify the total effort estiamte
    effort_est_stratum = stratify_effort(flight_data, gear, effort_est_total)

    # rewrite the total effort to make sure it is equal to the sum of the stratum-specific version
    # (sometimes they are off by 1 unit, due to rounding)
    effort_est_total = sum(effort_est_stratum)

    # build a list with the output
    output = list(
      method = method,
      gear = gear,
      trips = trips,
      trip_counts = trip_counts,
      p_T1_given_T2 = p_T1_given_T2,
      effort_count = effort_count,
      effort_not_count = effort_not_count,
      effort_per_interview = effort_per_interview,
      effort_est_total = effort_est_total,
      effort_est_stratum = effort_est_stratum
    )
  }

  if (method == "max_per_stratum") {
    # STEP 1: count up total effort counted during each flight
    flight_counts = flight_data[,stringr::str_detect(colnames(flight_data), gear)]

    # STEP 2: find the sum of the maximum number of effort counted on any flight in each stratum
    effort_est_total = sum(apply(flight_counts, 2, function(x) max(x)))

    # STEP 3: if the effort estimate based on flights is less than the number of interviews, use the number of interviews
    if (effort_est_total < nrow(interview_data[interview_data$gear == gear,])) {
      effort_est_total = nrow(interview_data[interview_data$gear == gear,])
    }

    # STEP 4: stratify the total effort estimate
    # any rounding involved here will round up always
    effort_est_stratum = stratify_effort(flight_data, gear, effort_est_total)

    # rewrite the total effort to make sure it is equal to the sum of the stratum-specific version
    # (sometimes they are off by 1 unit, due to rounding)
    effort_est_total = sum(effort_est_stratum)

    # build a list with the output
    output = list(
      method = method,
      gear = gear,
      effort_est_total = effort_est_total,
      effort_est_stratum = effort_est_stratum
    )
  }

  # return output
  return(output)
}
