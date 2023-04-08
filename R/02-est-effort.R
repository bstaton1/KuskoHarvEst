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

#' Estimate Effort
#'
#' Estimates total effort (completed trips) that occurred in a day of fishing
#'   for a given gear type
#'
#' @inheritParams tally_effort_data
#' @param method Character; which effort estimator should be applied? Only two options are accepted:
#'   * `method = "dbl_exp"` to perform corrections for trips counted on consecutive flights and trips not counted at all - generally used for drift nets
#'   * `method = "max_per_stratum"` to perform a simple calculation of the maximum number of trips counted in a given stratum - generally used for set nets. If this method is used, a further constraint is added to force the total effort estimate to be at least as large as the total number of interviews for that gear type.
#' @details If `method = "dbl_exp"`, this function calls two other critical functions on the data: [tally_effort_data()] and [N_estimator()].
#'   Please see these functions for more information.
#' @export

# FIXME: needs @return documentation
estimate_effort = function(interview_data, flight_data, gear, method = "dbl_exp") {

  # if using method that corrects for double counts and expands for uncounted trips
  if (method == "dbl_exp") {

    # simplify data objects to be passed to analysis
    trips = subset(interview_data, gear == gear & suit_effort)[,c("trip_start", "trip_end")]
    time_cols = colnames(flight_data)[stringr::str_detect(colnames(flight_data), "_time")]
    count_cols = colnames(flight_data)[stringr::str_which(colnames(flight_data), gear)]
    flight_data = flight_data[,c(time_cols, count_cols)]
    n_flights = nrow(flight_data)

    # tally effort data and estimate effort
    tallies = tally_effort_data(interview_data, flight_data)
    est_out = N_estimator(tallies)

    # nearly all of the rest of this is to return output in the legacy format
    # so the rest of the package need not change at this time
    # "legacy" = prior to formal derivation w/symbols, etc. Staton & Coggins (2016) & Staton (2018) didn't have the best explanations
    # FIXME: this could be cleaned up to:
    #   * use X/Y/pi/psi, naming throughout
    #   * downstream uses of the output of this function would need updates
    trips = cbind(trips, was_counted(trips, flight_data[,c("start_time", "end_time")]))
    flight_names = colnames(trips)[stringr::str_which(colnames(trips), "^X")]

    # handle output if more than one flight
    if (n_flights > 1) {
      trips$yes_counted = apply(trips[,flight_names], 1, any)
      trips$not_counted = !trips$yes_counted

      # names of joint combos
      joint_names = names(tallies$XnY)[stringr::str_count(names(tallies$XnY), "\\&") == 2]
      joint_names = stringr::str_remove(joint_names, "\\&Y$")

      # get interview-level joint outcomes
      joints = sapply(joint_names, function(x) {
        f1 = stringr::str_extract(x, "^X[:digit:]")
        f2 = stringr::str_extract(x, "X[:digit:]$")
        out = matrix(trips[,f1] & trips[,f2], ncol = 1)
        colnames(out) = x
        out
      })
      trips = cbind(trips, joints)

      # get conditional probability (f1|f2) for all joint combos
      p_T1_given_T2 = sapply(joint_names, function(x) {
        f2 = stringr::str_extract(x, "X[:digit:]$")
        sum(trips[,x])/sum(trips[,f2])
      })

      # get the number of trips counted for the first time on each flight
      unique_counts = c(tallies$X[1], round(tallies$X[2:n_flights] * (1 - p_T1_given_T2)))

      # update flight count names to be f, not X
      names(p_T1_given_T2) = stringr::str_replace_all(names(p_T1_given_T2), "X", "f")
    } else {
      trips$yes_counted = trips$X1
      trips$not_counted = !trips$yes_counted
      p_T1_given_T2 = NULL
      unique_counts = tallies$X["X1"]
    }

    # summarize/format output
    effort_est_total = round(unname(est_out["N_hat"]))
    effort_count = sum(unique_counts)
    effort_not_count = effort_est_total - effort_count
    effort_per_interview = unname(1/est_out["psi_hat"])

    # update flight count names to be f, not X
    colnames(trips) = stringr::str_replace_all(colnames(trips), "X", "f")

    # build a list with the output
    output = list(
      method = method,
      gear = gear,
      trips = trips,
      trip_counts = colSums(trips[,-which(colnames(trips) %in% c("trip_start", "trip_end"))]),
      p_T1_given_T2 = p_T1_given_T2,
      effort_count = effort_count,
      effort_not_count = effort_not_count,
      effort_per_interview = effort_per_interview,
      effort_est_total = effort_est_total,
      effort_est_stratum = stratify_effort(flight_data, gear, effort_est_total)
    )
  }

  # if using method that selects the max count found in each stratum
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
