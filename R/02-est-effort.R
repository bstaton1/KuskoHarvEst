#' Estimate effort
#'
#' @export

estimate_effort = function(interview_data, flight_data, gear = "drift") {

  # STEP 1: how many flights were performed, and give them names
  n_flights = nrow(flight_data)
  flight_names = paste0("f", 1:n_flights)

  # STEP 2: count up total effort counted during each flight
  flight_counts = rowSums(flight_data[,stringr::str_detect(colnames(flight_data), gear)])
  names(flight_counts) = flight_names

  # STEP 3: discard interview records that do not have both start and end times
  trips = interview_data[!is.na(interview_data$trip_start) & !is.na(interview_data$trip_end), ]

  # STEP 4: discard opposite gear and keep only trip times
  trips = trips[interview_data$gear == gear,c("trip_start", "trip_end")]

  # STEP 5: convert start/end times to intervals
  fint = lubridate::interval(flight_data$start_time, flight_data$end_time)
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
    trips = cbind(trips, trips, !trips)
    colnames(trips) = c("f1", "yes_counted", "not_counted")
    trip_counts = colSums(trips_counted)
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
  effort_est = effort_count + effort_not_count

  # build a list with the output
  output = list(
    trips = trips,
    trip_counts = trip_counts,
    p_T1_given_T2 = p_T1_given_T2,
    effort_per_interview = effort_per_interview,
    effort_est = effort_est
  )

  # return output
  return(output)
}
