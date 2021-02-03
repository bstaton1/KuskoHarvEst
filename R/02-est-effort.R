
# estimate_effort = function(idat, fdat) {
#
# }

input_files = c(
  "inst/example-reports/drift-set/BBH_6_12_18.csv",
  # "inst/example-reports/drift-set/CBM_6_12_18.csv",
  "inst/example-reports/drift-set/FC_6_12_18.csv",
  "inst/example-reports/drift-set/LE_6_12_18.csv",
  # "inst/example-reports/drift-set/ADFG_6_12_18.csv",
  "inst/example-reports/drift-set/Flight_counts2_6_12_18.csv"
)

out = lapply(input_files[1:3], function(f) prepare_interviews(f, include_whitefishes = F, include_village = F, include_goals = F))
idat = rbind(out[[1]], out[[2]], out[[3]])#, out[[4]], out[[5]])
fdat = prepare_flights(input_files[4])[1,]

##### FUNCTION START

# input_args: idat (interview data), fdat (flight data)
# this is the body of a function that is still in development

# STEP X: how many flights were performed, and give them names
n_flights = nrow(fdat)
flight_names = paste0("f", 1:n_flights)

# STEP X: count up total effort counted during each flight
set_counts = rowSums(fdat[,stringr::str_detect(colnames(fdat), "_set")])
drift_counts = rowSums(fdat[,stringr::str_detect(colnames(fdat), "_drift")])
names(set_counts) = names(drift_counts) = flight_names

# STEP X: discard interview records that do not have both start and end times
idat = idat[!is.na(idat$trip_start) & !is.na(idat$trip_end), ]

# STEP X: convert start/end times to intervals
fint = lubridate::interval(fdat$start_time, fdat$end_time)
iint = lubridate::interval(idat$trip_start, idat$trip_end)

# STEP X: which trips were available to be counted on each flight
# this one line replaces all of the old "trips.available()" function
trips_counted = sapply(1:n_flights, function(f) lubridate::int_overlaps(iint, fint[f]))
colnames(trips_counted) = flight_names

# STEP X: add which trips were or were not counted on any flight
trips_counted = cbind(trips_counted,
                      yes_counted = apply(trips_counted[,flight_names], 1, function(x) any(x)),
                      not_counted = apply(trips_counted[,flight_names], 1, function(x) !any(x)))

# if double-counting needs to be accounted for
if (n_flights > 1) {
  # figure out which combos of flights need to have have double counting accounted for
  # only consecutive flights need this
  m = cbind(flight_names[(1:n_flights)[-n_flights]], flight_names[2:n_flights])
  combos = apply(m, 1, function(x) paste(x, collapse = "&"))

  # count how many interviews were available to be counted on each set of consecutive flights
  joint_counts = sapply(combos, function(combo) {
    joint_flight_names = unlist(stringr::str_split(combo, "&"))
    apply(trips_counted[,joint_flight_names], 1, function(counted) counted[1] & counted[2])
  })
  colnames(joint_counts) = combos

  # add this to the rest of the trip counts
  trips_counted = cbind(trips_counted, joint_counts)

  # add up the number of outcomes in each type
  trip_counts = colSums(trips_counted)

  # calculate the proportion of interviews available to be counted on flight T2
  # that were also counted on flight T1
  p_T1_given_T2 = trip_counts[combos]/trip_counts[flight_names[2:n_flights]]

  # calculate the number of unique trips counted: removes trips that were double counted
  new_trips = round(drift_counts[flight_names[2:n_flights]] * (1 - prob_countT2_given_countT1))

  # unique trips counted on each flight
  unique_counts = c(drift_counts[1], new_trips)
} else {
  # if only one flight was conducted, skip all of this mess
  trip_counts = colSums(trips_counted)
  unique_counts = drift_counts
}

# total trips counted in flights
sum(unique_counts)

# next up: apply the "Lincoln-Peterson" estimator

# also to-do: compare with the old estimate.trips() function
# answer at this point should be identical, and if not we
# should be able to identify the causes of any differences

##### FUNCTION END
