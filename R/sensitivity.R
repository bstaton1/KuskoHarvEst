#' Make combinations of flights to leave out for effort sensitivity analysis
#'

make_flight_combos = function(flight_data) {
  # count the number of flights in full data set
  n_flights = nrow(flight_data)

  # build and execute expand.grid call to make all combinations of leaving them out
  call = paste0("expand.grid(", paste(paste0("F", 1:n_flights, " = c(TRUE, FALSE)"), collapse = ", "), ")")
  combos = eval(parse(text = call))

  # discard the case that retains no flights
  combos = combos[-which(rowSums(combos) == 0),]

  # build and execute call to order the combos
  call = paste0("combos[", paste0("order(", paste(paste0("combos[,'", names(combos), "']"), collapse = ", "), ", decreasing = TRUE)"), ",]")
  combos = eval(parse(text = call))
  combos = combos[order(rowSums(combos), decreasing = TRUE),]
  rownames(combos) = NULL

  # return the output
  return(combos)
}

#' Make combinations of interview data sources to leave out for effort sensitivity analysis
#'

make_interview_combos = function(interview_data) {

  # extract the interview data sources found in the data that have complete trip time information
  sources = unique(interview_data$source[has_trip_times(interview_data)])

  # build and execute expand.grid call to make all combinations of leaving them out
  call = paste0("expand.grid(", paste(paste0("x", 1:length(sources), " = c(TRUE, FALSE)"), collapse = ", "), ")")
  combos = eval(parse(text = call))
  names(combos) = sources

  # discard the case that would have no interview data
  combos = combos[-which(rowSums(combos) == 0),]

  # build and execute call to sort the combinations
  call = paste0("combos[", paste0("order(", paste(paste0("combos[,'", sources, "']"), collapse = ", "), ", decreasing = TRUE)"), ",]")
  combos = eval(parse(text = call))
  combos = combos[order(rowSums(combos), decreasing = TRUE),]
  rownames(combos) = NULL

  # return the output
  return(combos)
}

