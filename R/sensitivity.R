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

