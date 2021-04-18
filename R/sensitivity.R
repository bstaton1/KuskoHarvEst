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

#' Make combinations of interview data sources to leave out for harvest sensitivity analysis
#'

make_harvest_combos = function(interview_data) {

  # extract all interview data source names
  sources = unique(interview_data$source)

  # build and execute call to build all data exclusion scenarios
  call = paste0("expand.grid(", paste(paste0("x", 1:length(sources), " = c(TRUE, FALSE)"), collapse = ", "), ")")
  combos = eval(parse(text = call))
  names(combos) = sources

  # exclude the case that would retain no data
  combos = combos[-which(rowSums(combos) == 0),]

  # build and execute a call to order the scenarios
  call = paste0("combos[", paste0("order(", paste(paste0("combos[,'", sources, "']"), collapse = ", "), ", decreasing = TRUE)"), ",]")
  combos = eval(parse(text = call))
  combos = combos[order(rowSums(combos), decreasing = TRUE),]
  rownames(combos) = NULL

  # keep only cases that leave out 0 or 1 data sources at a time
  combos = combos[rowSums(combos) >= (length(sources) - 1),]

  # return the output
  return(combos)
}

#' Build a table to report results of effort sensitivity analyses
#'
#' @importFrom magrittr %>%

effort_sensitivity_table = function(effort_scenarios, flight_data, combos) {

  # extract primary effort estimation information from each effort estimate
  combo_total_ests = unlist(lapply(effort_scenarios, function(x) x$effort_est_total))
  combo_names = sapply(1:nrow(combos), function(i) paste(names(combos)[which(unlist(combos[i,]))], collapse = ", "))
  combo_p_change = percentize((combo_total_ests - combo_total_ests[1])/combo_total_ests[1], escape = TRUE)
  combo_effort_per_interview = unlist(lapply(effort_scenarios, function(x) x$effort_per_interview))
  combo_effort_not_counted = unlist(lapply(effort_scenarios, function(x) x$effort_not_count))

  # create the names of the conditional probabilities
  n_flights = nrow(flight_data)
  cond_names = paste0("Pr(F", 1:(n_flights - 1), "|F", 2:n_flights, ")")

  # format the conditional probabilities for each scenario
  if (n_flights > 1) {
    combo_conditionals = unlist_dfs(lapply(effort_scenarios, function(x) {
      x = unname(x$p_T1_given_T2)
      if (length(x) < length(cond_names)) {
        x = percentize(c(x, rep(NA, length(cond_names) - length(x))), escape = TRUE)
      } else {
        x = percentize(x, escape = TRUE)
      }
      x[x == "NA\\%"] = "--"
      x = as.data.frame(as.list(x))
      names(x) = cond_names
      x
    }))
  } else {
    combo_conditionals = NULL
  }

  # build the table
  combo_table = cbind(
    "Scenario" = combo_names,
    "Effort Estimate" = combo_total_ests,
    "\\% Change" = combo_p_change,
    combo_conditionals,
    "Trips Per Interview" = round(combo_effort_per_interview, 2),
    "Trips Not Observed" = combo_effort_not_counted
  )

  # build the kable
  knitr::kable(combo_table, format = "latex", booktabs = TRUE, longtable = FALSE, linesep = "", escape = FALSE,
               align = paste(c("l", paste(rep("c", ncol(combo_table) - 1), collapse = "")), collapse = "")) %>%
    kableExtra::kable_styling(full_width = FALSE, latex_options = c("scale_down", "HOLD_position")) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::column_spec(1, bold = TRUE)
}

