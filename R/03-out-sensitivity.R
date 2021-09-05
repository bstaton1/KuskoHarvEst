#' Create combinations of flights to use for effort sensitivity analysis
#'
#' @inheritParams estimate_effort
#' @details Based on the number of flights conducted, constructs a set of combinations
#'   of data scenarios where some flights are discarded.

create_flight_combos = function(flight_data) {
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

#' Create combinations of interview data sources to use for effort sensitivity analysis
#'
#' @inheritParams estimate_harvest
#' @details Based on the data sources that conducted interviews, constructs a set of combinations
#'   of data scenarios where some sources are discarded. All combinations of keeping/leaving out each
#'   data source are included, unlike [create_harvest_combos()].
#'

create_interview_combos = function(interview_data) {

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

#' Create combinations of interview data sources to use for harvest sensitivity analysis
#'
#' @inheritParams estimate_harvest
#' @details Based on the data sources that conducted interviews, constructs a set of combinations
#'   of data scenarios where some sources are discarded. Each scenario (beyond the original set)
#'   includes just one data source, unlike [create_interview_combos()].
#'

create_harvest_combos = function(interview_data) {

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

#' Make a table to report results of effort sensitivity analyses
#'
#' @inheritParams estimate_effort
#' @param effort_scenarios List; contains the output of executing [estimate_effort()]
#'   repeatedly, once per combination of flights or interview data sources. Each list element is the output from a different
#'   data scenario.
#' @param combos The output of [make_flight_combos()] or [make_interview_combos()]
#'
#' @export
#' @importFrom magrittr %>%
#'

make_effort_sensitivity_table = function(effort_scenarios, flight_data, combos) {

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
    combo_conditionals = lapply(effort_scenarios, function(x) {
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
    })
    combo_conditionals = do.call(rbind, combo_conditionals)
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
    kableExtra::column_spec(1, bold = TRUE) %>%
    add_vspace
}

#' Make a table to report results from harvest sensitivity analyses
#'
#' @param harvest_scenarios List storing the bootstrapped harvest output from each data scenario
#' @param combos The output of [make_harvest_combos()]
#'
#' @export
#' @importFrom magrittr %>%
#'

make_harvest_sensitivity_table = function(harvest_scenarios, combos) {

  # create names for each combo
  combo_names = sapply(1:nrow(combos), function(i) {
    discard = which(!unlist(combos[i,]))
    if (length(discard) == 0) {
      "All Data"
    } else {
      paste0("No ", names(combos)[discard])
    }
  })

  # create a nice column showing the estimate by species group
  pretty_chinook_ests = unlist(lapply(harvest_scenarios, function(x) {
    KuskoHarvEst:::tinyCI(report(spp = "chinook", gear = "total", stratum = "total", boot_out_use = x))
  }))

  pretty_chum_ests = unlist(lapply(harvest_scenarios, function(x) {
    KuskoHarvEst:::tinyCI(report(spp = "chum", gear = "total", stratum = "total", boot_out_use = x))
  }))

  pretty_sockeye_ests = unlist(lapply(harvest_scenarios, function(x) {
    KuskoHarvEst:::tinyCI(report(spp = "sockeye", gear = "total", stratum = "total", boot_out_use = x))
  }))

  pretty_total_ests = unlist(lapply(harvest_scenarios, function(x) {
    KuskoHarvEst:::tinyCI(report(spp = "total", gear = "total", stratum = "total", boot_out_use = x))
  }))

  # extract just the means: for calculating %change
  mean_chinook_ests = unlist(lapply(harvest_scenarios, function(x) {
    report(spp = "chinook", gear = "total", stratum = "total", boot_out_use = x, CI = FALSE, return_numeric = TRUE)
  }))

  mean_chum_ests = unlist(lapply(harvest_scenarios, function(x) {
    report(spp = "chum", gear = "total", stratum = "total", boot_out_use = x, CI = FALSE, return_numeric = TRUE)
  }))

  mean_sockeye_ests = unlist(lapply(harvest_scenarios, function(x) {
    report(spp = "sockeye", gear = "total", stratum = "total", boot_out_use = x, CI = FALSE, return_numeric = TRUE)
  }))

  mean_total_ests = unlist(lapply(harvest_scenarios, function(x) {
    report(spp = "total", gear = "total", stratum = "total", boot_out_use = x, CI = FALSE, return_numeric = TRUE)
  }))

  # calculate the % change in mean estimate
  chinook_p_diff = percentize((mean_chinook_ests - mean_chinook_ests[1])/mean_chinook_ests[1], escape = TRUE)
  chum_p_diff = percentize((mean_chum_ests - mean_chum_ests[1])/mean_chum_ests[1], escape = TRUE)
  sockeye_p_diff = percentize((mean_sockeye_ests - mean_sockeye_ests[1])/mean_sockeye_ests[1], escape = TRUE)
  total_p_diff = percentize((mean_total_ests - mean_total_ests[1])/mean_total_ests[1], escape = TRUE)

  # calculate the CV for each species group
  cv_chinook = unlist(lapply(harvest_scenarios, function(x) {
    mn = report(spp = "chinook", gear = "total", stratum = "total", boot_out_use = x, CI = FALSE, return_numeric = TRUE)
    sd = sd(subset(x, gear == "total" & stratum == "total")$chinook, na.rm = TRUE)
    percentize(sd/mn, escape = TRUE)
  }))

  cv_chum = unlist(lapply(harvest_scenarios, function(x) {
    mn = report(spp = "chum", gear = "total", stratum = "total", boot_out_use = x, CI = FALSE, return_numeric = TRUE)
    sd = sd(subset(x, gear == "total" & stratum == "total")$chum, na.rm = TRUE)
    percentize(sd/mn, escape = TRUE)
  }))

  cv_sockeye = unlist(lapply(harvest_scenarios, function(x) {
    mn = report(spp = "sockeye", gear = "total", stratum = "total", boot_out_use = x, CI = FALSE, return_numeric = TRUE)
    sd = sd(subset(x, gear == "total" & stratum == "total")$sockeye, na.rm = TRUE)
    percentize(sd/mn, escape = TRUE)
  }))

  cv_total = unlist(lapply(harvest_scenarios, function(x) {
    mn = report(spp = "total", gear = "total", stratum = "total", boot_out_use = x, CI = FALSE, return_numeric = TRUE)
    sd = sd(subset(x, gear == "total" & stratum == "total")$total, na.rm = TRUE)
    percentize(sd/mn, escape = TRUE)
  }))

  # build the data frame to print
  tab = data.frame(
    combo_names,
    pretty_chinook_ests, chinook_p_diff, cv_chinook,
    pretty_chum_ests, chum_p_diff, cv_chum,
    pretty_sockeye_ests, sockeye_p_diff, cv_sockeye,
    pretty_total_ests, total_p_diff, cv_total
  )

  # make nice column names
  colnames(tab) = c("Scenario", rep(c("Estimate", "\\% Change", "CV"), 4))

  # build the kable to print
  knitr::kable(tab, "latex", booktabs = TRUE, longtable = FALSE, linesep = "", align = "lcccccccccccc", escape = FALSE) %>%
    kableExtra::kable_styling(full_width = F, latex_options = c("scale_down", "HOLD_position")) %>%
    kableExtra::add_header_above(c(" " = 1, "Chinook" = 3, "Chum" = 3, "Sockeye" = 3, "Total" = 3), bold = TRUE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::row_spec(1:(nrow(tab) - 1), hline_after = TRUE) %>%
    kableExtra::column_spec(1, bold = TRUE) %>%
    add_vspace
}
