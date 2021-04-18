#' Create a table to report the number of interviews by data source
#'
#' @importFrom magrittr %>%
#' @export

interview_data_table = function(interview_data) {
  # count the number of interviews from each data source
  tab = sort(table(interview_data$source), decreasing = TRUE)

  # use the longer names
  names(tab) = source_names[names(tab),"source_long"]
  tab = t(t(tab))
  tab = rbind(tab, Total = sum(tab))

  # append the percent of all interviews by source
  tab = cbind(tab, Percent = percentize(tab[,1]/tab["Total",1]))

  # formatting
  tab = cbind("Data Source" = rownames(tab), tab); rownames(tab) = NULL
  colnames(tab)[2] = "Interviews"

  # build the kable
  knitr::kable(tab, "latex", linesep = "", booktabs = TRUE, longtable = FALSE, escape = TRUE, align = "lrr",
               caption = "The number and percent of fisher interviews conducted by location and organization.") %>%
    kableExtra::kable_styling(position = "center", latex_options = "HOLD_position") %>%
    kableExtra::row_spec(nrow(tab) - 1, hline_after = TRUE) %>%
    kableExtra::row_spec(nrow(tab), bold = TRUE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    add_vspace
}

#' Create a table to report the flights and the counts that were made on each
#'
#' @importFrom magrittr %>%
#' @export

flight_data_table = function(flight_data) {

  # determine if flights were flown on multiple days
  multi_day = ifelse(length(unique(lubridate::date(flight_data$start_time))) > 1 | length(unique(lubridate::date(flight_data$end_time))) > 1, T, F)

  # format the start and end times of each flight
  start_time = short_datetime(flight_data$start_time, include_date = multi_day)
  end_time = short_datetime(flight_data$end_time, include_date = multi_day)

  # calculate the number of hours each flight took
  duration = lubridate::interval(flight_data$start_time, flight_data$end_time)
  duration = round(lubridate::int_length(duration)/3600, 2)

  # calculate the total trip count for each flight by gear type
  drift_count = rowSums(flight_data[,stringr::str_detect(colnames(flight_data), "drift")])
  set_count = rowSums(flight_data[,stringr::str_detect(colnames(flight_data), "set")])

  # build the table to pass to kable
  tab = data.frame(start_time, end_time, duration, drift_count, set_count)

  # build the kable
  knitr::kable(tab, "latex", col.names = c("Start Time", "End Time", "Hours", "Drift", "Set"), linesep = "", booktabs = TRUE, longtable = FALSE, escape = TRUE, align = "llccc",
               caption = "The times each flight was conducted and the number of fishers counted on each.") %>%
    kableExtra::kable_styling(position = "center", latex_options = "HOLD_position") %>%
    kableExtra::add_header_above(c("Time Information" = 3, "Nets Counted" = 2), bold = TRUE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    add_vspace
}

#' Create a table to summarize information spatially
#'
#' @importFrom magrittr %>%
#' @export

strata_summary_table = function(gear) {

  # create nice names for the strata
  strata = paste0(strata_names$stratum_start, " $\\longleftrightarrow$ ", strata_names$stratum_end)
  names(strata) = strata_names$stratum

  # determine the correct effort info to use depending on the gear
  if (gear == "drift") {
    effort_info = drift_effort_info
  } else {
    effort_info = set_effort_info
  }

  # calculate/format harvest by stratum
  chinook = sapply(c("A", "B", "C", "D1"), function(stratum) tinyCI(report(spp = "chinook", stratum = stratum, gear = gear)))
  chum = sapply(c("A", "B", "C", "D1"), function(stratum) tinyCI(report(spp = "chum", stratum = stratum, gear = gear)))
  sockeye = sapply(c("A", "B", "C", "D1"), function(stratum) tinyCI(report(spp = "sockeye", stratum = stratum, gear = gear)))
  total = sapply(c("A", "B", "C", "D1"), function(stratum) tinyCI(report(spp = "total", stratum = stratum, gear = gear)))

  # build the strata-specific information for the table
  tab = cbind(
    # Stratum = kableExtra::linebreak(strata, align = "l"),
    Stratum = strata,
    Interviews = with(interview_data[interview_data$gear == gear,], table(stratum)),
    "Effort Est." = effort_info$effort_est_stratum,
    Chinook = chinook, Chum = chum, Sockeye = sockeye, Total = total
  )

  # build the across-strata information for the table
  tot_int = sum(as.numeric(tab[,"Interviews"]))
  tot_eff = sum(as.numeric(tab[,"Effort Est."]))
  tot_harv = sapply(c("chinook", "chum", "sockeye", "total"), function(spp) tinyCI(report(spp = spp, stratum = "total", gear = gear)))
  tots = c(Stratum = "All", Interviews = tot_int, "Effort Est." = tot_eff, tot_harv)

  # combine
  tab = rbind(tab, tots)

  # build the kable
  knitr::kable(tab, "latex", booktabs = TRUE, longtable = FALSE, linesep = "", escape = FALSE, row.names = FALSE,
               align = "lcccccc",
               caption = paste0("Summary of relevant quantities by river stratum (area) for ", gear, " nets. Numbers in parentheses are 95\\% confidence intervals.")) %>%
    kableExtra::kable_styling(full_width = FALSE, latex_options = c("HOLD_position", "scale_down")) %>%
    kableExtra::add_header_above(c(" " = 3, "Estimated Harvest" = 4), bold = T) %>%
    kableExtra::row_spec(c(0, nrow(tab)), bold = TRUE) %>%
    kableExtra::row_spec(1:(nrow(tab) - 1), hline_after = TRUE) %>%
    kableExtra::column_spec(ncol(tab), bold = TRUE) %>%
    kableExtra::column_spec(1, bold = TRUE) %>%
    add_vspace
}

#' Create a table to summarize catch rates and species composition relative to Johnson River
#'
#' @importFrom magrittr %>%
#' @export

johnson_summary_table = function() {
  # extract effort estimates above and below Johnson R.
  below_johnson_effort = drift_effort_info$effort_est_stratum["A"]
  above_johnson_effort = sum(drift_effort_info$effort_est_stratum[c("B", "C", "D1")])

  # extract bootstrap estimates of total harvest below and above johnson R
  below_johnson_total = subset(boot_out, gear == "drift" & stratum == "A")[,"total"]
  above_johnson_total = with(subset(boot_out, gear == "drift" & stratum != "A" & stratum != "total"), tapply(total, iter, sum))

  # extract bootstrap estimates of chinook harvest below and above johnson R
  below_johnson_chinook = subset(boot_out, gear == "drift" & stratum == "A")[,"chinook"]
  above_johnson_chinook = with(subset(boot_out, gear == "drift" & stratum != "A" & stratum != "total"), tapply(chinook, iter, sum))

  # calculate bootstrap estimates of total salmon catch per trip above and below johnson R
  below_johnson_cpt = below_johnson_total/below_johnson_effort
  above_johnson_cpt = above_johnson_total/above_johnson_effort

  # calculate bootstrap estimates of percent chinook catch per trip above and below johnson R
  below_johnson_pchinook = below_johnson_chinook/below_johnson_total
  above_johnson_pchinook = above_johnson_chinook/above_johnson_total

  # function to summarize/format the output
  f = function(x, as_percent) {
    summs = c(mean(x), quantile(x, c(0.025, 0.975)))
    if (!as_percent) {
      summs = round(summs)
      out = paste0(summs[1], " (", summs[2], " -- ", summs[3], ")")
    } else {
      out = paste0(percentize(summs[1], escape = T), " (", percentize(summs[2], escape = T), " -- ", percentize(summs[3], escape = T), ")")
    }
    out
  }

  # summarize/format bootstrapped output
  cpt_value = c(f(below_johnson_cpt, F), f(above_johnson_cpt, F))
  pchinook_value = c(f(below_johnson_pchinook, T), f(above_johnson_pchinook, T))

  # add tiny CIs
  cpt_value = sapply(cpt_value, tinyCI, linebreak = FALSE)
  pchinook_value = sapply(pchinook_value, tinyCI, linebreak = FALSE)

  # build the table to pass to kable()
  tab = rbind(
    c("Total Catch/Trip", cpt_value),
    c("\\% Chinook Salmon", pchinook_value)
  ); rownames(tab) = NULL

  # build the kable
  knitr::kable(tab, "latex", col.names = c("Quantity", "Downstream", "Upstream"),
               row.names = FALSE, booktabs = TRUE, longtable = FALSE, linesep = "",
               align = "lcc", escape = FALSE,
               caption = "Average (95\\% confidence limits) total salmon catch per trip and percent Chinook salmon, summarized for the areas above and below the confluence of the Johnson River with the Kuskokwim River. Quantities are derived from the strata- and species-specific harvest estimates, not the raw interview data.") %>%
    kableExtra::kable_styling(full_width = FALSE, latex_options = "HOLD_position") %>%
    kableExtra::add_header_above(c(" " = 1, "Proximity to Johnson R. Mouth" = 2), bold = TRUE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    add_vspace
}

#' Create a table to go in the report appendix
#'
#' @importFrom magrittr %>%
#' @export

make_appendix_table = function(interview_data, gear, variable) {

  # set the variables that are accepted, and perform error check
  accepted_variables = c(
    "chinook_rate",
    "chinook",
    "chum+sockeye",
    "chum+sockeye_rate",
    "soak_duration",
    "trip_start", "trip_end", "trip_duration",
    "net_length",
    "p_chinook"
  )

  if (!(variable %in% accepted_variables)) {
    stop ("supplied value for variable argument ('", variable, "') not accepted.\nAccepted values are:\n", paste0("  '", accepted_variables, "'\n"))
  }

  # subset out only data relevant for the table
  x_data = interview_data[is_complete_trip(interview_data) & !is.na(interview_data$stratum) & interview_data$gear == gear,]

  # prepare information: chinook catch per trip
  if (variable == "chinook") {
    x = x_data$chinook
    cap = paste0("Summary of ", gear, " net catch per trip of Chinook salmon by fishing area.")
    digits = 0
  }

  # prepare information: chinook catch rate per trip
  if (variable == "chinook_rate") {
    x_data = x_data[x_data$suit_cr_reliable,]
    x = x_data$chinook/(as.numeric(x_data$soak_duration, "hours") * x_data$net_length) * 150
    cap = paste0("Summary of ", gear, " net catch rate of Chinook salmon by fishing area (units are salmon per 150 feet of net soaked for 1 hour).")
    digits = 1
  }

  # prepare information: chum+sockeye catch per trip
  if (variable == "chum+sockeye") {
    x = x_data$chum + x_data$sockeye
    cap = paste0("Summary of ", gear, " net catch per trip of chum+sockeye salmon by fishing area.")
    digits = 0
  }

  # prepare information: chum+sockeye catch rate per trip
  if (variable == "chum+sockeye_rate") {
    x_data = x_data[x_data$suit_cr_reliable,]
    x = (x_data$chum + x_data$sockeye)/(as.numeric(x_data$soak_duration, "hours") * x_data$net_length) * 150
    cap = paste0("Summary of ", gear, " net catch rate of chum+sockeye salmon by fishing area (units are salmon per 150 feet of net soaked for 1 hour).")
    digits = 1
  }

  # prepare information: soak time per trip
  if (variable == "soak_duration") {
    x_data = x_data[x_data$suit_avg_soak,]
    x = as.numeric(x_data$soak_duration, "hours")
    cap = paste0("Summary of ", gear, " net active fishing hours by fishing area.")
    digits = 1
  }

  # prepare information: trip duration
  if (variable == "trip_duration") {
    x_data = x_data[KuskoHarvEst:::is_possible_trip(x_data),]
    x = as.numeric(x_data$soak_duration, "hours")
    cap = paste0("Summary of ", gear, " net active fishing hours by fishing area.")
    digits = 1
  }

  # prepare information: net length
  if (variable == "net_length") {
    x_data = x_data[x_data$suit_avg_net,]
    x = x_data$net_length
    cap = paste0("Summary of ", gear, " net length (feet) by fishing area.")
    digits = 0
  }

  # prepare information: trip start time
  if (variable == "trip_start") {
    x_data = x_data[KuskoHarvEst:::is_possible_trip(x_data),]
    x = x_data$trip_start
    cap = paste0("Summary of ", gear, " net trip start time by fishing area.")
    digits = NA
  }

  # prepare information: trip end time
  if (variable == "trip_end") {
    x_data = x_data[KuskoHarvEst:::is_possible_trip(x_data),]
    x = x_data$trip_end
    cap = paste0("Summary of ", gear, " net trip end time by fishing area.")
    digits = NA
  }

  # prepare information: percent chinook catches
  if (variable == "p_chinook") {
    x = x_data$chinook/(x_data$chinook + x_data$chum + x_data$sockeye)
    cap = paste0("Summary of ", gear, " net percent composition of Chinook salmon by fishing area.")
    digits = NA
  }

  # calculate the number of interviews used
  N = tapply(x, x_data$stratum, function(z) length(!is.na(z)))
  N_all = sum(N)

  # calculate and format summaries: numerical quantities that need only rounding for format
  if (!is.na(digits)) {
    Min = round(tapply(x, x_data$stratum, function(z) min(z, na.rm = TRUE)), digits = digits)
    q25 = round(tapply(x, x_data$stratum, function(z) quantile(z, 0.25, na.rm = TRUE)), digits = digits)
    Mean = round(tapply(x, x_data$stratum, function(z) mean(z, na.rm = TRUE)), digits = digits)
    q75 = round(tapply(x, x_data$stratum, function(z) quantile(z, 0.75, na.rm = TRUE)), digits = digits)
    Max = round(tapply(x, x_data$stratum, function(z) max(z, na.rm = T)), digits = digits)

    all_min = round(min(x, na.rm = TRUE), digits = digits)
    all_q25 = round(quantile(x, 0.25, na.rm = TRUE), digits = digits)
    all_mean = round(mean(x, na.rm = TRUE), digits = digits)
    all_q75 = round(quantile(x, 0.75, na.rm = TRUE), digits = digits)
    all_max = round(max(x, na.rm = TRUE), digits = digits)
  }

  # calculate and format summaries: percent chinook
  if (is.na(digits) & variable == "p_chinook") {
    Min = percentize(tapply(x, x_data$stratum, function(z) min(z, na.rm = TRUE)), escape = TRUE)
    q25 = percentize(tapply(x, x_data$stratum, function(z) quantile(z, 0.25, na.rm = TRUE)), escape = TRUE)
    Mean = percentize(tapply(x, x_data$stratum, function(z) mean(z, na.rm = TRUE)), escape = TRUE)
    q75 = percentize(tapply(x, x_data$stratum, function(z) quantile(z, 0.75, na.rm = TRUE)), escape = TRUE)
    Max = percentize(tapply(x, x_data$stratum, function(z) max(z, na.rm = T)), escape = TRUE)

    all_min = percentize(min(x, na.rm = TRUE), escape = TRUE)
    all_q25 = percentize(quantile(x, 0.25, na.rm = TRUE), escape = TRUE)
    all_mean = percentize(mean(x, na.rm = TRUE), escape = TRUE)
    all_q75 = percentize(quantile(x, 0.75, na.rm = TRUE), escape = TRUE)
    all_max = percentize(max(x, na.rm = TRUE), escape = TRUE)
  }

  # calculate and format summaries: trip times
  if (is.na(digits) & variable != "p_chinook"){
    Min = short_datetime(aggregate(x ~ x_data$stratum, FUN = min, na.rm = T)$x); names(Min) = unique(x_data$stratum)
    q25 = short_datetime(aggregate(x ~ x_data$stratum, FUN = quantile, prob = 0.25, na.rm = T)$x); names(q25) = unique(x_data$stratum)
    Mean = short_datetime(aggregate(x ~ x_data$stratum, FUN = mean, na.rm = T)$x); names(Mean) = unique(x_data$stratum)
    q75 = short_datetime(aggregate(x ~ x_data$stratum, FUN = quantile, prob = 0.75, na.rm = T)$x); names(q75) = unique(x_data$stratum)
    Max = short_datetime(aggregate(x ~ x_data$stratum, FUN = max, na.rm = T)$x); names(Max) = unique(x_data$stratum)

    all_min = short_datetime(min(x, na.rm = TRUE))
    all_q25 = short_datetime(quantile(x, 0.25, na.rm = TRUE))
    all_mean = short_datetime(mean(x, na.rm = TRUE))
    all_q75 = short_datetime(quantile(x, 0.75, na.rm = TRUE))
    all_max = short_datetime(max(x, na.rm = TRUE))
  }

  # build the table
  strata_tab = cbind(N, Min, "25\\%" = q25, Mean, "75\\%" = q75, Max)
  all_tab = c(N = N_all, Min = all_min, "25\\%" = all_q25, Mean = all_mean, "75\\%" = all_q75, Max = all_max)
  tab = rbind(strata_tab, all_tab)

  # create nice names for the strata
  strata = paste0(strata_names$stratum_start[strata_names$stratum %in% rownames(tab)], " $\\longleftrightarrow$ ", strata_names$stratum_end[strata_names$stratum %in% rownames(tab)])
  strata = c(strata, "All")

  # combine with summaries
  tab = cbind(Area = strata, tab)
  rownames(tab) = NULL

  # build the kable
  knitr::kable(tab, "latex", booktabs = TRUE, longtable = FALSE, linesep = "", caption = cap, escape = F, align = "lcccccc") %>%
    kableExtra::kable_styling(full_width = FALSE, latex_options = "HOLD_position") %>%
    kableExtra::row_spec(c(0, nrow(tab)), bold = TRUE) %>%
    kableExtra::row_spec(nrow(tab) - 1, hline_after = TRUE) %>%
    kableExtra::column_spec(1, bold = TRUE) %>%
    add_vspace
}
