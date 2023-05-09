#' Create a table to report the number of interviews by data source
#'
#' @inheritParams estimate_harvest
#'
#' @importFrom magrittr %>%
#' @export
#'

make_interview_data_table = function(interview_data) {
  # count the number of interviews from each data source
  tab = sort(table(interview_data$source), decreasing = TRUE)

  # use the longer names
  names(tab) = source_names[names(tab),"source_long"]
  tab = t(t(tab))
  tab = rbind(tab, Total = sum(tab))

  # append the percent of all interviews by source
  p = KuskoHarvUtils::smart_round(tab[,1]/tab["Total",1], digits = 2)
  tab = cbind(tab, Percent = KuskoHarvUtils::percentize(p))

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
    KuskoHarvUtils::add_vspace()
}

#' Create a table to report the flights and the counts that were made on each
#'
#' @inheritParams estimate_effort
#'
#' @importFrom magrittr %>%
#' @export

make_flight_data_table = function(flight_data) {

  # determine if flights were flown on multiple days
  multi_day = ifelse(length(unique(lubridate::date(flight_data$start_time))) > 1 | length(unique(lubridate::date(flight_data$end_time))) > 1, T, F)

  # format the start and end times of each flight
  start_time = KuskoHarvUtils::short_datetime(flight_data$start_time, include_date = multi_day)
  end_time = KuskoHarvUtils::short_datetime(flight_data$end_time, include_date = multi_day)

  # calculate the number of hours each flight took
  duration = lubridate::interval(flight_data$start_time, flight_data$end_time)
  duration = round(lubridate::int_length(duration)/3600, 2)

  # calculate the total trip count for each flight by gear type
  drift_count = rowSums(flight_data[,stringr::str_detect(colnames(flight_data), "drift")])
  set_count = rowSums(flight_data[,stringr::str_detect(colnames(flight_data), "set")])

  # build the table to pass to kable
  tab = data.frame(start_time, end_time, duration, drift_count, set_count)

  # build the kable
  knitr::kable(tab, "latex", col.names = c("Start Time", "End Time", "Hours", "Drift", "Set"), linesep = "", booktabs = TRUE, longtable = FALSE, escape = TRUE, align = "rrccc",
               caption = "The time each flight was conducted and fishers counted each flight.") %>%
    kableExtra::kable_styling(position = "center", latex_options = "HOLD_position") %>%
    kableExtra::add_header_above(c("Time Information" = 3, "Nets Counted" = 2), bold = TRUE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    KuskoHarvUtils::add_vspace()
}

#' Create a table to summarize information spatially
#'
#' @inheritParams estimate_harvest
#' @param nonsalmon Logical; should the table display estimates for non-salmon instead of salmon species?
#'
#' @importFrom magrittr %>%
#' @export

make_strata_summary_table = function(interview_data, gear, nonsalmon = FALSE) {

  # determine the correct effort info to use depending on the gear
  if (gear == "drift") {
    effort_info = drift_effort_info
  } else {
    effort_info = set_effort_info
  }

  # figure out species to show & return useful error if none are found
  spp = species_in_data(interview_data)[[ifelse(nonsalmon, "nonsalmon", "salmon")]]
  if (length(spp) == 0) stop (paste0("No species found matching nonsalmon = ", TRUE))
  if (length(spp) > 1) spp = c(spp, "total")

  # summarize/format harvest: by species and stratum
  spp_summs = lapply(spp, function(sp) {
    st_summs = lapply(strata_names$stratum, function(stratum) {
      KuskoHarvUtils::tinyCI(report(spp = sp, stratum = stratum, gear = gear))
    })
    names(st_summs) = strata_names$stratum
    st_summs
  })
  names(spp_summs) = spp
  harv_tab = do.call(cbind, spp_summs)

  # summarize/format harvest: by species for all strata
  spp_summs = lapply(spp, function(sp) {
    KuskoHarvUtils::tinyCI(report(spp = sp, stratum = "total", gear = gear))
  })
  names(spp_summs) = spp
  harv_tot_tab = do.call(cbind, spp_summs)
  rownames(harv_tot_tab) = "total"

  # combine harvest
  harv_tab = rbind(harv_tab, harv_tot_tab)
  colnames(harv_tab) = KuskoHarvUtils::capitalize(colnames(harv_tab))

  # create nice names for the strata
  strata = paste0(strata_names$stratum_start, " $\\longleftrightarrow$ ", strata_names$stratum_end)
  names(strata) = strata_names$stratum

  # count the number of interviews per stratum
  n_interviews = with(interview_data[interview_data$gear == gear,], table(factor(stratum, levels = strata_names$stratum)))
  n_interviews = c(n_interviews, total = sum(n_interviews))

  # extract the estimated effort per stratum
  n_effort = effort_info$effort_est_stratum
  n_effort = c(n_effort, total = sum(n_effort))

  # combine information into table
  tab = cbind(
    Strata = c(strata, total = "Total"),
    Interviews = n_interviews,
    "Effort Est." = n_effort,
    harv_tab
  )

  latex_options = "HOLD_position"
  if (length(spp) > 2) latex_options = c(latex_options, "scale_down")

  # build the kable
  knitr::kable(tab, "latex", booktabs = TRUE, longtable = FALSE, linesep = "", escape = FALSE, row.names = FALSE,
               align = paste(c("l", rep("c", 2 + length(spp))), collapse = ""),
               caption = paste0("Summary of relevant quantities by river stratum (area) for ", gear, " nets. Numbers in parentheses are 95\\% confidence intervals.")) %>%
    kableExtra::kable_styling(full_width = FALSE, latex_options = latex_options) %>%
    kableExtra::add_header_above(c(" " = 3, "Estimated Harvest" = length(spp)), bold = TRUE) %>%
    kableExtra::row_spec(c(0, nrow(tab)), bold = TRUE) %>%
    kableExtra::row_spec(1:(nrow(tab) - 1), hline_after = TRUE) %>%
    kableExtra::column_spec(ncol(tab), bold = length(spp) > 1) %>%
    kableExtra::column_spec(1, bold = TRUE) %>%
    KuskoHarvUtils::add_vspace()
}

#' Create a table to summarize catch rates and species composition relative to Johnson River
#'
#' @details Requires that an object named `boot_out` to be in existence
#'
#' @importFrom magrittr %>%
#' @export

make_johnson_summary_table = function() {

  # figure out species to show
  spp = species_in_data(boot_out)[["salmon"]]

  # extract effort estimates above and below Johnson R.
  below_johnson_effort = drift_effort_info$effort_est_stratum["A"]
  above_johnson_effort = sum(drift_effort_info$effort_est_stratum[c("B", "C", "D1")])

  # extract bootstrap estimates of total harvest below and above johnson R
  below_johnson_total = subset(boot_out, gear == "drift" & stratum == "A")[,"total"]
  above_johnson_total = with(subset(boot_out, gear == "drift" & stratum != "A" & stratum != "total"), tapply(total, iter, sum))

  # extract bootstrap estimates of species-specific harvest below and above johnson R
  below_johnson_spp = as.matrix(subset(boot_out, gear == "drift" & stratum == "A")[,spp])
  above_johnson_spp = lapply(spp, function(sp) {
    boot_sub = subset(boot_out, gear == "drift" & stratum != "A" & stratum != "total")[,c("iter", sp)]
    colnames(boot_sub)[2] = "to_sum"
    with(boot_sub, tapply(to_sum, iter, sum))
  })
  names(above_johnson_spp) = spp
  above_johnson_spp = do.call(cbind, above_johnson_spp)

  # calculate bootstrap estimates of total salmon catch per trip above and below johnson R
  below_johnson_cpt = below_johnson_total/below_johnson_effort
  above_johnson_cpt = above_johnson_total/above_johnson_effort

  # calculate bootstrap estimates of percent species-specific catch per trip above and below johnson R
  below_johnson_pspp = apply(below_johnson_spp, 2, function(s) s/below_johnson_total)
  above_johnson_pspp = apply(above_johnson_spp, 2, function(s) s/above_johnson_total)
  if (length(spp) > 1) {
    below_johnson_pspp = t(apply(below_johnson_pspp, 1, KuskoHarvUtils::smart_round, 2))
    above_johnson_pspp = t(apply(above_johnson_pspp, 1, KuskoHarvUtils::smart_round, 2))
  }

  # function to summarize/format the output
  f = function(x, as_percent) {
    summs = c(mean(x), quantile(x, c(0.025, 0.975)))
    if (!as_percent) {
      summs = round(summs)
      out = paste0(summs[1], " (", summs[2], " -- ", summs[3], ")")
    } else {
      out = paste0(KuskoHarvUtils::percentize(summs[1], escape = T), " (", KuskoHarvUtils::percentize(summs[2], escape = T), " -- ", KuskoHarvUtils::percentize(summs[3], escape = T), ")")
    }
    KuskoHarvUtils::tinyCI(out, linebreak = FALSE)
  }

  # summarize/format bootstrapped output
  effort_value = unname(c(below_johnson_effort, above_johnson_effort))
  cpt_value = c(f(below_johnson_cpt, FALSE), f(above_johnson_cpt, FALSE))
  pspp_value = cbind(
    do.call(rbind, lapply(1:length(spp), function(s) f(below_johnson_pspp[,s], as_percent = TRUE))),
    do.call(rbind, lapply(1:length(spp), function(s) f(above_johnson_pspp[,s], as_percent = TRUE)))
  )

  # build the table to pass to kable()
  tab = cbind(
    var = c("Total Trips", "Total Catch/Trip", KuskoHarvUtils::capitalize(spp)),
    rbind(effort_value, cpt_value, pspp_value)
  ); rownames(tab) = NULL
  tab = t(tab)
  colnames(tab) = tab[1,]; tab = tab[-1,]
  tab = cbind("Location" = c("Downstream of Johnson R. ", "Upstream of Johnson R."), tab)

  latex_options = "HOLD_position"
  if (length(spp) > 2) latex_options = c(latex_options, "scale_down")

  # build the kable
  knitr::kable(tab, "latex",
               row.names = FALSE, booktabs = TRUE, longtable = FALSE, linesep = "",
               align = paste0("lcc", paste(rep("c", length(spp)), collapse = "")), escape = FALSE,
               caption = "Estimated trips, average (95\\% confidence limits) total salmon catch per trip, and percent catch by species summarized for the areas above and below the confluence of the Johnson River with the Kuskokwim River. Quantities are derived from the strata- and species-specific harvest estimates, not the raw interview data.") %>%
    kableExtra::kable_styling(full_width = FALSE, latex_options = latex_options) %>%
    kableExtra::add_header_above(c(" " = 3, "Salmon Species \\\\% Composition" = length(spp)), bold = TRUE, escape = FALSE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::column_spec(1, bold = TRUE) %>%
    KuskoHarvUtils::add_vspace()
}

#' Create a table to go in the report appendix
#'
#' @inheritParams estimate_harvest
#' @param variable Character; accepted options are:
#'   * Any species contained in `KuskoHarvEst:::species_names$species`, additionally `"chum+sockeye"`
#'   *  The catch rate of any of these species, supplied as e.g., `"chinook_rate"`
#'   * `"trip_start"`
#'   * `"trip_end"`
#'   * `"trip_duration"`
#'   * `"soak_duration"`
#'   * `"net_length"`
#'   * `"p_chinook"`
#'
#' @importFrom magrittr %>%
#' @export
#'

make_appendix_table = function(interview_data, gear, variable) {

  # set the variables that are accepted, and perform error check
  spp_accept = c(unlist(paste(species_names$species)), "chum+sockeye")
  rate_accept = paste0(spp_accept, "_rate")
  duration_accept = c("trip_duration", "soak_duration")
  time_accept = c("trip_start", "trip_end")
  p_accept = c("p_chinook")
  net_accept = c("net_length")
  accepted_variables = c(spp_accept, rate_accept, duration_accept, time_accept, p_accept, net_accept)

  # determine what kind of variable it is
  is_catch = variable %in% spp_accept
  is_catch_rate = variable %in% rate_accept
  is_duration = variable %in% duration_accept
  is_time = variable %in% time_accept
  is_p = variable %in% p_accept
  is_net = variable %in% net_accept

  # which species are found in the interview data
  spp_found = unname(unlist(species_in_data(interview_data)))

  # if more than one variable supplied, stop
  if (length(variable) > 1) {
    stop ("only one variable can be supplied at once")
  }

  # if the supplied variable isn't in the list of those accepted, stop
  if (!(variable %in% accepted_variables)) {
    stop ("supplied value for variable argument ('", variable, "') not accepted.\nAccepted values are:", knitr::combine_words(accepted_variables, before = "  \n'", after = "'", and = " or "))
  }

  # if both chum and sockeye data are found in data, add a chum+sockeye variable
  if (all(c("chum", "sockeye") %in% spp_found)) {
    interview_data = cbind(interview_data, "chum+sockeye" = rowSums(interview_data[,c("chum", "sockeye")]))
    species_names = rbind(species_names, data.frame(species = "chum+sockeye", is_salmon = TRUE, in_text = "chum+sockeye salmon"))
  }

  # if it is a catch or a catch rate and the species name isn't in data, stop
  if ((is_catch | is_catch_rate | is_p) & (!stringr::str_remove(variable, "_rate$|^p_") %in% c(spp_found, "chum+sockeye"))) {
    stop ("species '", stringr::str_remove(variable, "_rate$|^p_"), "' is not contained in interview_data")
  }

  # subset out only data relevant for the table
  x_data = interview_data[is_complete_trip(interview_data) & !is.na(interview_data$stratum) & interview_data$gear == gear,]

  # extract columns corresponding to catch data
  spp_keep = spp_accept[spp_accept %in% colnames(x_data)]
  catch_x_data = as.matrix(x_data[,spp_keep])
  colnames(catch_x_data) = spp_keep

  # calculate catch rates
  rate_x_data = as.matrix(apply(catch_x_data, 2, function(catch) catch/(as.numeric(x_data$soak_duration, "hours") * x_data$net_length) * 150))
  colnames(rate_x_data) = paste0(spp_keep, "_rate")

  # recombine
  x_data = cbind(x_data[,c("stratum", "net_length", "trip_start", "trip_end", "trip_duration", "soak_duration", "suit_cr_reliable", "suit_avg_soak", "suit_avg_net")], catch_x_data, rate_x_data)

  # prepare information: catch per trip variables
  if (is_catch) {
    x = x_data[,variable]
    regex_spp = paste0("^", stringr::str_replace(variable, "\\+", "\\\\+"), "$")
    cap = "Summary of GEAR net catch per trip of SPECIES by fishing area." |>
      stringr::str_replace("GEAR", gear) |>
      stringr::str_replace("SPECIES", species_names$in_text[stringr::str_detect(species_names$species, regex_spp)])
    digits = 0
  }

  # prepare information: catch rate variables
  if (is_catch_rate) {
    x_data = x_data[x_data$suit_cr_reliable,]
    x = x_data[,stringr::str_remove(variable, "_rate$")]/(as.numeric(x_data$soak_duration, "hours") * x_data$net_length) * 150
    regex_spp = paste0("^", stringr::str_replace(stringr::str_remove(variable, "_rate$"), "\\+", "\\\\+"), "$")
    cap = "Summary of GEAR net catch rate of SPECIES by fishing area (fish per 150 feet of net per hour)." |>
      stringr::str_replace("GEAR", gear) |>
      stringr::str_replace("SPECIES", species_names$in_text[stringr::str_detect(species_names$species, regex_spp)])
    digits = 1
  }

  # prepare information: duration variables
  if (is_duration) {
    x_data = x_data[x_data$suit_avg_soak,]
    x = as.numeric(x_data[,variable], "hours")
    duration_phrase = ifelse(stringr::str_detect(variable, "soak"), "active fishing hours", "trip duration")
    cap = "Summary of GEAR net DURATION_PHRASE by fishing area." |>
      stringr::str_replace("GEAR", gear) |>
      stringr::str_replace("DURATION_PHRASE", duration_phrase)
    digits = 1
  }

  # prepare information: time variables
  if (is_time) {
    x_data = x_data[KuskoHarvEst:::is_possible_trip(x_data),]
    x = x_data[,variable]
    time_type = stringr::str_remove(variable, "^trip_")
    cap = "Summary of GEAR net trip TIME_TYPE time by fishing area." |>
      stringr::str_replace("GEAR", gear) |>
      stringr::str_replace("TIME_TYPE", time_type)
    digits = NA
  }

  # prepare information: species composition variables
  if (is_p) {
    species_names$is_salmon[species_names$species == "chum+sockeye"] = FALSE
    p_spp = stringr::str_remove(variable, "^p_")
    salmon_spp = spp_found[spp_found %in% species_names$species[species_names$is_salmon]]
    numerator = x_data[,p_spp]
    if (length(salmon_spp) > 1) denominator = rowSums(x_data[,salmon_spp]) else denominator = x_data[,salmon_spp]
    x = numerator/denominator
    cap = "Summary of GEAR net percent composition of SPECIES by fishing area." |>
      stringr::str_replace("GEAR", gear) |>
      stringr::str_replace("SPECIES", species_names$in_text[species_names$species == p_spp])
    digits = NA
  }

  # prepare information: net characteristics variables
  if (is_net) {
    x_data = x_data[x_data$suit_avg_net,]
    x = x_data$net_length
    cap = stringr::str_replace("Summary of GEAR net length (feet) by fishing area.")
    digits = 0
  }

  # calculate the number of interviews used
  N = tapply(x, x_data$stratum, function(z) length(!is.na(z)))
  N_all = sum(N)

  # calculate and format summaries: numerical quantities that need only rounding for format
  if (any(is_catch, is_catch_rate, is_net, is_duration)) {
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
  if (is_p) {
    Min = KuskoHarvUtils::percentize(tapply(x, x_data$stratum, function(z) min(z, na.rm = TRUE)), escape = TRUE)
    q25 = KuskoHarvUtils::percentize(tapply(x, x_data$stratum, function(z) quantile(z, 0.25, na.rm = TRUE)), escape = TRUE)
    Mean = KuskoHarvUtils::percentize(tapply(x, x_data$stratum, function(z) mean(z, na.rm = TRUE)), escape = TRUE)
    q75 = KuskoHarvUtils::percentize(tapply(x, x_data$stratum, function(z) quantile(z, 0.75, na.rm = TRUE)), escape = TRUE)
    Max = KuskoHarvUtils::percentize(tapply(x, x_data$stratum, function(z) max(z, na.rm = T)), escape = TRUE)

    all_min = KuskoHarvUtils::percentize(min(x, na.rm = TRUE), escape = TRUE)
    all_q25 = KuskoHarvUtils::percentize(quantile(x, 0.25, na.rm = TRUE), escape = TRUE)
    all_mean = KuskoHarvUtils::percentize(mean(x, na.rm = TRUE), escape = TRUE)
    all_q75 = KuskoHarvUtils::percentize(quantile(x, 0.75, na.rm = TRUE), escape = TRUE)
    all_max = KuskoHarvUtils::percentize(max(x, na.rm = TRUE), escape = TRUE)
  }

  # calculate and format summaries: trip times
  if (is_time){
    Min = KuskoHarvUtils::short_datetime(aggregate(x ~ x_data$stratum, FUN = min, na.rm = T)$x); names(Min) = unique(x_data$stratum)
    q25 = KuskoHarvUtils::short_datetime(aggregate(x ~ x_data$stratum, FUN = quantile, prob = 0.25, na.rm = T)$x); names(q25) = unique(x_data$stratum)
    Mean = KuskoHarvUtils::short_datetime(aggregate(x ~ x_data$stratum, FUN = mean, na.rm = T)$x); names(Mean) = unique(x_data$stratum)
    q75 = KuskoHarvUtils::short_datetime(aggregate(x ~ x_data$stratum, FUN = quantile, prob = 0.75, na.rm = T)$x); names(q75) = unique(x_data$stratum)
    Max = KuskoHarvUtils::short_datetime(aggregate(x ~ x_data$stratum, FUN = max, na.rm = T)$x); names(Max) = unique(x_data$stratum)

    all_min = KuskoHarvUtils::short_datetime(min(x, na.rm = TRUE))
    all_q25 = KuskoHarvUtils::short_datetime(quantile(x, 0.25, na.rm = TRUE))
    all_mean = KuskoHarvUtils::short_datetime(mean(x, na.rm = TRUE))
    all_q75 = KuskoHarvUtils::short_datetime(quantile(x, 0.75, na.rm = TRUE))
    all_max = KuskoHarvUtils::short_datetime(max(x, na.rm = TRUE))
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
    KuskoHarvUtils::add_vspace()

}

#' Create a table displaying reported harvest goal attainment
#'
#' @inheritParams estimate_harvest
#'
#' @importFrom magrittr %>%
#' @export

make_goals_summary_table = function(interview_data) {

  # check to make sure the goal data are present in the data set
  goal_columns = stringr::str_detect(colnames(interview_data), "goal")
  if (sum(goal_columns) == 0) {
    stop ("Goal data not found in the interview data.\nBe sure to use include_goals = TRUE when using prepare_interviews().")
  }

  # extract the goal info
  goals = interview_data[,goal_columns]

  # discard any records that have NAs
  goals = na.omit(goals)

  # count how many interviews have valid goal information
  n_goal_interviews = nrow(goals)

  # convert data to long format: easier tallying
  colnames(goals) = stringr::str_remove(colnames(goals), "_goal")
  goals = suppressMessages(reshape2::melt(goals, value.name = "goal", variable.name = "species"))

  # use names rather than numeric codes
  goals$named_goal = sapply(goals$goal, function(x) {
    switch(x,
           "1" = "Under Half",
           "2" = "Half",
           "3" = "Over Half",
           "4" = "Done")
  })

  # create it as a factor: keeps order consistent, and will include zero counts if no fishers report in a category
  goals$named_goal = factor(goals$named_goal, levels = c("Under Half", "Half", "Over Half", "Done"))

  # count the number of interviews by species and goal reported
  counts = table(goals$species, goals$named_goal)
  counts = as.data.frame(counts)

  # format the counts as percentages
  counts$Freq = counts$Freq/n_goal_interviews
  counts$Freq = KuskoHarvUtils::smart_round(counts$Freq, digits = 2)
  counts$Freq = KuskoHarvUtils::percentize(counts$Freq, escape = TRUE)

  # format the table
  tab = reshape2::dcast(counts, Var1 ~ Var2, value.var = "Freq")
  colnames(tab)[1] = "Species"
  tab$Species = KuskoHarvUtils::capitalize(as.character(tab$Species))

  # build the kable
  knitr::kable(tab, "latex", booktabs = TRUE, longtable = FALSE, linesep = "", align = "lcccc", escape = FALSE,
               caption = paste0("Percentage of fishers reporting that they are either under halfway done, halfway done, over halfway done, or completely done fishing for a given species, relative to their season-wide harvest goals (sample size = ", n_goal_interviews, ").")) %>%
    kableExtra::kable_styling(full_width = FALSE, latex_options = c("HOLD_position")) %>%
    kableExtra::add_header_above(c(" " = 1, "Category of Harvest Goals Attained" = 4), bold = TRUE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::column_spec(1, bold = TRUE) %>%
    KuskoHarvUtils::add_vspace()
}
