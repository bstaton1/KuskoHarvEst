#' Create a table to report the number of interviews by data source
#'
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
    kableExtra::row_spec(0, bold = TRUE)
}

#' Create a table to report the flights and the counts that were made on each
#'
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
    kableExtra::row_spec(0, bold = TRUE)
}

