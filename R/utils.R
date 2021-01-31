#' Convert date and time variables into a datetime variable
#'
#'

combine_datetime = function(dates, times) {
  # step 1: combine dates and times and into standardized format
  step1 = lubridate::mdy_hms(paste(dates, paste0(times, ":00")), tz = "US/Alaska")

  # step 2: force it to be a datetime object in R
  step2 = lubridate::as_datetime(step1, tz = "US/Alaska")

  # return the output
  return(step2)
}

