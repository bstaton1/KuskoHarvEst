#' Convert date and time variables into a datetime variable
#'
#'

combine_datetime = function(dates, times) {
  # step 1: combine dates and times and into standardized format
  step1 = suppressWarnings(lubridate::mdy_hms(paste(dates, paste0(times, ":00")), tz = "US/Alaska"))

  # step 2: force it to be a datetime object in R
  step2 = lubridate::as_datetime(step1, tz = "US/Alaska")

  # return the output
  return(step2)
}

#' Combine a List of Data Frames
#'
#'

unlist_dfs = function(list) {
  # empty object
  output = NULL

  # loop through list elements, combining the data frame in each with all previous
  for (i in 1:length(list)) output = rbind(output, list[[i]])

  # return the output
  return(output)
}

#' Convert a proportion to a percent
#'
#' @export

percentize = function(x, escape = FALSE, digits = 0) {
  # create the percent version
  out = paste0(round(x * 100, digits = digits), ifelse(escape, "\\%", "%"))

  # test if a non-zero proportion was rounded to a zero value percent
  # if so, change output to be <1%
  zero_test = ifelse(escape, "0\\%", "0%")
  lt1_replace = ifelse(escape, "<1\\%", "<1%")
  ifelse(out == zero_test & x > 0, lt1_replace, out)
}

#' Format a datetime object to be shorter
#'
#' @export

short_datetime = function(datetimes, include_date = F) {

  # extract the day and month, and format them as M/D
  dates_short = paste(
    lubridate::month(datetimes),
    lubridate::day(datetimes), sep = "/"
  )

  # extract the time and format it as 12 hour clock (drop seconds as well)
  times_short = format(datetimes, format = "%I:%M %p")
  times_short = stringr::str_remove(times_short, "^0")

  # combine them if requested
  if (include_date) {
    out = paste0(times_short, " (", dates_short, ")")
  } else {
    out = times_short
  }

  # return the output
  return(out)
}

#' Make the CI part of a summary smaller text than the mean
#'

tinyCI = function(x, linebreak = TRUE) {
  # if x has CIs
  if (stringr::str_detect(x, "\\(")) {
    # build the replacement text
    replacement = paste0("\\footnotesize{", stringr::str_extract(x, "\\(.+\\)"), "}")

    # extract the mean part
    x = stringr::str_extract(x, "^.+ \\(")

    # paste on the CI replacement text
    x = paste0(substr(x, 1, nchar(x) - 1), replacement)

    # include the latex code to put the CI on a new line if in a table cell
    if (linebreak) {
      x = stringr::str_replace(x, " \\\\f", "\n\\\\f")
      x = kableExtra::linebreak(x, align = "c")
    }
    return(x)
  } else {
    return(x)
  }
}
