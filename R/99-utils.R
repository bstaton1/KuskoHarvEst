#' Create path to a package resource file
#'
#' Constructs a complete file path to a package resource file located in
#'   `rstudio/templates/project/resources` within the 'KuskoHarvEst' library
#'
#' @param file Character; a file name or file path within the resources folder to point to
#'

resource_path = function(file) {
  system.file(file.path("rstudio", "templates", "project", "resources", file), package = "KuskoHarvEst")
}

#' Create a project directory to use for use with 'KuskoHarvEst'
#'
#' Called by the RStudio project template builder
#'
#' @param path A location to put the new project
#' @param is_for_final_report Logical; is the project for compiling all estimates
#'   into tables and figures for final reporting rather than producing estimates for one day in-season?
#'

KuskoHarvEst_skeleton = function(path, is_for_final_report) {

  # create the project directory
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  if (!is_for_final_report) {
    # create subdirectories
    dir.create(file.path(path, "data-raw"))
  } else {
    # create subdirectories
    dir.create(file.path(path, "raw-data-files"))

    # find the files
    resource_path = resource_path("07-final-report-content")
    files = list.files(resource_path)

    # build full file paths
    source = file.path(resource_path, files)
    target = file.path(path, files)

    # copy the files into the new project
    file.copy(source, target)
  }

  TRUE
}

#' Convert date and time variables into a datetime variable
#'
#' @param dates Character; vector with elements in `M/D/YYYY` format
#' @param times Character; vector with elements in `H:MM` format
#'
#' @return A vector with of class `datetime`, with the time zone set to US/Alaska
#'

combine_datetime = function(dates, times) {
  # step 1: combine dates and times and into standardized format
  step1 = suppressWarnings(lubridate::mdy_hms(paste(dates, paste0(times, ":00")), tz = "US/Alaska"))

  # step 2: force it to be a datetime object in R
  step2 = lubridate::as_datetime(step1, tz = "US/Alaska")

  # return the output
  return(step2)
}

#' Convert a proportion to a percent
#'
#' @param x Numeric; vector containing values on the proportional scale to be converted to a percentage value
#' @param escape Logical; should the percent symbol be escaped? Aids in placing this output into LaTeX tables
#' @param digits Numeric; supplied to [base::round()]. Defaults to `0`.
#'
#' @return Character vector storing percentage values. If a non-zero value would be rounded to zero, `"<1%"` is returned instead.
#'

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
#' Convert a `datetime` object to a character
#'   class that is easier to read for humans.
#'
#' @param datetimes Object of class `datetime`
#' @param include_dates Logical; if `TRUE` `(M/D)` will be appended to the back of the output.
#'   If `FALSE`, only the time (12-hour clock) will be returned.
#'

short_datetime = function(datetimes, include_date = FALSE) {

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
#' Adds the appropriate LaTeX code to a summary produced by
#'   [report()] to make the CI portion smaller than the mean portion.
#'   This is a nice touch for tabular output.
#'
#' @param x Character; the output of [report()]
#' @param linebreak Logical; should a linebreak be inserted between the mean portion and the CI portion?

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

#' Capitalize a character string
#'
#' Applies [base::toupper()] to the first character in each
#'   element of a character vector
#'
#' @param x Character; a vector of character strings for which the
#'   first character should be converted to uppercase

capitalize = function (x) {
  if (!is.character(x)) stop("x must be of class 'character'")
  first = substr(x, 1, 1)
  last = substr(x, 2, nchar(x))
  paste0(toupper(first), last)
}

#' Create a date for use in file names
#'
#' Converts a `datetime` object to a
#'   character class with format `YYYY_MM_DD`. This format
#'   is useful in constructing file names.
#'
#' @param x and object of class `datetime`
#'

file_date = function(x) {
  day = stringr::str_pad(lubridate::day(x), width = 2, side = "left", pad = "0")
  month = stringr::str_pad(lubridate::month(x), width = 2, side = "left", pad = "0")
  year = lubridate::year(x)
  paste(year, month, day, sep = "_")
}

#' Create a basic date from date time object
#'
#' Convert a `datetime` object to a character
#'   class that is easier to read for humans.
#'
#' @param datetime Object of class `datetime`
#'

basic_date = function(datetime) {
  day = lubridate::day(datetime)
  month = lubridate::month(datetime)
  year = lubridate::year(datetime)
  paste(month, day, year, sep = "/")
}

#' A function to add vspace to the bottom of a kable
#'
#' @param kable_input Output of a `knitr::kable() %>% kableExtra::fn()` chain.
#' @param space Character; LaTex units and magnitude of space to include in a vspace
#'   call at the bottom of the table
#' @details This function should be called as the last step in the chain of commands.
#'

add_vspace = function(kable_input, space = "-1em") {
  kable_input_new = paste(c(as.character(kable_input), "\n\\vspace{", space, "}"), collapse = "")
  class(kable_input_new) = class(kable_input)
  attributes(kable_input_new) = attributes(kable_input)
  return(kable_input_new)
}

#' Create a markdown link to a local documentation file
#'
#' @param doc Character; file path to the documentation file in question
#' @param text Character; the text to display as the clickable link
#'

link_to_doc = function(doc, text = "here") {
  paste0('[', text, '](./', doc, '){target="_blank"}')
}

#' Summation-informed rounding
#'
#' Rounds a vector such that the sum of the rounded vector equals the sum of the unrounded vector
#'
#' @param x Numeric; vector to be rounded
#' @param digits Numeric; number of decimal points to round to
#' @references The source code for this function was copied from [this Stack Overflow answer](https://stackoverflow.com/a/35930285/3911200)

smart_round = function(x, digits = 0) {
  # copied from https://stackoverflow.com/a/35930285/3911200
  up = 10 ^ digits
  x = x * up
  y = floor(x)
  indices = tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] = y[indices] + 1
  y/up
}
