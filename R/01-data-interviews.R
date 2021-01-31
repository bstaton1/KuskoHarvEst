#' Format interview data for analysis
#'
#' @export

prepare_interviews = function(input_file, src_name = NULL) {

  ### STEP 0: load the input data file & format column names
  dat_in = read.csv(input_file, stringsAsFactors = F)

  # which variables are available?
  vars = colnames(dat_in)

  # replace . with _
  vars = stringr::str_replace_all(vars, "\\.", "_")

  # make lower case
  vars = tolower(vars)
  colnames(dat_in) = vars

  ### STEP 1: handle the source name
  if (is.null(src_name)) {
    src_name = stringr::str_extract(basename(input_file), "[A-Z]+")
  }
  dat_out = data.frame(source = rep(src_name, nrow(dat_in)))

  ### STEP 2: handle trip times
  has_starttime = "trip_start" %in% vars
  has_endtime = "trip_end" %in% vars

  # this code still assumes start and end times are on the same calendar day
  if (has_starttime) {
    dat_out$trip_start = combine_datetime(dat_in$date, dat_in$trip_start)
  } else {
    dat_out$trip_start = NA
  }
  if (has_endtime) {
    dat_out$trip_end = combine_datetime(dat_in$date, dat_in$trip_end)
  } else {
    dat_out$trip_end = NA
  }

  # calculate trip duration
  dat_out$trip_duration = lubridate::as.duration(lubridate::interval(dat_out$trip_start, dat_out$trip_end))

  ### STEP 3: handle soak times
  soak_var = vars[stringr::str_which(vars, "soak")]
  soak_units_entered = stringr::str_extract(soak_var, "_.+$")
  soak_units_entered = stringr::str_remove(soak_units_entered, "_")

  # convert to a duration class
  dat_out$soak_duration = lubridate::duration(dat_in[,soak_var], ifelse(soak_units_entered == "hrs", "hours", "minutes"))

  # more steps...

  # return the formatted output
  return(dat_out)
}

