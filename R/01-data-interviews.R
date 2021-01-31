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

  ### STEP X: handle the stratum name
  dat_out$stratum = dat_in$stratum

  ### STEP X: handle the gear (net) type
  gear_entered = dat_in$gear
  gear_standard = tolower(gear_entered) # make lowercase
  gear_standard = stringr::str_remove(gear_standard, "net")
  dat_out$gear = gear_standard

  ### STEX X: handle net dimensions
  has_mesh = "mesh" %in% vars
  dat_out$net_length = dat_in$length
  if (has_mesh) {
    dat_out$mesh_size = round(dat_in$mesh, 3)
  } else {
    dat_out$mesh_size = NA
  }

  ### STEP X: handle trip times
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

  ### STEP X: handle soak times
  # extract the soak time variable name and time unit names from raw data
  # some sources use HH:MM format, others use the number of minutes or number of hours
  soak_var = vars[stringr::str_which(vars, "soak")]
  soak_units_entered = stringr::str_extract(soak_var, "_.+$")
  soak_units_entered = stringr::str_remove(soak_units_entered, "_")
  soak_class = class(dat_in[,soak_var])

  # convert to duration class: different function depending on the units and format of the input
  if (soak_units_entered == "hrs" & soak_class == "character") {
    dat_out$soak_duration = lubridate::as.duration(lubridate::hm(dat_in[,soak_var]))
  } else {
    dat_out$soak_duration = lubridate::duration(num = dat_in[,soak_var], ifelse(soak_units_entered == "hrs", "hours", "minutes"))
  }

  # more steps...

  # return the formatted output
  return(dat_out)
}
