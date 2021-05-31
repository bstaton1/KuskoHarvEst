#' Prepare one interview data file
#'
#' Reads in and formats one data file containing interview data
#'   into a standardized format to be used by other functions
#'
#' @param input_file Character; the name of one file that contains interview data from a single source and fishing day
#' @param include_village Logical; should the village of the fisher be included in the output?
#' @param include_goals Logical; should the fisher's reported progress towards meeting their season-wide harvest goals be returned?
#'

prepare_interviews_one = function(input_file, include_village = FALSE, include_goals = FALSE) {

  ### STEP 0: load the input data file & format column names
  dat_in = read.csv(input_file, stringsAsFactors = FALSE)

  # determine and delete the rows that have all NA values: Excel/CSV quirk sometimes includes these
  all_NA = sapply(1:nrow(dat_in), function(i) all(is.na(dat_in[i,]) | dat_in[i,] == ""))
  dat_in = dat_in[!all_NA,]

  # which variables are available?
  vars = colnames(dat_in)

  # replace . with _
  vars = stringr::str_replace_all(vars, "\\.", "_")

  # make lower case
  vars = tolower(vars)
  colnames(dat_in) = vars

  ### STEP 1: handle the source name
  src_name = stringr::str_extract(basename(input_file), "^[A-Z]+")
  dat_out = data.frame(source = rep(src_name, nrow(dat_in)))

  ### STEP 2: handle the stratum name
  dat_out$stratum = dat_in$stratum

  ### STEP 3: handle the gear (net) type
  gear_entered = dat_in$gear
  gear_standard = tolower(gear_entered) # make lowercase
  gear_standard = stringr::str_remove(gear_standard, "net")
  dat_out$gear = gear_standard

  ### STEX 4: handle net dimensions
  has_mesh = "mesh" %in% vars
  dat_out$net_length = dat_in$length
  if (has_mesh) {
    dat_out$mesh_size = round(dat_in$mesh, 3)
  } else {
    dat_out$mesh_size = NA
  }

  ### STEP 5: handle trip times
  has_starttime = "trip_start" %in% vars
  has_endtime = "trip_end" %in% vars

  # determine whether both date_start and date_end were supplied, or only a date column
  has_startdate = "date_start" %in% vars
  has_enddate = "date_end" %in% vars

  # determine name of the date columns to use
  startdate_use = ifelse(!has_startdate, "date", "date_start")
  enddate_use = ifelse(!has_startdate, "date", "date_end")

  # return a warning if only the "date" column was available
  if (!has_startdate & !has_enddate) {
    warning("Only the 'date' column was found in the raw data, not 'date_start' and 'date_end'.\nIt is thus assumed that all trips started and ended on the same date.")
  }

  # build the trip start and end datetime objects
  if (has_starttime) {
    dat_out$trip_start = combine_datetime(dat_in[,startdate_use], dat_in$trip_start)
  } else {
    dat_out$trip_start = NA
  }
  if (has_endtime) {
    dat_out$trip_end = combine_datetime(dat_in[,enddate_use], dat_in$trip_end)
  } else {
    dat_out$trip_end = NA
  }

  # calculate trip duration
  dat_out$trip_duration = suppressWarnings(lubridate::as.period(lubridate::interval(dat_out$trip_start, dat_out$trip_end)))

  ### STEP 6: handle soak times
  # extract the soak time variable name and time unit names from raw data
  # some sources use HH:MM format, others use the number of minutes or number of hours
  soak_var = vars[stringr::str_which(vars, "soak")]
  soak_units_entered = stringr::str_extract(soak_var, "_.+$")
  soak_units_entered = stringr::str_remove(soak_units_entered, "_")
  soak_class = class(dat_in[,soak_var])

  # convert to duration class: different function depending on the units and format of the input
  if (soak_units_entered == "hrs" & soak_class == "character") {
    dat_out$soak_duration = suppressWarnings(lubridate::as.period(round(lubridate::as.duration(lubridate::hm(dat_in[,soak_var])))))
  } else {
    dat_out$soak_duration = suppressWarnings(lubridate::as.period(round(lubridate::duration(num = dat_in[,soak_var], ifelse(soak_units_entered == "hrs", "hours", "minutes")))))
  }

  ### STEP 7: handle which species to keep
  keep_spp = c("chinook", "chum", "sockeye")
  dat_out = cbind(dat_out, dat_in[,keep_spp])

  ### STEP 8: add village information if requested
  if (include_village) {
    has_village = "village" %in% vars
    if (!has_village) {
      dat_out$village = ifelse(src_name == "BBH", "Bethel", NA)
    } else {
      dat_out$village = dat_in$village
    }
  }

  ### STEP 9: add information about goal attainment if requested
  if (include_goals) {
    goal_vars = vars[stringr::str_detect(vars, "goal$")]
    if (length(goal_vars) == 0) {
      dat_out = cbind(dat_out, chinook_goal = NA, chum_goal = NA, sockeye_goal = NA)
    } else {
      dat_out = cbind(dat_out, dat_in[,goal_vars])
    }
  }

  # return the formatted output
  return(dat_out)
}
