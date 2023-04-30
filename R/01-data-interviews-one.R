#' Prepare one interview data file
#'
#' Reads in and formats one data file containing interview data
#'   into a standardized format to be used by other functions
#'
#' @inheritParams prepare_interviews
#' @param input_file Character; name of a single file that contains interview data
#' @note This function should always be called by calling the wrapper [prepare_interviews()], not directly.
#'

prepare_interviews_one = function(input_file, include_salmon, include_nonsalmon, include_village, include_goals) {

  ### ERROR CHECKS FOR SPECIES ###

  # small function to ensure acceptable values supplied
  check_spp_args = function(include, accepted, label) {
    has_all = any(include == "all")
    has_none = any(include == "none")
    lgt1 = length(include) > 1

    if (has_all & lgt1) {
      paste0("if ", label, " includes 'all', it cannot include other selections") |>
        stop()
    }
    if (has_none & lgt1)  {
      paste0("if ", label, " includes 'none', it cannot include other selections") |>
        stop()
    }
    if (!all(include %in% c("none", "all", accepted))) {
      paste0(label, " must be one of 'all', 'none', or any combination of ", knitr::combine_words(accepted, and = " or ", before = "'")) |>
        stop()
    }
  }

  # which species names are accepted for salmon/nonsalmon?
  accepted_salmon = species_names$species[species_names$is_salmon]
  accepted_nonsalmon = species_names$species[!species_names$is_salmon]

  # check the validity of supplied argument values
  check_spp_args(include_salmon, accepted_salmon, "include_salmon")
  check_spp_args(include_nonsalmon, accepted_nonsalmon, "include_nonsalmon")

  # if both salmon and nonsalmon are set to 'none', return error
  if (all(include_salmon == "none") & all(include_nonsalmon == "none")) {
    stop ("one of include_salmon or include_nonsalmon must not be 'none'")
  }

  ### STEP 0: load the input data file & format column names
  dat_in = suppressWarnings(read.csv(input_file, stringsAsFactors = FALSE))

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
  src_name = toupper(stringr::str_extract(basename(input_file), "^[A-Z|a-z]+"))

  # if source name not recognized, return an error
  if (!(src_name %in% rownames(source_names))) {
    stop("The data source name (", src_name, ") was not recognized.\nAccepted values are: ", paste(rownames(source_names), collapse = ", "), ".\nPlease change the data file name to match one of these data sources,\nor notify the software developer if a new data source has been added.")
  } else {
    dat_out = data.frame(source = rep(src_name, nrow(dat_in)))
  }

  ### STEP 2: handle the stratum name
  dat_out$stratum = stringr::str_remove(toupper(dat_in$stratum), " ")

  # determine if any records have an unknown stratum (e.g., O). If so, remove them and return warning
  unknown_stratum = !(dat_out$stratum %in% c(strata_names$stratum, NA))
  if (any(unknown_stratum)) {
    warning("There were ", sum(unknown_stratum), " records with invalid stratum values: ", paste(unique(dat_out$stratum[unknown_stratum]), collapse = ", "), "\n  They have been discarded.")
    dat_in = dat_in[!unknown_stratum,]
    dat_out = dat_out[!unknown_stratum,]
  }

  ### STEP 3: handle the gear (net) type
  gear_entered = stringr::str_remove(dat_in$gear, " ")
  gear_standard = tolower(gear_entered) # make lowercase
  gear_standard = stringr::str_remove(gear_standard, "net")
  dat_out$gear = gear_standard

  ### STEP 4: handle net dimensions
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
    dat_out$trip_start = KuskoHarvUtils::combine_datetime(dat_in[,startdate_use], dat_in$trip_start)
  } else {
    dat_out$trip_start = NA
  }
  if (has_endtime) {
    dat_out$trip_end = KuskoHarvUtils::combine_datetime(dat_in[,enddate_use], dat_in$trip_end)
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

  pull_or_create = function(x, v) {
    if (v %in% colnames(x)) {
      return(x[,v])
    } else {
      return(rep(NA, nrow(x)))
    }
  }

  ### STEP 7a: handle catches by salmon species
  if (length(include_salmon) == 1) {
    if (include_salmon == "all") keep_salmon = accepted_salmon
    if (include_salmon == "none") keep_salmon = NULL
    if (include_salmon %in% accepted_salmon) keep_salmon = include_salmon
  } else {
    keep_salmon = include_salmon
  }
  if (all(!is.null(keep_salmon))) {
    salmon_catch = do.call(cbind, lapply(keep_salmon, function(v) pull_or_create(x = dat_in, v = v)))
    colnames(salmon_catch) = keep_salmon
    dat_out = cbind(dat_out, salmon_catch)
  }

  ### STEP 7b: handle catches by nonsalmon species
  if (length(include_nonsalmon) == 1) {
    if (include_nonsalmon == "all") keep_nonsalmon = accepted_nonsalmon
    if (include_nonsalmon == "none") keep_nonsalmon = NULL
    if (include_nonsalmon %in% accepted_nonsalmon) keep_nonsalmon = include_nonsalmon
  } else {
    keep_nonsalmon = include_nonsalmon
  }
  if (all(!is.null(keep_nonsalmon))) {
    nonsalmon_catch = do.call(cbind, lapply(keep_nonsalmon, function(v) pull_or_create(x = dat_in, v = v)))
    colnames(nonsalmon_catch) = keep_nonsalmon
    dat_out = cbind(dat_out, nonsalmon_catch)
  }

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
