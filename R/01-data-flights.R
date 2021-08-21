#' Prepare flight data file
#'
#' Reads in and formats the data file containing flight counts of active fishing trips
#'   into a standardized format to be used by other functions
#'
#' @param input_file Character; the name of the flight data file
#' @export

prepare_flights = function(input_file) {

  ### STEP 0: read in data file
  dat_in = suppressWarnings(read.csv(input_file, stringsAsFactors = FALSE))

  # determine and delete the rows that have all NA values: Excel/CSV quirk sometimes includes these
  all_NA = sapply(1:nrow(dat_in), function(i) all(is.na(dat_in[i,]) | dat_in[i,] == ""))
  dat_in = dat_in[!all_NA,]

  ### STEP 1: handle dates/times
  dat_out = data.frame(flight = dat_in$flight)
  dat_out$start_time = combine_datetime(dat_in$date, dat_in$start)
  dat_out$end_time = combine_datetime(dat_in$date, dat_in$end)
  dat_out = cbind(dat_out, dat_in[,colnames(dat_in)[-which(colnames(dat_in) %in% c("date", "start", "end", "flight"))]])

  ### STEP 2: ensure gear is formatted properly
  dat_out$gear = stringr::str_remove(tolower(dat_out$gear), " ")
  dat_out$gear = stringr::str_remove(dat_out$gear, "net")

  ### STEP 3: make one row per flight
  # reformat count data: make long
  dat_out = reshape2::melt(dat_out, id.vars = c("flight", "start_time", "end_time", "gear"), variable.name = "stratum", value.name = "count")

  # reformat count data: make wide
  dat_out = reshape2::dcast(dat_out, flight + start_time + end_time ~ stratum + gear, value.var = "count")

  # return the formatted output
  return(dat_out)
}
