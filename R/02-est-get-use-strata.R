#' Create stratum pooling strategy for interviews
#'
#' @export

get_use_strata = function(interview_data, pooling_threshold = 10, gear = "drift") {

  # discard any other gears
  interview_data = interview_data[interview_data$gear == gear,]

  # calculate the number of interviews per strata
  strata_counts = table(interview_data$stratum)

  # extract the strata names
  strata_names = names(strata_counts)

  # container object
  use_strata = list()

  # loop through each strata.
  # if there are fewer than "pooling_threshold" interviews, do pooling otherwise don't.
  # the pooling rules are sort of specific: use the strata downstream of the strata in question, unless:
  # in the case of A, there is no downstream stratum. So use B, recoginizing that shorter nets are used here
  # in the case of B, stratum A fishers use longer nets so those interviews should be used in B. So use C instead.
  for (s in 1:length(strata_names)) {
    if (strata_counts[s] < pooling_threshold) {
      if (strata_names[s] == "A") use_strata[[s]] = c("A", "B", NA)
      if (strata_names[s] == "B") use_strata[[s]] = c("B", "C", NA)
      if (strata_names[s] == "C") use_strata[[s]] = c("B", "C", NA)
      if (strata_names[s] == "D1") use_strata[[s]] = c("C", "D1", NA)
    } else {
      use_strata[[s]] = c(strata_names[s], NA)
    }
  }

  # give the output names
  names(use_strata) = strata_names

  # return the output
  return(use_strata)
}
