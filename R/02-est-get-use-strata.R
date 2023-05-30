#' Get a pooling strategy for stratifying interviews
#'
#' Uses a consistent rule to determine which interviews will
#'   be used to inform stratum-specific estimates
#'
#' @inheritParams estimate_harvest
#' @param pooling_threshold Numeric; the minimum number of interviews required to avoid pooling
#' @details If the number of interviews that document fishing trips occurring in a stratum is
#'   less than `pooling_threshold`, then data must be borrowed from a nearby stratum
#'   to inform the stratum-specific estimate. Pooling rules are as follows:
#'     * If stratum D2 has few interviews, data will be borrowed from stratum D1 and stratum C
#'     * If stratum D1 has few interviews, data will be borrowed from stratum C
#'     * If stratum C has few interviews, data will be borrowed from stratum B
#'     * If stratum B has few interviews, data will be borrowed from stratum C
#'     * If stratum A has few interviews, data will be borrowed from stratum B
#'     * Interviews with stratum recorded as `NA` may be used in any stratum
#'

get_use_strata = function(interview_data, pooling_threshold = KuskoHarvEst_opts("pooling_threshold"), gear = "drift") {

  # discard any other gears
  interview_data = interview_data[interview_data$gear == gear,]

  # convert stratum to a factor class, so if some strata are missing records entirely
  # the counts will be zero
  interview_data$stratum = factor(interview_data$stratum, levels = strata_names$stratum)

  # calculate the number of interviews per strata
  strata_counts = table(interview_data$stratum)

  # extract the strata names
  strata_names = names(strata_counts)

  # container object
  use_strata = list()

  # loop through each strata.
  # if there are fewer than "pooling_threshold" interviews, do pooling otherwise don't.
  # the pooling rules are sort of specific: use the strata downstream of the strata in question, unless:
  # in the case of A, there is no downstream stratum. So use B, recognizing that shorter nets are used here
  # in the case of B, stratum A fishers use longer nets so those interviews should be used in B. So use C instead.
  for (s in 1:length(strata_names)) {
    if (strata_counts[s] < pooling_threshold) {
      if (strata_names[s] == "A") use_strata[[s]] = c("A", "B", NA)
      if (strata_names[s] == "B") use_strata[[s]] = c("B", "C", NA)
      if (strata_names[s] == "C") use_strata[[s]] = c("B", "C", NA)
      if (strata_names[s] == "D1") use_strata[[s]] = c("C", "D1", NA)
      if (strata_names[s] == "D2") use_strata[[s]] = c("C", "D1", "D2", NA)
    } else {
      use_strata[[s]] = c(strata_names[s], NA)
    }
  }

  # give the output names
  names(use_strata) = strata_names

  # return the output
  return(use_strata)
}
