#' Estimate harvest
#'
#' Estimates harvest separated by species and geographic stratum that occurred in a day of fishing
#'   for a given gear type
#'
#' @param interview_data Data frame storing interview data; created using [prepare_interviews()]
#' @param effort_info List storing the output of [estimate_effort()]
#' @param gear Character; which gear type to use? Only two options are accepted:
#'   * `gear = "drift"` for drift nets
#'   * `gear = "set"` for set nets
#' @param randomize Logical; should interview data be randomized prior to performing the estimate?
#' @param stratify_interviews Logical; should interview data be separated by geographic stratum before performing the estimate?
#'   Generally this should be `TRUE` for `gear = "drift"` and `FALSE` for `gear = "set"`
#' @export

estimate_harvest = function(interview_data, effort_info, gear, randomize = FALSE, stratify_interviews) {

  # extract the strata names
  strata_names = names(effort_info$effort_est_stratum)

  # define a quick expansion function
  expand = function(catch_per_trip, effort) {
    out = round(catch_per_trip * effort)
    c(out, "total" = sum(out))
  }

  if (!stratify_interviews) {

    # perform the randomization once for all strata before expansion
    interview_data_use = interview_data[interview_data$gear == gear,]
    if(randomize) {
      interview_data_use = randomize_data(interview_data_use)
    }

    # apply the expand() function separately to each stratum, but don't stratify interview data
    ests_all = sapply(strata_names, function(s) {
      expand(catch_per_trip = estimate_catch_per_trip(interview_data = interview_data_use, gear = gear, randomize = FALSE),
             effort = effort_info$effort_est_stratum[s]
      )
    })
  } else {
    # set a pooling strategy
    use_strata = get_use_strata(interview_data = interview_data, gear = gear)

    # apply the expand() function separately to each stratum and stratify interview data
    ests_all = sapply(strata_names, function(s) {
      expand(catch_per_trip = estimate_catch_per_trip(interview_data = interview_data[interview_data$stratum %in% unlist(use_strata[s]),],
                                                      gear = gear, randomize = randomize),
             effort = effort_info$effort_est_stratum[s]
      )
    })
  }

  # format the output: add strata and species totals
  output = t(ests_all)
  output = rbind(output, total = colSums(output))

  # format output: give gear/stratum IDs
  output = data.frame(output)
  output = cbind(gear = gear, stratum = rownames(output), output)
  rownames(output) = NULL

  # format the output: add the date the estimate applies to
  output = cbind(date = unique_start_dates(interview_data), output)

  # return the output
  return(output)
}
