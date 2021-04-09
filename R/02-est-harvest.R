#' Estimate harvest for all strata and species
#'
#' @export

estimate_harvest = function(interview_data, effort_info, gear, randomize = FALSE, stratify_interviews = TRUE) {

  # extract the strata names
  strata_names = names(effort_info$effort_est_stratum)

  # define a quick expansion function
  expand = function(catch_per_trip, effort) {
    out = round(catch_per_trip * effort)
    c(out, "total" = sum(out))
  }

  if (!stratify_interviews) {
    # apply the expand() function separately to each stratum, but don't stratify interview data
    ests_all = sapply(strata_names, function(s) {
      expand(catch_per_trip = estimate_catch_per_trip(interview_data = interview_data, gear = gear, randomize = randomize),
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
  # output = cbind(output, total = rowSums(output))

  # format output: give gear/stratum IDs
  output = data.frame(output)
  output = cbind(gear = gear, stratum = rownames(output), output)
  rownames(output) = NULL

  # format the output: add the date the estimate applies to
  output = cbind(date = unique_start_dates(interview_data), output)

  # return the output
  return(output)
}
