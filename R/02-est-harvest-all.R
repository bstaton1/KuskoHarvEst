#' Estimate Harvest for All Strata
#'
#' @export

estimate_harvest_all = function(interview_data, effort_info, gear, stratify_interviews = TRUE, pooling_threshold = 10) {

  # extract the records for this gear type
  interview_data = interview_data[interview_data$gear == gear,]

  # extract the strata names
  strata_names = names(effort_info$effort_est_stratum)

  if (!stratify_interviews) {
    # apply the estimate_harvest() function separately to data from each stratum, but use all interview data
    ests_all = sapply(strata_names, function(s) {
      estimate_harvest(interview_data = interview_data,
                       effort_est = effort_info$effort_est_stratum[s],
                       gear = gear
      )
    })
  } else {
    # set a pooling strategy
    use_strata = get_use_strata(interview_data, pooling_threshold, gear)

    # apply the estimate_harvest() function separately to data from each stratum, but use strata-specific interview data
    ests_all = sapply(strata_names, function(s) {
      estimate_harvest(interview_data = interview_data[interview_data$stratum %in% use_strata[s],],
                       effort_est = effort_info$effort_est_stratum[s],
                       gear = gear
      )
    })
  }

  # format the output: add strata and species totals
  output = t(ests_all)
  output = rbind(output, total = colSums(output))
  output = cbind(output, total = rowSums(output))

  # format output: give gear/stratum IDs
  output = data.frame(output)
  output = cbind(gear = gear, stratum = rownames(output), output)
  rownames(output) = NULL

  # return the output
  return(output)
}
