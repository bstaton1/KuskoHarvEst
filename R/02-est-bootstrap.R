#' Perform Non-Parametric Bootstrapping On Harvest Estimates
#'
#' @export

bootstrap_harvest = function(interview_data, effort_info, gear, n_boot = 1000, source_weights = NULL, stratify_interviews = TRUE) {

  # loop over bootstrap iterations, randomizing the data, and obtaining all harvest estimates for each
  output = lapply(1:n_boot, function(i) {
    out = estimate_harvest_all(
      interview_data = randomize_data(interview_data[interview_data$gear == gear,], source_weights = source_weights),
      effort_info = effort_info,
      gear = gear,
      stratify_interviews = stratify_interviews
    )
    out = cbind(iter = i, out)
    return(out)
  })

  # combine the output of each bootstrapped sample into a data frame
  output = unlist_dfs(output)
}
