#' Perform Non-Parametric Bootstrapping On Harvest Estimates
#'
#' @export

bootstrap_harvest = function(interview_data, effort_info, gear, n_boot = 1000, stratify_interviews = TRUE, seed = 1234) {

  # set the random seed
  set.seed(seed)

  # loop over bootstrap iterations, randomizing the data, and obtaining all harvest estimates for each
  output = lapply(1:n_boot, function(i) {
    out = estimate_harvest(
      interview_data = interview_data,
      effort_info = effort_info,
      gear = gear,
      randomize = TRUE,
      stratify_interviews = stratify_interviews
    )
    out = cbind(iter = i, out)
    return(out)
  })

  # combine the output of each bootstrapped sample into a data frame
  output = unlist_dfs(output)

  # return the output
  return(output)
}
