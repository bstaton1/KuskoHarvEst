#' Perform non-parametric bootstrapping
#'
#' Repeatedly calls [estimate_harvest()], each time with a data set that has been resampled with replacement
#'
#' @inheritParams estimate_harvest
#' @param n_boot Numeric; the number of bootstrap iterations to perform
#' @param seed Numeric; the seed of the random number generator to use -- enforces reproducibility
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
