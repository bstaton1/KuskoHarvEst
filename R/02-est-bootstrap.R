#' Perform non-parametric bootstrapping
#'
#' Repeatedly calls [estimate_harvest()], each time with a data set that has been resampled with replacement
#'
#' @inheritParams estimate_harvest
#' @param n_boot Numeric; the number of bootstrap iterations to perform
#' @param seed Numeric; the seed of the random number generator to use -- enforces reproducibility
#' @param nonsalmon Logical; should estimates be returned for whitefish and sheefish rather than for Chinook, chum, and sockeye salmon?

#' @export

bootstrap_harvest = function(interview_data, effort_info, gear, n_boot = 1000, stratify_interviews = TRUE, nonsalmon = FALSE, seed = 1234) {

  # set the random seed
  set.seed(seed)

  # loop over bootstrap iterations, randomizing the data, and obtaining all harvest estimates for each
  output = lapply(1:n_boot, function(i) {
    out = estimate_harvest(
      interview_data = interview_data,
      effort_info = effort_info,
      gear = gear,
      randomize = TRUE,
      stratify_interviews = stratify_interviews,
      nonsalmon = nonsalmon
    )
    out = cbind(iter = i, out)
    return(out)
  })

  # combine the output of each bootstrapped sample into a data frame
  output = do.call(rbind, output)

  # return the output
  return(output)
}

#' Combine Bootstrap Harvest Estimates Across Gear Types
#'
#' Accepts drift and set net bootstrap output from [bootstrap_harvest()]
#' And returns a combined data frame including a total across gears.
#'
#' @param boot_out_drift Output from running [bootstrap_harvest()] with `gear = "drift"`.
#'   Leave to default (`NULL`) if no drift net harvest estimate made
#' @param boot_out_set Output from running [bootstrap_harvest()] with `gear = "set"`
#'   Leave to default (`NULL`) if no set net harvest estimate made
#' @details If no estimates are available for a given gear type, then placeholder `NA` values will be
#'   created in the output.
#' @export

combine_boot_out = function(boot_out_drift = NULL, boot_out_set = NULL) {

  # create shortcut variables
  no_drift = is.null(boot_out_drift)
  no_set = is.null(boot_out_set)

  # if both inputs are null, return error
  if (no_drift & no_set) {
    stop ("At least one of 'boot_out_drift' or 'boot_out_set' must contain the output of KuskoHarvEst::bootstrap_harvest()")
  }

  # which columns store ID variables
  id_vars = c("iter", "date", "gear", "stratum")

  # placeholder object for if no drift estimates
  # duplicate the set net output and convert estimates to 0
  if (no_drift) {
    boot_out_drift = boot_out_set
    boot_out_drift[,!(colnames(boot_out_drift) %in% id_vars)] = 0
    boot_out_drift$gear = "drift"
  }

  # placeholder object for if no set estimates
  # duplicate the drift net output and convert estimates to 0
  if (no_set) {
    boot_out_set = boot_out_drift
    boot_out_set[,!(colnames(boot_out_set) %in% id_vars)] = 0
    boot_out_set$gear = "set"
  }

  # extract species names
  spp = colnames(boot_out_drift)[!(colnames(boot_out_drift) %in% id_vars)]

  # obtain a total across gears
  boot_out_total = cbind(
    boot_out_drift[,id_vars],
    sapply(spp, function(s) rowSums(cbind(boot_out_drift[,s], boot_out_set[,s]), na.rm = FALSE))
  )
  boot_out_total$gear = "total"

  # combine gear types
  rbind(boot_out_total, boot_out_drift, boot_out_set)

}
