#' Return summarized harvest estimates
#'
#' @export

report = function(spp = "total", gear = "total", stratum = "total", CI = TRUE, conf_level = 0.95, digits = -1, return_numeric = FALSE, boot_out_use = NULL) {

  # error handle
  if (is.null(boot_out_use)) {
    if (!exists("boot_out")) {
      stop ("If 'boot_out_use' is NULL (the default), an object named 'boot_out' must exist in the workspace.")
    } else {
      boot_out_use = boot_out
    }
  }

  # convert to long form
  long_boot = reshape2::melt(boot_out_use,
                             id.vars = c("iter", "gear", "stratum"),
                             variable.name = "species", value.name = "harvest")

  # subset the output to match the input arguments
  long_boot_sub = long_boot[long_boot$gear == gear & long_boot$stratum == stratum & long_boot$species == spp,]

  # extract only the bootstrapped harvest numbers
  harv = long_boot_sub$harvest

  # calculate the mean
  mean_est = c(mean = round(mean(harv), digits = digits))

  # calculate the bounds of the interval
  tail_prob = (1 - conf_level)/2
  lwr_est = round(quantile(harv, tail_prob), digits = digits)
  upr_est = round(quantile(harv, 1 - tail_prob), digits = digits)

  # build the four types of output a user might request
  num_out = unname(mean_est)
  num_CI_out = c(mean_est, lwr_est, upr_est)
  chr_out = prettyNum(unname(mean_est), ",", scientific = FALSE)
  chr_CI_out = paste0(prettyNum(mean_est, ",", scientific = FALSE), " (",
                      prettyNum(lwr_est, ",", scientific = FALSE), " -- ",
                      prettyNum(upr_est, ",", scientific = FALSE), ")")

  # determine the right output to return
  # based on whether CIs are desired, and whether output is numeric or character
  if (CI & return_numeric) out = num_CI_out
  if (!CI & return_numeric) out = num_out
  if (CI & !return_numeric) out = chr_CI_out
  if (!CI & !return_numeric) out = chr_out

  # return the output
  return(out)
}