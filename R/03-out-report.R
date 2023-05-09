#' Return summarized harvest estimates
#'
#' Summarizes bootstrapped harvest estimates into a point estimate
#'   and confidence limits, by species, gear, and geographic stratum
#'
#' @param spp Character; the species to summarize.
#'   Accepted options are `"chinook"`, `"chum"`, `"sockeye"`, or `"total"`.
#'   Defaults to `"total"`.
#' @param gear Character; the gear type to summarize.
#'   Accepted options are `"drift"`, `"set"`, or `"total"`.
#'   Defaults to `"total"`.
#' @param stratum Character; the geographic stratum to summarize.
#'   Accepted options are `"A"`, `"B"`, `"C"`, `"D1"`, or `"total"`.
#'   Defaults to `"total"`.
#' @param date Character; the date to summarize.
#'   Should be supplied in YYYY-MM-DD format if not `NULL`.
#'   Defaults to `NULL`, in which case the date is ignored when summarizing.
#' @param CI Logical; should the confidence intervals be returned?
#' @param conf_level Numeric; the confidence level of the confidence interval.
#'   E.g., `0.95` corresponds to a 95% confidence interval (the default).
#'   Ignored if `CI = FALSE`
#' @param digits Numeric; the rounding rule (passed to [base::round()]); defaults to `0` (i.e., nearest whole fish)
#' @param return_numeric; Logical; should summary be returned as numeric class rather than character?
#' @param boot_out_use; Data frame; the output of [bootstrap_harvest()] to be summarized.
#'   Defaults to `NULL`, in which case the function searches for an object in existence named `boot_out` which will be used.
#' @note All zero values will be returned if all bootstrapped values to be summarized are `NA`.
#'
#' @export
#'

report = function(spp = "total", gear = "total", stratum = "total", date = NULL, CI = TRUE, conf_level = 0.95, digits = 0, return_numeric = FALSE, boot_out_use = NULL) {

  # error handle
  if (is.null(boot_out_use)) {
    if (!exists("boot_out")) {
      stop ("If 'boot_out_use' is NULL (the default), an object named 'boot_out' must exist in the workspace.")
    } else {
      boot_out_use = boot_out
    }
  }

  # select output from the requested date
  if (!is.null(date)) {
    boot_out_use = boot_out_use[boot_out_use$date == date,]
  }

  # convert to long form
  long_boot = reshape2::melt(boot_out_use,
                             id.vars = c("iter", "gear", "stratum", "date"),
                             variable.name = "species", value.name = "harvest")

  # subset the output to match the input arguments
  long_boot_sub = long_boot[long_boot$gear == gear & long_boot$stratum == stratum & long_boot$species == spp,]

  # return error if no estimates available
  if (nrow(long_boot_sub) == 0) {
    stop ("No bootstrap samples meeting the desired attributes (gear, species, or stratum)")
  }

  # extract only the bootstrapped harvest numbers
  harv = long_boot_sub$harvest

  # calculate the mean
  mean_est = c(mean = round(mean(harv, na.rm = TRUE), digits = digits))

  # calculate the bounds of the interval
  tail_prob = (1 - conf_level)/2
  lwr_est = round(quantile(harv, tail_prob, na.rm = TRUE), digits = digits)
  upr_est = round(quantile(harv, 1 - tail_prob, na.rm = TRUE), digits = digits)

  # build the four types of output a user might request
  num_out = unname(mean_est)
  num_CI_out = c(mean_est, lwr_est, upr_est)
  chr_out = prettyNum(unname(mean_est), ",", scientific = FALSE)
  chr_CI_out = paste0(prettyNum(mean_est, ",", scientific = FALSE), " (",
                      prettyNum(lwr_est, ",", scientific = FALSE), " -- ",
                      prettyNum(upr_est, ",", scientific = FALSE), ")")

  # if requesting a gear with NaN estimates involved, return zeros
  # if values are involved here, that means no estimates were produced, and they should be zero
  if ("NaN" %in% num_out) {
    num_out = 0
    num_CI_out[] = 0
    chr_out = "0"
    chr_CI_out = "0 (0 -- 0)"
  }

  # determine the right output to return
  # based on whether CIs are desired, and whether output is numeric or character
  if (CI & return_numeric) out = num_CI_out
  if (!CI & return_numeric) out = num_out
  if (CI & !return_numeric) out = chr_CI_out
  if (!CI & !return_numeric) out = chr_out

  # return the output
  return(out)
}

#' Summarize Harvest Estimates Bullet List
#'
#' Wrapper around [`report()`] to cleanly return harvest estimates
#' as a markdown list. Automatically selects species to report.
#'
#' @inheritParams estimate_harvest
#' @param include_set_summary Logical; do you wish to return a bullet point reporting the total estimated harvest by set nets as well as the species composition?
#' @export

harvest_bullets = function(include_set_summary, nonsalmon = FALSE) {

  # determine which species will be included
  include_spp = KuskoHarvEst:::species_in_data(boot_out)[[ifelse(nonsalmon, "nonsalmon", "salmon")]]

  # calculate the total harvest by species across all gears; including a total across species
  harv_totals = sapply(c("total", include_spp), function(s) report(spp = s))

  # format the species names
  text_spp = species_names$in_text[species_names$species %in% include_spp]
  names(text_spp) = species_names$species[species_names$species %in% include_spp]
  text_spp = c(text_spp, "total" = ifelse(nonsalmon, "nonsalmon", "salmon"))
  text_spp = text_spp[c("total", include_spp)]

  # build the indentation for bullet points
  indents = c("", rep("  ", length(include_spp)))

  # build the full text
  total_bullets = paste0(indents, "* An estimated total of **", harv_totals, "** ", text_spp, " were harvested")

  # drop the species aggregate summary if only one species present
  if (length(include_spp) == 1) {
    # the first element is always the total (see above), so keep the second one that is labeled
    total_bullets = total_bullets[2] |>
      stringr::str_remove("^  ")
  }

  # build the bullet point reporting on set net harvest if requested
  if (include_set_summary) {

    # the main portion of the bullet, reported regardless of which species present
    set_bullet_main = paste0("* Harvest by set nets accounted for an estimated **", report(gear = "set"), "** total ", ifelse(nonsalmon, "nonsalmon ", "salmon"))

    # get set net harvest by species
    set_harv = sapply(include_spp, function(s) report(spp = s, gear = "set", CI = FALSE, return_numeric = TRUE))

    # get set net total harvest
    set_harv_tot = report(spp = "total", gear = "set", CI = FALSE, return_numeric = TRUE)

    # calculate species comp
    if (set_harv_tot == 0) {
      set_spp_comp = rep(0, length(include_spp))
      names(set_spp_comp) = include_spp
    } else {
      set_spp_comp = set_harv/unname(set_harv_tot)
    }

    # format the species comp
    set_spp_comp = set_spp_comp |>
      KuskoHarvUtils::smart_round(digits = 2) |>
      KuskoHarvUtils::percentize()

    # build the bullet text regarding species composition
    set_bullet_comp = paste0("**", set_spp_comp, "** ", text_spp[-which(names(text_spp) == "total")]) |>
      knitr::combine_words()
    set_bullet_comp = paste0("(", set_bullet_comp, ")")

    # if only one species, don't report the species composition
    if (length(include_spp) > 1) {
      set_bullet = paste0(" ", set_bullet_main, " ", set_bullet_comp)
    } else {
      set_bullet = paste0(" ", set_bullet_main)
    }

    # combine with the total bullets if requested
    out = c(total_bullets, set_bullet)
  } else {
    out = total_bullets
  }

  # report(gear = "set")
  cat(paste0(out, "."), sep = "\n")
}
