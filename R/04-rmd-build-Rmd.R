#' Automate creation of a YAML header based on pre-defined meta data
#'
#' Prevents needing to edit the YAML header by hand
#'
#' @param doc_type Character; accepted options are `"estimate_report"` or `"sensitivity_report"`.
#' @param draft Logical; should a draft watermark be printed in the rendered output?
#' @param species Character; vector of accepted species (see `KuskoHarvEst:::species_names`).
#' @param n_boot Numeric; how many bootstrap iterations should be performed?
#' @param do_drift Logical; should harvest and effort estimates be produced for drift nets?
#' @param do_set Logical; should harvest and effort estimates be produced for set nets?
#' @param split_chum_sockeye Logical; if both chum and sockeye salmon data are available, do you wish to
#'   present their summaries disaggregated by species (`TRUE`, default), or aggregated together as chum+sockeye (`FALSE`)?
#' @export

build_yaml = function(doc_type = "estimate_report", draft = FALSE,
                      do_drift, do_set, species = c("chinook", "chum", "sockeye"),
                      n_boot = 1000,
                      split_chum_sockeye = TRUE
) {

  # read in the meta data file
  meta_file = list.files(pattern = "meta\\.rds", full.names = TRUE, recursive = TRUE)
  meta = readRDS(meta_file)

  # if either do_set or do_drift wasn't specified, stop
  if (missing(do_set) | missing(do_drift)) {
    stop ("Both do_set and do_drift must be specified")
  }

  # function needed to treat logicals properly
  logical_handler = function(x) {
    result = ifelse(x, "true", "false")
    class(result) = "verbatim"
    return(result)
  }

  # create the R code to point to the graphics directory
  graphics_path = paste0('`r KuskoHarvEst:::resource_path("06-logos")`', "/")
  class(graphics_path) = "verbatim"

  # create a list with yaml key mappings (i.e., settings for Rmarkdown and the LaTeX template)
  yaml_in = list(
    output = "KuskoHarvEst:::pdf_report",
    editor_options = list(chunk_output_type = "console"),
    title = ifelse(doc_type == "estimate_report", "Kuskokwim River In-season Harvest and Effort Estimates", "Kuskokwim River In-season Harvest/Effort Sensitivity Analysis"),
    "opener-label" = paste0(KuskoHarvUtils::basic_date(meta$start_date), " Subsistence Harvest Opportunity (", ifelse(meta$set_only, "Set Nets Only)", "Drift & Set Nets)")),
    rfooter = paste0(KuskoHarvUtils::basic_date(meta$start_date), " Opportunity (", ifelse(meta$set_only, "Set Nets Only)", "Drift & Set Nets)")),
    "opener-start" = KuskoHarvUtils::short_datetime(meta$start_date, include_date = lubridate::date(meta$start_date) != lubridate::date(meta$end_date)),
    "opener-end" = KuskoHarvUtils::short_datetime(meta$end_date, include_date = lubridate::date(meta$start_date) != lubridate::date(meta$end_date)),
    "opener-duration" = paste0(round(as.numeric(lubridate::as.duration(lubridate::int_length(lubridate::interval(meta$start_date, meta$end_date))), units = "hours"), 1), " Hours"),
    "ds-bound" = meta$ds_bound,
    "us-bound" = meta$us_bound,
    lfooter = ifelse(doc_type == "estimate_report", "In-season Harvest and Effort Estimates", "Sensitivity Analyses"),
    "draft-watermark" = draft,
    "graphics-path" = graphics_path
  )

  # build the list passed to the params key
  all_species = species_names$species
  params_list = lapply(1:length(all_species), function(i) list(value = all_species[i] %in% species))
  names(params_list) = all_species
  params_list = c(params_list,
                  list(do_drift = list(value = do_drift)),
                  list(do_set = list(value = do_set)),
                  list(split_chum_sockeye = list(value = split_chum_sockeye)),
                  list(n_boot = list(value = as.integer(n_boot)))
  )

  # combine params with the other settings
  yaml_in = c(yaml_in, params = list(params_list))

  # append the contact person(s) setting if found in meta
  if (!is.na(meta$contact_persons)) yaml_in = append(yaml_in, list(contact = meta$contact_persons))

  # append the announcement number (ID) setting if found in meta
  if (!is.na(meta$announce_name)) yaml_in = append(yaml_in, list(announcement = meta$announce_name))

  # append the announcement URL setting if found in meta
  if (!is.na(meta$announce_url)) yaml_in = append(yaml_in, list("announcement-url" = meta$announce_url))

  # append the news release URL setting if found in meta
  if (!is.na(meta$announce_news_url)) yaml_in = append(yaml_in, list("news-release-url" = meta$announce_news_url))

  # convert the list into yaml
  yaml_out = yaml::as.yaml(yaml_in, indent.mapping.sequence = TRUE, handlers = list(logical = logical_handler))

  # add yaml fences
  yaml_out = paste("---\n", yaml_out, "---\n", sep = "")

  # return the converted yaml header
  return(yaml_out)
}

#' Automate creation of a Rmd source file for in-season reports
#'
#' Based on a set of supplied options, builds the Rmarkdown source file to build
#'  an in-season report documenting key output from the sampling and estimation
#'  for a single day of fishing
#' @inheritParams build_yaml
#' @param include_johnson_table Logical; should the output of [make_johnson_summary_table()] be included?
#' @param include_goal_table Logical; should the output of [make_goals_summary_table()] be included?
#' @param include_appendix Logical; should the many tables each produced by [make_appendix_table()] be included?
#' @param save_bootstrap Logical; should a code chunk be included that saves a file containing the bootstrap samples of harvest?
#' @param ... Arguments to be passed to [build_yaml()]. Both `do_drift = TRUE/FALSE` and `do_set = TRUE/FALSE`
#'   are required, the remainder of the arguments have defaults. See [build_yaml()].
#' @details This function selects from the many Rmarkdown source scripts found in `inst/rstudio/templates/project/resources/`
#'   (subdirectories: `01-common` and `02-estimate-report` therein) to automate the construction of the report source code.
#'   This was previously a major time bottle neck, since old code had to be repeatedly copied, pasted, and edited depending on
#'   the features of the new case the code needed to be applied to.
#' @export

build_estimate_report_Rmd = function(do_drift, do_set, species = c("chinook", "chum", "sockeye"),
                                     include_johnson_table = TRUE,
                                     include_goal_table = FALSE,
                                     include_appendix = FALSE,
                                     save_bootstrap = TRUE, ...
                                     ) {

  # read in the meta data file
  meta_file = list.files(pattern = "meta\\.rds", full.names = TRUE, recursive = TRUE)
  meta = readRDS(meta_file)

  # determine how many flights were conducted
  flight_file = list.files(pattern = "flight_data\\.rds", full.names = TRUE, recursive = TRUE)
  n_flights = nrow(readRDS(flight_file))

  # files for global setup and data preparation
  setup_file = resource_path(file.path("01-common", "01-setup.Rmd"))
  data_prep_file = resource_path(file.path("01-common", "02-data-prep.Rmd"))

  # a blank file
  blank_file = resource_path(file.path("01-common", "blank.Rmd"))

  # return warning if its a set net only opener and the johnson table was requested
  # that table is for drift net estimates only, and can't be produced for a set net only opportunity
  if (include_johnson_table & meta$set_only) {
    warning ("The output of make_johnson_summary_table() was requested, but this is a set net only estimate.\nIt will not be included in the output report.")
  }

  # 1: select the right file to produce data source summaries
  if (!meta$set_only) {
    if (do_set) {
      data_sources_file = resource_path(file.path("02-estimate-report", "01a-data-sources_driftset.Rmd"))
    } else {
      data_sources_file = resource_path(file.path("02-estimate-report", "01b-data-sources_driftset_noset.Rmd"))
    }
  } else {
    data_sources_file = resource_path(file.path("02-estimate-report", "01c-data-sources_setonly.Rmd"))
  }

  # 2: select the right file to produce effort estimate summaries
  if (!meta$set_only) {
    if (do_set) {
      effort_file = resource_path(file.path("02-estimate-report", paste0("02a-effort_driftset_", n_flights, "flight.Rmd")))
    } else {
      effort_file = resource_path(file.path("02-estimate-report", paste0("02b-effort_driftset_noset_", n_flights, "flight.Rmd")))
    }
  } else {
    effort_file = resource_path(file.path("02-estimate-report", "02c-effort_setonly.Rmd"))
  }

  # 3: select the right file to produce harvest estimate summaries
  if (!meta$set_only) {
    if (do_set) {
      harvest_file = resource_path(file.path("02-estimate-report", "03a-harvest_driftset.Rmd"))
    } else {
      harvest_file = resource_path(file.path("02-estimate-report", "03b-harvest_driftset_noset.Rmd"))
    }
  } else {
    harvest_file = resource_path(file.path("02-estimate-report", "03c-harvest_setonly.Rmd"))
  }

  # 4: select the right file to use for the place where the johnson summary table should go if requested
  if (include_johnson_table & !meta$set_only) {
    johnson_file = resource_path(file.path("02-estimate-report", "04-johnson-table.Rmd"))
  } else {
    johnson_file = blank_file
  }

  # 5: select the right file to use for the place where the goals summary table should go if requested
  if (include_goal_table) {
    goal_file = resource_path(file.path("02-estimate-report", "05-goal-table.Rmd"))
  } else {
    goal_file = blank_file
  }

  # 6: select the right file to use for the histograms
  if (!meta$set_only) {
    histogram_file = resource_path(file.path("02-estimate-report", "06a-histograms_drift.Rmd"))
  } else {
    histogram_file = resource_path(file.path("02-estimate-report", "06b-histograms_set.Rmd"))
  }

  # 7: select the right file to use for saving the bootstrapped output file
  if (save_bootstrap) {
    save_boot_file = resource_path(file.path("02-estimate-report", "07-save-boot.Rmd"))
  } else {
    save_boot_file = blank_file
  }

  # 8: select the right file to use for the appendix
  if (include_appendix) {
    if (!meta$set_only) {
      appendix_file = resource_path(file.path("02-estimate-report", "08a-appendix_drift.Rmd"))
    } else {
      appendix_file = resource_path(file.path("02-estimate-report", "08b-appendix_set.Rmd"))
    }
  } else {
    appendix_file = blank_file
  }

  # 9: select the right file to use for the nonsalmon appendix
  if (any(species_names$species[!species_names$is_salmon] %in% species)) {
    if (!meta$set_only) {
      if (do_set) {
        nonsalmon_file = resource_path(file.path("02-estimate-report", "09a-nonsalmon_driftset.Rmd"))
      } else {
        nonsalmon_file = resource_path(file.path("02-estimate-report", "09b-nonsalmon_driftset_noset.Rmd"))
      }
    } else {
      nonsalmon_file = resource_path(file.path("02-estimate-report", "09c-nonsalmon_setonly.Rmd"))
    }
  } else {
    nonsalmon_file = blank_file
  }

  # build the YAML header
  yaml_contents = build_yaml(doc_type = "estimate_report", species = species, do_drift = do_drift, do_set = do_set, ...)

  # combine the names of the Rmd source files to use
  body_files = c(setup_file, data_prep_file, data_sources_file, effort_file, harvest_file, johnson_file, goal_file, histogram_file, save_boot_file, appendix_file, nonsalmon_file)

  # read in each file and combine into a vector
  body_contents = unlist(lapply(body_files, readLines))

  # paste all content into one vector, with entries separated by new lines
  Rmd_contents = paste(yaml_contents, paste(body_contents, collapse = "\n"), collapse = "\n")

  # build the file name
  Rmd_file = paste0("KuskoHarvEst_", KuskoHarvUtils::file_date(meta$start_date), ".Rmd")

  # write the Rmd source file that is ready to be knitted
  writeLines(Rmd_contents, Rmd_file)

  # return the name of the output file
  return(Rmd_file)
}

#' Automate creation of a Rmd source file for sensitivity analysis reports
#'
#' Based on a set of supplied options, builds the Rmarkdown source file to build
#'  a sensitivity analysis report documenting the output of a variety of analyses
#'  that leave out certain data sources to gauge the reliability of the estimate.
#'
#' @inheritParams build_yaml
#' @param include_plots Logical; should the output of [make_effort_plot()] be displayed
#'   for each data scenario? This can result in many plots and a long document, so it is `FALSE` by default.
#' @details This function selects from the many Rmarkdown source scripts found in `inst/rstudio/templates/project/resources/`
#'   (subdirectories: `01-common` and `03-sensitivity-report` therein) to automate the construction of the report source code.
#'   This was previously a major time bottle neck, since old code had to be repeatedly copied, pasted, and edited depending on
#'   the features of the new case the code needed to be applied to.
#' @param ... Arguments to be passed to [build_yaml()]. Both `do_drift = TRUE/FALSE` and `do_set = TRUE/FALSE`
#'   are required, the remainder of the arguments have defaults. See [build_yaml()].
#' @export

build_sensitivity_report_Rmd = function(do_drift, do_set,
                                        species = c("chinook", "chum", "sockeye"),
                                        include_plots = FALSE, ...) {

  # read in the meta data file
  meta_file = list.files(pattern = "meta\\.rds", full.names = TRUE, recursive = TRUE)
  meta = readRDS(meta_file)

  # determine how many flights were conducted
  flight_file = list.files(pattern = "flight_data\\.rds", full.names = TRUE, recursive = TRUE)
  n_flights = nrow(readRDS(flight_file))

  # files for global setup and data preparation
  setup_file = resource_path(file.path("01-common", "01-setup.Rmd"))
  data_prep_file = resource_path(file.path("01-common", "02-data-prep.Rmd"))

  # return an error if the opener is a set net only opener -- sensitivity analyses aren't conducted
  if (meta$set_only) {
    stop ("This is a set net only estimate, so the sensitivity analyses are not applicable.")
  }

  # 1: select the right file to produce effort sensitivity analyses
  if (n_flights == 1) {
    if (!include_plots) {
      effort_file = resource_path(file.path("03-sensitivity-report", "01a-effort_1flight_noplots.Rmd"))
    } else {
      effort_file = resource_path(file.path("03-sensitivity-report", "01b-effort_1flight_plots.Rmd"))
    }
  } else {
    if (!include_plots) {
      effort_file = resource_path(file.path("03-sensitivity-report", "01c-effort_multiflight_noplots.Rmd"))
    } else {
      effort_file = resource_path(file.path("03-sensitivity-report", "01d-effort_multiflight_plots.Rmd"))
    }
  }

  # 2: select the right file to produce effort sensitivity analyses
  if (do_set) {
    harvest_file = resource_path(file.path("03-sensitivity-report", "02a-harvest_driftset.Rmd"))
  } else {
    harvest_file = resource_path(file.path("03-sensitivity-report", "02b-harvest_driftset_noset.Rmd"))
  }

  # build the YAML header
  yaml_contents = build_yaml(doc_type = "sensitivity_report", do_drift = do_drift, do_set = do_set, species = species, ...)

  # combine the names of the Rmd source files to use
  body_files = c(setup_file, data_prep_file, effort_file, harvest_file)

  # read in each file and combine into a vector
  body_contents = unlist(lapply(body_files, readLines))

  # paste all content into one vector, with entries separated by new lines
  Rmd_contents = paste(yaml_contents, paste(body_contents, collapse = "\n"), collapse = "\n")

  # build the file name
  Rmd_file = paste0("sensitivity_", KuskoHarvUtils::file_date(meta$start_date), ".Rmd")

  # write the Rmd source file that is ready to be knitted
  writeLines(Rmd_contents, Rmd_file)

  # return the name of the output file
  return(Rmd_file)
}
