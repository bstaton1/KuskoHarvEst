#' Automate creation of a YAML header based on pre-defined meta data
#'
#' @param doc_type One of `"estimate_report"` or `"sensitivity_report"`
#' @param draft Logical. Should a draft watermark be printed in the rendered output?

build_yaml = function(doc_type, draft) {

  # read in the meta data file
  meta = readRDS(list.files(pattern = "meta", full.names = TRUE, recursive = TRUE))

  # make the output type setting
  output = "output: KuskoHarvEst::pdf_report"

  # make the document title setting
  if (doc_type == "estimate_report") {
    title = 'title: "Kuskokwim River In-season Harvest and Effort Estimates"'
  } else {
    title = 'title: "Kuskokwim River In-season Harvest/Effort Sensitivity Analysis"'
  }

  # make the opener-label setting
  opener_label = paste0('opener-label: "', basic_date(meta$start_date), ' Subsistence Harvest Opportunity (', ifelse(meta$set_only, 'Set Nets Only)"', 'Drift & Set Nets)"'))

  # make the footer-label setting
  rfooter = stringr::str_replace(opener_label, "^opener-label", "rfooter")
  rfooter = stringr::str_remove(rfooter, "Subsistence Harvest ")

  # make the opener-start setting
  include_date = ifelse(lubridate::date(meta$start_date) == lubridate::date(meta$start_date), FALSE, TRUE)
  opener_start = paste0('opener-start: "', short_datetime(meta$start_date, include_date = include_date), '"')

  # make the opener-end setting
  opener_end = paste0('opener-end: "', short_datetime(meta$end_date, include_date = include_date), '"')

  # make the opener duration setting
  hours_open = round(as.numeric(lubridate::as.duration(lubridate::int_length(lubridate::interval(meta$start_date, meta$end_date))), units = "hours"), 1)
  opener_duration = paste0('opener-duration: "', paste0(hours_open, ' Hours'), '"')

  # make the downstream and upstream boundaries settings
  ds_bound = paste0('ds-bound: "', meta$ds_bound, '"')
  us_bound = paste0('us-bound: "', meta$us_bound, '"')

  # make the contact person(s) setting
  if (!is.na(meta$contact_persons)) {
    contact = paste0('contact: "', meta$contact_persons, '"')
  } else {
    contact = NULL
  }

  # make the special action name setting
  if (!is.na(meta$spact_url)) {
    special_action = paste0('special-action: "', meta$spact_name, '"')
  } else {
    special_action = NULL
  }

  # make the special action URL setting
  if (!is.na(meta$spact_url)) {
    special_action_url = paste0('special-action-url: "', meta$spact_url, '"')
  } else {
    special_action_url = NULL
  }

  # make the special action URL setting
  if (!is.na(meta$spact_news_url)) {
    news_release_url = paste0('news-release-url: "', meta$spact_news_url, '"')
  } else {
    news_release_url = NULL
  }

  # make the doc label setting
  if (doc_type == 'estimate_report') {
    lfooter = 'lfooter: "In-season Harvest and Effort Estimates"'
  } else {
    lfooter = 'lfooter: "Sensitivity Analyses"'
  }

  # make the draft watermark setting
  draft_watermark = paste0('draft-watermark: ', tolower(as.character(draft)))

  # make the editor options setting
  editor_options = "editor_options:\n  chunk_output_type: console"

  # combine all settings into a vector with YAML fences at start and end
  yaml_contents = c(
    "---",
    output,
    title,
    opener_label,
    opener_start,
    opener_end,
    opener_duration,
    ds_bound,
    us_bound,
    contact,
    special_action,
    special_action_url,
    news_release_url,
    lfooter,
    rfooter,
    draft_watermark,
    editor_options,
    "---\n"
  )

  # build a single string that contains everything
  out = paste(yaml_contents, collapse = "\n")

  # return the output
  return(out)
}

#' Automate creation of a Rmd source file for in-season reports
#'

build_estimate_report_Rmd = function(draft = FALSE, do_setnets = TRUE, n_boot = 1000, include_johnson_table = TRUE, include_goal_table = FALSE, include_appendix = FALSE, save_bootstrap = TRUE) {

  # read in the meta data file
  meta_file = list.files(pattern = "meta", full.names = TRUE, recursive = TRUE)
  meta = readRDS(meta_file)

  # determine how many flights were conducted
  flight_file = list.files(pattern = "flight_data", full.names = TRUE, recursive = TRUE)
  n_flights = nrow(readRDS(flight_file))

  # files for global setup and data preparation
  setup_file = resource_path(file.path("01-common", "01-setup.Rmd"))
  data_prep_file = resource_path(file.path("01-common", "02-data-prep.Rmd"))

  # a blank file
  blank_file = resource_path(file.path("01-common", "blank.Rmd"))

  # return warning if its a set net only opener and the johnson table was requested
  # that table is for drift net estimates only, and can't be produced for a set net only opportunity
  if (include_johnson_table & meta$set_only) {
    warning ("The output of johnson_summary_table() was requested, but this is a set net only estimate.\nIt will not be included in the output report.")
  }

  # 1: select the right file to produce data source summaries
  if (!meta$set_only) {
    if (do_setnets) {
      data_sources_file = resource_path(file.path("02-estimate-report", "01a-data-sources_driftset.Rmd"))
    } else {
      data_sources_file = resource_path(file.path("02-estimate-report", "01b-data-sources_driftset_noset.Rmd"))
    }
  } else {
    data_sources_file = resource_path(file.path("02-estimate-report", "01c-data-sources_setonly.Rmd"))
  }

  # 2: select the right file to produce effort estimate summaries
  if (!meta$set_only) {
    if (do_setnets) {
      paste0("02a-effort_driftset_", n_flights, "flight.Rmd")
      effort_file = resource_path(file.path("02-estimate-report", paste0("02a-effort_driftset_", n_flights, "flight.Rmd")))
    } else {
      effort_file = resource_path(file.path("02-estimate-report", paste0("02b-effort_driftset_noset_", n_flights, "flight.Rmd")))
    }
  } else {
    effort_file = resource_path(file.path("02-estimate-report", "02c-effort_setonly.Rmd"))
  }

  # 3: select the right file to produce harvest estimate summaries
  if (!meta$set_only) {
    if (do_setnets) {
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

  # build the YAML header
  yaml_contents = build_yaml("estimate_report", draft)

  # combine the names of the Rmd source files to use
  body_files = c(setup_file, data_prep_file, data_sources_file, effort_file, harvest_file, johnson_file, goal_file, histogram_file, save_boot_file, appendix_file)

  # read in each file and combine into a vector
  body_contents = unlist(lapply(body_files, readLines))

  # paste all content into one vector, with entries separated by new lines
  Rmd_contents = paste(yaml_contents, paste(body_contents, collapse = "\n"), collapse = "\n")

  # replace the n_boot placeholder text with the number supplied
  Rmd_contents = stringr::str_replace(Rmd_contents, "N_BOOT_REPLACE", as.character(n_boot))

  # build the file name
  Rmd_file = paste0("KuskoHarvEst_", file_date(meta$start_date), ".Rmd")

  # write the Rmd source file that is ready to be knitted
  writeLines(Rmd_contents, Rmd_file)

  # return the name of the output file
  return(Rmd_file)
}

#' Automate creation of a Rmd source file for sensitivity analysis reports
#'

build_sensitivity_report_Rmd = function(draft = FALSE, do_setnets = TRUE, n_boot = 1000, include_plots = FALSE) {

  # read in the meta data file
  meta_file = list.files(pattern = "meta", full.names = TRUE, recursive = TRUE)
  meta = readRDS(meta_file)

  # determine how many flights were conducted
  flight_file = list.files(pattern = "flight_data", full.names = TRUE, recursive = TRUE)
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
  if (do_setnets) {
    harvest_file = resource_path(file.path("03-sensitivity-report", "02a-harvest_driftset.Rmd"))
  } else {
    harvest_file = resource_path(file.path("03-sensitivity-report", "02b-harvest_driftset_noset.Rmd"))
  }

  # build the YAML header
  yaml_contents = build_yaml("sensitivity_report", draft)

  # combine the names of the Rmd source files to use
  body_files = c(setup_file, data_prep_file, effort_file, harvest_file)

  # read in each file and combine into a vector
  body_contents = unlist(lapply(body_files, readLines))

  # paste all content into one vector, with entries separated by new lines
  Rmd_contents = paste(yaml_contents, paste(body_contents, collapse = "\n"), collapse = "\n")

  # replace the n_boot placeholder text with the number supplied
  Rmd_contents = stringr::str_replace(Rmd_contents, "N_BOOT_REPLACE", as.character(n_boot))

  # build the file name
  Rmd_file = paste0("sensitivity_", file_date(meta$start_date), ".Rmd")

  # write the Rmd source file that is ready to be knitted
  writeLines(Rmd_contents, Rmd_file)

  # return the name of the output file
  return(Rmd_file)
}
