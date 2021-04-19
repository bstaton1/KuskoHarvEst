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
    title = 'title: "Kuskokwim River In-season Harvest/Effort Sensitivity Analyses"'
  }

  # make the opener-label setting
  opener_label = paste0('opener-label: "', basic_date(meta$start_date), ' Subsistence Harvest Opportunity"')

  # make the opener-start setting
  opener_start = paste0('opener-start: "', basic_date(meta$start_date), ' ', short_datetime(meta$start_date, include_date = FALSE), '"')

  # make the opener-end setting
  opener_end = paste0('opener-end: "', basic_date(meta$end_date), ' ', short_datetime(meta$end_date, include_date = FALSE), '"')

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
    doc_label = 'doc-label: "In-season Harvest and Effort Estimates"'
  } else {
    doc_label = 'doc-label: "Sensitivity Analyses"'
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
    ds_bound,
    us_bound,
    contact,
    special_action,
    special_action_url,
    news_release_url,
    doc_label,
    draft_watermark,
    editor_options,
    "---\n"
  )

  # build a single string that contains everything
  out = paste(yaml_contents, collapse = "\n")

  # return the output
  return(out)
}

