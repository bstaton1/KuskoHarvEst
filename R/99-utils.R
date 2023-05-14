#' Create path to a package resource file
#'
#' Constructs a complete file path to a package resource file located in
#'   `rstudio/templates/project/resources` within the 'KuskoHarvEst' library
#'
#' @param file Character; a file name or file path within the resources folder to point to
#'

resource_path = function(file) {
  system.file(file.path("rstudio", "templates", "project", "resources", file), package = "KuskoHarvEst")
}

#' Create a project directory to use for use with 'KuskoHarvEst'
#'
#' Called by the RStudio project template builder
#'
#' @param path A location to put the new project
#' @param is_for_final_report Logical; is the project for compiling all estimates
#'   into tables and figures for final reporting rather than producing estimates for one day in-season?
#'

KuskoHarvEst_skeleton = function(path, is_for_final_report) {

  # create the project directory
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  if (!is_for_final_report) {
    # create subdirectories
    dir.create(file.path(path, "data-raw"))
  } else {
    # create subdirectories
    dir.create(file.path(path, "raw-data-files"))

    # find the files
    resource_path = resource_path("07-final-report-content")
    files = list.files(resource_path)

    # build full file paths
    source = file.path(resource_path, files)
    target = file.path(path, files)

    # copy the files into the new project
    file.copy(source, target)
  }

  TRUE
}

#' Create a markdown link to a local documentation file
#'
#' @param doc Character; file path to the documentation file in question
#' @param text Character; the text to display as the clickable link
#'

link_to_doc = function(doc, text = "here") {
  paste0('[', text, '](./', doc, '){target="_blank"}')
}


#' Find Which Species are in Data Set
#'
#' Given a data frame of interview data,
#' return the species names separated into
#' salmon and non-salmon categories
#'
#' @inheritParams estimate_harvest
#'

species_in_data = function(interview_data) {

  with(KuskoHarvEst:::species_names, {
    vars = colnames(interview_data)
    spp = vars[vars %in% species]

    list(
      salmon = sort(spp[spp %in% species[is_salmon]]),
      nonsalmon = sort(spp[spp %in% species[!is_salmon]])
    )
  })
}

#' Select Species after Data Processing
#'
#' Given a data frame of interview data,
#' return the entire data frame
#' but including only those selected in
#' a knitr `params` list.
#'
#' @inheritParams estimate_harvest
#' @param knitr_params List; must contain the names of the desired/undesired
#'   species as named elements with logical (`TRUE/FALSE`) values.
#'   Generally provided via the `params` argument to [rmarkdown::render()].
#'

select_species = function(interview_data, knitr_params) {
  # get the names of the species contained in yaml params
  species_choices = unlist(knitr_params[names(knitr_params) %in% KuskoHarvEst:::species_names$species])

  # keep only the species with yaml value of 'true'
  keep_species = names(species_choices)[which(species_choices)]

  # the variable names of all non-catch variables
  non_catch_vars = colnames(interview_data)[!colnames(interview_data) %in% KuskoHarvEst:::species_names$species]

  # keep the non-catch and only the desired catch variables
  interview_data[,colnames(interview_data) %in% c(non_catch_vars, keep_species)]
}
