#' Create path to a package resource file
#'

resource_path = function(file) {
  system.file(file.path("rstudio", "templates", "project", "resources", file), package = "KuskoHarvEst")
}

#' Rmarkdown output format for PDF reports
#'

pdf_report = function(...) {

  # call the rmarkdown::pdf_document() function with appropriate settings
  rmarkdown::pdf_document(
    latex_engine = "pdflatex",
    template = resource_path(file.path("01-common", "report-template.tex")),
    ...
  )
}
