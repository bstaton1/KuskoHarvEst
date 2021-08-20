#' Rmarkdown output format for PDF reports
#'
#' A custom output type for PDFs produced by 'KuskoHarvEst'
#'
#' @param ... Optional arguments supplied to [rmarkdown::pdf_document()]
#'
#' @details The `template` argument and `latex_engine` arguments are defined internally and cannot be
#'   supplied to `...`. `latex_engine = "pdflatex"` is used.

pdf_report = function(...) {

  # call the rmarkdown::pdf_document() function with appropriate settings
  rmarkdown::pdf_document(
    latex_engine = "pdflatex",
    template = resource_path(file.path("01-common", "report-template.tex")),
    ...
  )
}
