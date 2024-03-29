% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04-rmd-build-Rmd.R
\name{build_sensitivity_report_Rmd}
\alias{build_sensitivity_report_Rmd}
\title{Automate creation of a Rmd source file for sensitivity analysis reports}
\usage{
build_sensitivity_report_Rmd(
  do_drift,
  do_set,
  species = c("chinook", "chum", "sockeye"),
  include_plots = FALSE,
  ...
)
}
\arguments{
\item{do_drift}{Logical; should harvest and effort estimates be produced for drift nets?}

\item{do_set}{Logical; should harvest and effort estimates be produced for set nets?}

\item{species}{Character; vector of accepted species (see \code{KuskoHarvEst:::species_names}).}

\item{include_plots}{Logical; should the output of \code{\link[=make_effort_plot]{make_effort_plot()}} be displayed
for each data scenario? This can result in many plots and a long document, so it is \code{FALSE} by default.}

\item{...}{Arguments to be passed to \code{\link[=build_yaml]{build_yaml()}}. Both \code{do_drift = TRUE/FALSE} and \code{do_set = TRUE/FALSE}
are required, the remainder of the arguments have defaults. See \code{\link[=build_yaml]{build_yaml()}}.}
}
\description{
Based on a set of supplied options, builds the Rmarkdown source file to build
a sensitivity analysis report documenting the output of a variety of analyses
that leave out certain data sources to gauge the reliability of the estimate.
}
\details{
This function selects from the many Rmarkdown source scripts found in \verb{inst/rstudio/templates/project/resources/}
(subdirectories: \code{01-common} and \code{03-sensitivity-report} therein) to automate the construction of the report source code.
This was previously a major time bottle neck, since old code had to be repeatedly copied, pasted, and edited depending on
the features of the new case the code needed to be applied to.
}
