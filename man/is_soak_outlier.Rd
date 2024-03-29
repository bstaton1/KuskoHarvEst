% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01-data-checks.R
\name{is_soak_outlier}
\alias{is_soak_outlier}
\title{Check soak time for outliers}
\usage{
is_soak_outlier(interview_data, soak_sd_cut = KuskoHarvEst_opts("soak_sd_cut"))
}
\arguments{
\item{interview_data}{Data frame storing interview data; created using \code{\link[=prepare_interviews]{prepare_interviews()}}}

\item{soak_sd_cut}{Numeric; maximum number of standard deviations the soak time can be from the mean
without being deemed an outlier}
}
\description{
Check soak time for outliers
}
\details{
Consider soak time an outlier if its value is larger than
the mean plus XSDs of the remaining values for that gear type. The user
chooses the value of X with the argument \code{soak_sd_cut}. This function applies
to completed trips only -- records that are for incomplete trips or that
do not have soak time recorded will have \code{FALSE} returned.
}
