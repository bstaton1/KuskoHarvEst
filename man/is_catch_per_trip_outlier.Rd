% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01-data-checks.R
\name{is_catch_per_trip_outlier}
\alias{is_catch_per_trip_outlier}
\title{Determine if an interview is an outlier in terms of catch per trip}
\usage{
is_catch_per_trip_outlier(
  interview_data,
  catch_per_trip_cut = KuskoHarvEst_opts("catch_per_trip_cut")
)
}
\arguments{
\item{interview_data}{Data frame storing interview data; created using \code{\link[=prepare_interviews]{prepare_interviews()}}}

\item{catch_per_trip_cut}{Numeric; the maximum proportional change in average catch per trip allowed
when an interview is discarded for that interview to be considered reliable}
}
\description{
Determine if an interview is an outlier in terms of catch per trip
}
\details{
This function recalculates the average catch per trip (non-stratified)
by leaving out each interview sequentially. It then determines the proportional change
from the original estimate with all data included. If the absolute proportional change when
leaving out an interview is greater than \code{catch_per_trip_cut} the catch rate data from that
interview will be deemed unreliable.
Calculations done only for drift net interviews -- set net interviews are skipped.
}
