% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02-est-stratify-effort.R
\name{stratify_effort}
\alias{stratify_effort}
\title{Apportion total effort estimate to geographic}
\usage{
stratify_effort(flight_data, gear, effort_est)
}
\arguments{
\item{flight_data}{A \code{\link[base:data.frame]{data.frame}} storing the times and counts on each flight, constructed via \code{\link[=prepare_flights]{prepare_flights()}}}

\item{effort_est}{Numeric; the total effort estimate}
}
\description{
Applies a simple rule to stratify the total effort estimate for a gear type
}
\details{
Calculates the average proportion of effort counted via air in each stratum then multiply by the total effort estimate to apportion
}
