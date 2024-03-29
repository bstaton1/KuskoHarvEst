% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03-out-histograms.R
\name{make_histogram}
\alias{make_histogram}
\title{Make a single histogram}
\usage{
make_histogram(interview_data, gear, variable, n_bins = 10)
}
\arguments{
\item{interview_data}{Data frame storing interview data; created using \code{\link[=prepare_interviews]{prepare_interviews()}}}

\item{gear}{Character; which gear type to use? Only two options are accepted:
\itemize{
\item \code{gear = "drift"} for drift nets
\item \code{gear = "set"} for set nets
}}

\item{variable}{a variable to show the distribution for. Accepted options are:
\itemize{
\item \code{"total_salmon"}
\item \code{"chinook"}
\item \code{"chum"}
\item \code{"sockeye"}
\item \code{"chum+sockeye"}
\item \code{"trip_start"}
\item \code{"trip_end"}
\item \code{"soak_duration"}
\item \code{"trip_duration"}
\item \code{"p_chinook"}
}}

\item{n_bins}{Numeric; the number of bars to draw for the histogram}
}
\description{
Histogram shows the distribution of a variable across interviews
}
