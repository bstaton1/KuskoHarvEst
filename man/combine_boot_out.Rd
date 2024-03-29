% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02-est-bootstrap.R
\name{combine_boot_out}
\alias{combine_boot_out}
\title{Combine Bootstrap Harvest Estimates Across Gear Types}
\usage{
combine_boot_out(boot_out_drift = NULL, boot_out_set = NULL)
}
\arguments{
\item{boot_out_drift}{Output from running \code{\link[=bootstrap_harvest]{bootstrap_harvest()}} with \code{gear = "drift"}.
Leave to default (\code{NULL}) if no drift net harvest estimate made}

\item{boot_out_set}{Output from running \code{\link[=bootstrap_harvest]{bootstrap_harvest()}} with \code{gear = "set"}
Leave to default (\code{NULL}) if no set net harvest estimate made}
}
\description{
Accepts drift and set net bootstrap output from \code{\link[=bootstrap_harvest]{bootstrap_harvest()}}
And returns a combined data frame including a total across gears.
}
\details{
If no estimates are available for a given gear type, then placeholder \code{NA} values will be
created in the output.
}
