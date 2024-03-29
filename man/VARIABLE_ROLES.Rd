% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/000_globs.R
\docType{data}
\name{VARIABLE_ROLES}
\alias{VARIABLE_ROLES}
\title{Variable roles can be one of the following:}
\format{
An object of class \code{list} of length 5.
}
\usage{
VARIABLE_ROLES
}
\description{
\itemize{
\item \code{intro} a variable holding consent-data
\item \code{primary} a primary outcome variable
\item \code{secondary} a secondary outcome variable
\item \code{process} a variable describing the measurement process
\item \code{suppress} a variable added on the fly computing sub-reports, i.e., by
\link{dq_report_by} to have all referred variables available,
even if they are not part of the currently processed segment.
But they will only be fully assessed in their real segment's
report.
}
}
\keyword{datasets}
\alias{variable roles}
