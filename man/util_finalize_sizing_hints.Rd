% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_finalize_sizing_hints.R
\name{util_finalize_sizing_hints}
\alias{util_finalize_sizing_hints}
\title{Function that calculated height and width values for \code{script_iframe}}
\usage{
util_finalize_sizing_hints(sizing_hints)
}
\arguments{
\item{sizing_hints}{\link{list} containing information for setting
the size of the \code{iframe}}
}
\value{
a list with figure_type_id, w, and h; sizes are as \code{CSS}, existing
elements are kept, \code{w_in_cm} and \code{h_in_cm} are estimates for the
size in centimeters on a typical computer display (in 2024)
}
\description{
Function that calculated height and width values for \code{script_iframe}
}
\keyword{internal}
