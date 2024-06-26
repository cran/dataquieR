% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reflection.R
\docType{data}
\name{Indicator}
\alias{Indicator}
\title{Indicator Function}
\format{
An object of class \code{character} of length 1.
}
\usage{
Indicator
}
\description{
A function that returns some value that correlates with the magnitude of
a certain class of data quality problems. Typically, in \code{dataquieR}, such
functions return a \code{SummaryTable} that features columns with names, that
start with a short abbreviation that describes the specific semantics of
the value (e.g., \code{PCT} for a percentage or \code{COR} for a correlation) and
the public name of the indicator according to the data quality concept
\code{DQ_OBS}, e.g., \code{com_qum_nonresp} for item-non-response-rate. A name could
therefore be \code{PCT_com_qum_nonresp}.

The object \code{Indicator} only contains the name used internally to tag
such functions.
}
\seealso{
\link{Descriptor}
}
\keyword{internal}
