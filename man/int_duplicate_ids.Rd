% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/int_duplicate_ids.R
\name{int_duplicate_ids}
\alias{int_duplicate_ids}
\title{Check for duplicated IDs}
\usage{
int_duplicate_ids(level = c("dataframe", "segment"), ...)
}
\arguments{
\item{level}{\link{character} a character vector indicating whether the assessment should be conducted at the study level (level = "dataframe") or at the segment level (level = "segment").}

\item{...}{Depending on \code{level}, passed to either
\link{util_int_duplicate_ids_segment} or
\link{util_int_duplicate_ids_dataframe}}
}
\value{
a \link{list}. Depending on \code{level}, see
\link{util_int_duplicate_ids_segment} or
\link{util_int_duplicate_ids_dataframe} for a description of the outputs.
}
\description{
This function tests for duplicates entries in identifiers. It is possible to
check duplicated identifiers by study segments or to consider only selected
segments.

\link{Indicator}
}
