% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_recode.R
\name{util_recode}
\alias{util_recode}
\title{Map a vector of values based on an assignment table}
\usage{
util_recode(values, mapping_table, from, to, default = NULL)
}
\arguments{
\item{values}{\link{vector} the vector}

\item{mapping_table}{\link{data.frame} a table with the mapping table}

\item{from}{\link{character} the name of the column with the "old values"}

\item{to}{\link{character} the name of the column with the "new values"}

\item{default}{\link{character} either one character or on character per value,
being used, if an entry from \code{values} is not
in the \code{from} column in `mapping_table}
}
\value{
the mapped values
}
\description{
Map a vector of values based on an assignment table
}
\seealso{
\link[dplyr:recode]{dplyr::recode}

Other mapping: 
\code{\link{util_map_all}()},
\code{\link{util_map_by_largest_prefix}()},
\code{\link{util_map_labels}()}
}
\concept{mapping}
\concept{process}
\keyword{internal}
