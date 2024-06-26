% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_int_duplicate_ids_dataframe.R
\name{util_int_duplicate_ids_dataframe}
\alias{util_int_duplicate_ids_dataframe}
\title{Check for duplicated IDs}
\usage{
util_int_duplicate_ids_dataframe(
  level = c("dataframe"),
  id_vars_list,
  identifier_name_list,
  repetitions,
  meta_data = NULL
)
}
\arguments{
\item{level}{\link{character} a character vector indicating whether the assessment should be conducted at the study level (level = "dataframe") or at the segment level (level = "segment").}

\item{id_vars_list}{\link{list} id variable names for each segment or data frame}

\item{identifier_name_list}{\link{vector} the segments or data frame names being assessed}

\item{repetitions}{\link{vector} an integer vector indicating the number of allowed repetitions in the id_vars. Currently, no repetitions are supported. # TODO}

\item{meta_data}{\link{data.frame} the data frame that contains metadata attributes of the study data, mandatory.}
}
\value{
a \link{list} with
\itemize{
\item \code{DataframeData}: data frame with the results of the quality check for duplicated identifiers
\item \code{DataframeTable}: data frame with selected duplicated identifiers check results, used for the data quality report.
\item \code{Duplicates}: vector with row indices of duplicated identifiers, if any, otherwise NULL.
}
}
\description{
This function tests for duplicates entries in identifiers. It is possible to
check duplicated identifiers by study segments or to consider only selected
segments.
}
\seealso{
Other integrity_indicator_functions: 
\code{\link{util_int_duplicate_content_dataframe}()},
\code{\link{util_int_duplicate_content_segment}()},
\code{\link{util_int_duplicate_ids_segment}()},
\code{\link{util_int_unexp_records_set_dataframe}()},
\code{\link{util_int_unexp_records_set_segment}()}
}
\concept{integrity_indicator}
\concept{integrity_indicator_functions}
\keyword{internal}
