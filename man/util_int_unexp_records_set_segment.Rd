% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_int_unexp_records_set_segment.R
\name{util_int_unexp_records_set_segment}
\alias{util_int_unexp_records_set_segment}
\title{Check for unexpected data record set}
\usage{
util_int_unexp_records_set_segment(
  level = c("segment"),
  id_vars_list,
  identifier_name_list,
  valid_id_table_list,
  meta_data_record_check_list,
  study_data,
  meta_data
)
}
\arguments{
\item{level}{\link{character} a character vector indicating whether the assessment should be conducted at the study level (level = "dataframe") or at the segment level (level = "segment").}

\item{id_vars_list}{\link{list} the list containing the identifier variables names to be used in the assessment.}

\item{identifier_name_list}{\link{list} the list that contains the name of the identifier to be used in the assessment. For the study level, corresponds to the names of the different data frames. For the segment level, indicates the name of the segments.}

\item{valid_id_table_list}{\link{list} the reference list with the identifier variable values.}

\item{meta_data_record_check_list}{\link{character} a character vector indicating the type of check to conduct, either "subset" or "exact".}

\item{study_data}{\link{data.frame} the data frame that contains the measurements, mandatory.}

\item{meta_data}{\link{data.frame} the data frame that contains metadata attributes of the study data, mandatory.}
}
\value{
a \link{list} with
\itemize{
\item \code{SegmentData}: data frame with the results of the quality check for unexpected data elements
\item \code{SegmentTable}: data frame with selected unexpected data elements check results, used for the data quality report.
\item \code{UnexpectedRecords}: vector with row indices of duplicated records, if any, otherwise NULL.
}
}
\description{
This function tests that the identifiers match a provided record set. It is possible to
check for unexpected data record sets by study segments or to consider only selected
segments.
}
\examples{
\dontrun{
study_data <- readRDS(system.file("extdata", "ship.RDS",
  package = "dataquieR"
))
meta_data <- readRDS(system.file("extdata", "ship_meta.RDS",
  package = "dataquieR"
))
md1_segment <- readRDS(system.file("extdata", "meta_data_segment.RDS",
  package = "dataquieR"
))
ids_segment <- readRDS(system.file("extdata", "meta_data_ids_segment.RDS",
  package = "dataquieR"
))

# TODO: update examples
int_unexp_records_set(
  level = "segment",
  identifier_name_list = c("INTERVIEW", "LABORATORY"),
  valid_id_table_list = ids_segment,
  meta_data_record_check = md1_segment[,
    c("STUDY_SEGMENT", "SEGMENT_RECORD_CHECK")],
  study_data = study_data,
  meta_data = meta_data
)
}

}
\seealso{
Other integrity_indicator_functions: 
\code{\link{util_int_duplicate_content_dataframe}()},
\code{\link{util_int_duplicate_content_segment}()},
\code{\link{util_int_duplicate_ids_dataframe}()},
\code{\link{util_int_duplicate_ids_segment}()},
\code{\link{util_int_unexp_records_set_dataframe}()}
}
\concept{integrity_indicator}
\concept{integrity_indicator_functions}
\keyword{internal}
