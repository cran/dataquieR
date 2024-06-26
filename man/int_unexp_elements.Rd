% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/int_unexp_elements.R
\name{int_unexp_elements}
\alias{int_unexp_elements}
\title{Check for unexpected data element count}
\usage{
int_unexp_elements(identifier_name_list, data_element_count)
}
\arguments{
\item{identifier_name_list}{\link{character} a character vector indicating the name of each study data frame, mandatory.}

\item{data_element_count}{\link{integer}  an integer vector with the number of expected data elements, mandatory.}
}
\value{
a \link{list} with
\itemize{
\item \code{DataframeData}: data frame with the results of the quality check for unexpected data elements
\item \code{DataframeTable}: data frame with selected unexpected data elements check results, used for the data quality report.
}
}
\description{
This function contrasts the expected element number in each study in
the metadata with the actual element number in each study data frame.

\link{Indicator}
}
\examples{
\dontrun{
study_tables <- list(
"sd1" = readRDS(system.file("extdata", "ship_subset1.RDS",
                     package = "dataquieR")),
"sd2" = readRDS(system.file("extdata", "ship_subset2.RDS",
                     package = "dataquieR"))
)

prep_add_data_frames(data_frame_list = study_tables)

int_unexp_elements(
 identifier_name_list = c("sd1", "sd2"),
 data_element_count = c(30, 29)
)
}
}
