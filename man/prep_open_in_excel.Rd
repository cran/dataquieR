% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_open_in_excel.R
\name{prep_open_in_excel}
\alias{prep_open_in_excel}
\title{Open a data frame in Excel}
\usage{
prep_open_in_excel(dfr)
}
\arguments{
\item{dfr}{the data frame}
}
\value{
potentially modified data frame after dialog was closed
}
\description{
Open a data frame in Excel
}
\details{
if the file cannot be read on function exit, NULL will be returned
}
