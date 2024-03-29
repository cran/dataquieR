% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_rbind.R
\name{util_rbind}
\alias{util_rbind}
\title{Bind data frames row-based}
\usage{
util_rbind(..., data_frames_list = list())
}
\arguments{
\item{...}{\link{data.frame} none more more data frames}

\item{data_frames_list}{\link{list} optional, a list of data frames}
}
\value{
\link{data.frame} all data frames appended
}
\description{
if not all data frames share  all columns, missing columns will be filled with
\code{NA}s.
}
\examples{
\dontrun{
util_rbind(head(cars), head(iris))
util_rbind(head(cars), tail(cars))
util_rbind(head(cars)[, "dist", FALSE], tail(cars)[, "speed", FALSE])
}
}
