% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_dichotomize.R
\name{util_dichotomize}
\alias{util_dichotomize}
\title{utility function to dichotomize variables}
\usage{
util_dichotomize(study_data, meta_data, label_col = VAR_NAMES)
}
\arguments{
\item{study_data}{study data including jump/missing codes as specified in the
code conventions}

\item{meta_data}{metadata as specified in the code conventions}

\item{label_col}{\link{variable attribute} the name of the column in the metadata
with labels of variables}
}
\description{
use the metadata attribute \code{RECODE} (="recode") to
dichotomize the data
}
