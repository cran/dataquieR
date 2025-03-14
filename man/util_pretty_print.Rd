% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_pretty_print.R
\name{util_pretty_print}
\alias{util_pretty_print}
\title{Convert single \code{dataquieR} result to an \code{htmltools} compatible object}
\usage{
util_pretty_print(
  dqr,
  nm,
  is_single_var,
  meta_data,
  label_col,
  use_plot_ly,
  dir,
  ...
)
}
\arguments{
\item{dqr}{\link{dataquieR_result} an output (indicator) from \code{dataquieR}}

\item{nm}{\link{character} the name used in the report, the alias name of the
function call plus the variable name}

\item{is_single_var}{\link{logical} we are creating a single variable overview
page or an indicator summary page}

\item{meta_data}{\link{meta_data}  the data frame that contains metadata
attributes of study data}

\item{label_col}{\link{variable attribute} the name of the column in the metadata
with labels of variables}

\item{use_plot_ly}{\link{logical} use \code{plotly}}

\item{dir}{\link{character} output directory for potential \code{iframes}.}

\item{...}{further arguments passed through, if applicable}
}
\value{
\code{htmltools} compatible object with rendered \code{dqr}
}
\description{
Convert single \code{dataquieR} result to an \code{htmltools} compatible object
}
\keyword{internal}
