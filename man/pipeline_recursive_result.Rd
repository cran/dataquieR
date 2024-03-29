% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipeline_recursive_result.R
\name{pipeline_recursive_result}
\alias{pipeline_recursive_result}
\title{Convert a pipeline result data frame to named encapsulated lists}
\usage{
pipeline_recursive_result(
  call_plan_with_results,
  result_groups = setdiff(colnames(call_plan_with_results), c(NA, "results",
    "resp_vars"))
)
}
\arguments{
\item{call_plan_with_results}{\link{data.frame} result from \link{pipeline_vectorized}}

\item{result_groups}{\link{character} arguments to group by}
}
\value{
a list with:
\itemize{
\item first argument's values in \code{result_groups}, each containing second's
argument's values as a similar list recursively
}
}
\description{
This function converts a data frame to a recursive list structure based on
columns selected for grouping
}
\details{
The data frame columns for the arguments of a certain row/computation from
the calling plan translate to levels in the encapsulated list hierarchy.
The order of the levels can be specified in the \code{result_groups} argument.
}
\examples{
\dontrun{
call_plan_with_results <- structure(list(
  resp_vars =
    c(
      "SBP_0", "DBP_0", "VO2_CAPCAT_0",
      "BSG_0"
    ), group_vars = c(
    "USR_BP_0", "USR_BP_0", "USR_VO2_0",
    "USR_BP_0"
  ), co_vars = list("SEX_0", "SEX_0", "SEX_0", "SEX_0")
),
class = "data.frame", row.names = c(
  NA,
  -4L
)
)
call_plan_with_results[["results"]] <-
  list(NA, 2, "Hello", ggplot2::ggplot())
result_groups <-
  colnames(call_plan_with_results)[2:(ncol(call_plan_with_results) - 1)]
pipeline_recursive_result(call_plan_with_results, result_groups)
pipeline_recursive_result(call_plan_with_results, rev(result_groups))
}
}
