% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dq_report2.R
\name{dq_report2}
\alias{dq_report2}
\title{Generate a full DQ report, v2}
\usage{
dq_report2(
  study_data,
  meta_data = "item_level",
  label_col = LABEL,
  meta_data_segment = "segment_level",
  meta_data_dataframe = "dataframe_level",
  meta_data_cross_item = "cross-item_level",
  meta_data_v2,
  ...,
  dimensions = c("Completeness", "Consistency"),
  cores = list(mode = "socket", logging = FALSE, cpus = util_detect_cores(),
    load.balancing = TRUE),
  specific_args = list(),
  advanced_options = list(),
  author = prep_get_user_name(),
  title = "Data quality report",
  subtitle = as.character(Sys.Date()),
  user_info = NULL,
  debug_parallel = FALSE,
  resp_vars = character(0),
  filter_indicator_functions = character(0),
  filter_result_slots = c("^Summary", "^Segment", "^DataTypePlotList",
    "^ReportSummaryTable", "^Dataframe", "^Result", "^VariableGroup"),
  mode = c("default", "futures", "queue", "parallel"),
  mode_args = list(),
  notes_from_wrapper = list()
)
}
\arguments{
\item{study_data}{\link{data.frame} the data frame that contains the measurements}

\item{meta_data}{\link{data.frame} the data frame that contains metadata
attributes of study data}

\item{label_col}{\link{variable attribute} the name of the column in the metadata
with labels of variables}

\item{meta_data_segment}{\link{data.frame} -- optional: Segment level metadata}

\item{meta_data_dataframe}{\link{data.frame} -- optional: Data frame level
metadata}

\item{meta_data_cross_item}{\link{data.frame} -- optional: Cross-item level
metadata}

\item{meta_data_v2}{\link{character} path to workbook like metadata file, see
\code{\link{prep_load_workbook_like_file}} for details.
\strong{ALL LOADED DATAFRAMES WILL BE PURGED},
using \code{\link{prep_purge_data_frame_cache}},
if you specify \code{meta_data_v2}.}

\item{...}{arguments to be passed to all called indicator functions if
applicable.}

\item{dimensions}{\link{dimensions} Vector of dimensions to address in the report.
Allowed values in the vector are Completeness, Consistency,
and Accuracy. The generated report will only cover the
listed data quality dimensions. Accuracy is computational
expensive, so this dimension is not enabled by default.
Completeness should be included, if Consistency is
included, and Consistency should be included, if Accuracy
is included to avoid misleading detections of e.g. missing
codes as outliers, please refer to the data quality concept
for more details. Integrity is always included.}

\item{cores}{\link{integer} number of cpu cores to use or a named list with
arguments for \link[parallelMap:parallelStart]{parallelMap::parallelStart} or NULL,
if parallel has already been started by the caller.
Can also be a cluster.}

\item{specific_args}{\link{list} named list of arguments specifically for one of
the called functions, the of the list elements
correspond to the indicator functions whose calls
should be modified. The elements are lists of
arguments.}

\item{advanced_options}{\link{list} options to set during report computation,
see \code{\link[=options]{options()}}}

\item{author}{\link{character} author for the report documents.}

\item{title}{\link{character} optional argument to specify the title for
the data quality report}

\item{subtitle}{\link{character} optional argument to specify a subtitle for
the data quality report}

\item{user_info}{\link{list} additional info stored with the report, e.g.,
comments, title, ...}

\item{debug_parallel}{\link{logical} print blocks currently evaluated in parallel}

\item{resp_vars}{\link{variable list} the name of the measurement variables
for the report. If missing, all variables
will be used. Only item level indicator
functions are filtered, so far.}

\item{filter_indicator_functions}{\link{character} regular expressions, only
if an indicator function's name
matches one of these, it'll
be used for the report. If
of length zero, no filtering
is performed.}

\item{filter_result_slots}{\link{character} regular expressions, only
if an indicator function's
result's name
matches one of these, it'll
be used for the report. If
of length zero, no filtering
is performed.}

\item{mode}{\link{character} work mode for parallel execution. default is
"default", the values mean:
- default: use \code{queue} except \code{cores} has been set explicitly
- futures: use the \code{future} package
- queue: use a queue as described in the examples
from the \code{callr} package by Csárdi and Chang and start
sub-processes as workers that evaluate the queue.
- parallel: use the cluster from \code{cores} to evaluate all
calls of indicator functions using the classic
R \code{parallel} back-ends}

\item{mode_args}{\link{list} of arguments for the selected \code{mode}. As of writing
this manual, only for the mode \code{queue} the argument
\code{step} is supported, which gives the number of
function calls that are run by one worker at a time.
the default is 15, which gives on most of the tested
systems a good balance between synchronization
overhead and idling workers.}

\item{notes_from_wrapper}{\link{list} a list containing notes about changed labels
by \code{dq_report_by} (otherwise NULL)}
}
\value{
a \link{dataquieR_resultset2} that can be
\link[=print.dataquieR_resultset2]{printed} creating a \code{HTML}-report.
}
\description{
Generate a full DQ report, v2
}
\details{
See \link{dq_report_by} for a way to generate stratified or splitted reports
easily.
}
\examples{
\dontrun{
prep_load_workbook_like_file("inst/extdata/meta_data_v2.xlsx")
meta_data <- prep_get_data_frame("item_level")
meta_data_cross <- prep_get_data_frame("cross-item_level")
x <- dq_report2("study_data", dimensions = NULL, label_col = "LABEL")
xx <- pbapply::pblapply(x, util_eval_to_dataquieR_result, env = environment())
xx <- pbapply::pblapply(tail(x), util_eval_to_dataquieR_result, env = environment())
xx <- parallel
cat(vapply(x, deparse1, FUN.VALUE = character(1)), sep = "\n", file = "all_calls.txt")
rstudioapi::navigateToFile("all_calls.txt")
eval(x$`acc_multivariate_outlier.Blood pressure checks`)
}
}
\seealso{
\itemize{
\item \link{as.data.frame.dataquieR_resultset}
\item \link{as.list.dataquieR_resultset}
\item \link{print.dataquieR_resultset}
\item \link{summary.dataquieR_resultset}
\item \link{dq_report_by}
}
}
