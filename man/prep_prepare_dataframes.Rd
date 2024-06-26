% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_prepare_dataframes.R
\name{prep_prepare_dataframes}
\alias{prep_prepare_dataframes}
\title{Prepare and verify study data with metadata}
\usage{
prep_prepare_dataframes(
  .study_data,
  .meta_data,
  .label_col,
  .replace_hard_limits,
  .replace_missings,
  .sm_code = NULL,
  .allow_empty = FALSE,
  .adjust_data_type = TRUE,
  .amend_scale_level = TRUE,
  .internal = rlang::env_inherits(rlang::caller_env(), parent.env(environment()))
)
}
\arguments{
\item{.study_data}{if provided, use this data set as study_data}

\item{.meta_data}{if provided, use this data set as meta_data}

\item{.label_col}{if provided, use this as label_col}

\item{.replace_hard_limits}{replace \code{HARD_LIMIT} violations by \code{NA},
defaults to \code{FALSE}.}

\item{.replace_missings}{replace missing codes, defaults to \code{TRUE}}

\item{.sm_code}{missing code for \code{NAs}, if they have been
re-coded by \code{util_combine_missing_lists}}

\item{.allow_empty}{allow \code{ds1} to be empty, i.e., 0 rows and/or 0 columns}

\item{.adjust_data_type}{ensure that the data type of variables in the study
data corresponds to their data type specified in the metadata}

\item{.amend_scale_level}{ensure that \code{SCALE_LEVEL} is available in the
item-level \code{meta_data}. internally used to prevent
recursion, if called from
\code{\link[=prep_scalelevel_from_data_and_metadata]{prep_scalelevel_from_data_and_metadata()}}.}

\item{.internal}{\link{logical} internally called, modify caller's environment.}
}
\value{
\code{ds1} the study data with mapped column names
}
\description{
This function ensures, that a data frame \code{ds1} with suitable variable
names study_data and meta_data exist as base \link{data.frame}s.
}
\details{
This function defines \code{ds1} and modifies \code{study_data} and \code{meta_data} in the
environment of its caller (see \link{eval.parent}). It also defines or modifies
the object \code{label_col} in the calling environment. Almost all functions
exported by \code{dataquieR} call this function initially, so that aspects common
to all functions live here, e.g. testing, if an argument \code{meta_data} has been
given and features really a \link{data.frame}. It verifies the existence of
required metadata attributes (\link{VARATT_REQUIRE_LEVELS}). It can also replace
missing codes by \code{NA}s, and calls \link{prep_study2meta} to generate a minimum
set of metadata from the study data on the fly (should be amended, so
on-the-fly-calling is not recommended for an instructive use of \code{dataquieR}).

The function also detects \code{tibbles}, which are then converted to base-R
\link{data.frame}s, which are expected by \code{dataquieR}.

Different from the other utility function that work
in the caller's environment, so it modifies objects in the calling function.
It defines a new object \code{ds1}, it modifies \code{study_data} and/or \code{meta_data}
and \code{label_col}, if \code{.internal} is \code{TRUE}.
}
\examples{
\dontrun{
acc_test1 <- function(resp_variable, aux_variable,
                      time_variable, co_variables,
                      group_vars, study_data, meta_data) {
  prep_prepare_dataframes()
  invisible(ds1)
}
acc_test2 <- function(resp_variable, aux_variable,
                      time_variable, co_variables,
                      group_vars, study_data, meta_data, label_col) {
  ds1 <- prep_prepare_dataframes(study_data, meta_data)
  invisible(ds1)
}
environment(acc_test1) <- asNamespace("dataquieR")
# perform this inside the package (not needed for functions that have been
# integrated with the package already)

environment(acc_test2) <- asNamespace("dataquieR")
# perform this inside the package (not needed for functions that have been
# integrated with the package already)
acc_test3 <- function(resp_variable, aux_variable, time_variable,
                      co_variables, group_vars, study_data, meta_data,
                      label_col) {
  prep_prepare_dataframes()
  invisible(ds1)
}
acc_test4 <- function(resp_variable, aux_variable, time_variable,
                      co_variables, group_vars, study_data, meta_data,
                      label_col) {
  ds1 <- prep_prepare_dataframes(study_data, meta_data)
  invisible(ds1)
}
environment(acc_test3) <- asNamespace("dataquieR")
# perform this inside the package (not needed for functions that have been
# integrated with the package already)

environment(acc_test4) <- asNamespace("dataquieR")
# perform this inside the package (not needed for functions that have been
# integrated with the package already)
load(system.file("extdata/meta_data.RData", package = "dataquieR"))
load(system.file("extdata/study_data.RData", package = "dataquieR"))
try(acc_test1())
try(acc_test2())
acc_test1(study_data = study_data)
try(acc_test1(meta_data = meta_data))
try(acc_test2(study_data = 12, meta_data = meta_data))
print(head(acc_test1(study_data = study_data, meta_data = meta_data)))
print(head(acc_test2(study_data = study_data, meta_data = meta_data)))
print(head(acc_test3(study_data = study_data, meta_data = meta_data)))
print(head(acc_test3(study_data = study_data, meta_data = meta_data,
  label_col = LABEL)))
print(head(acc_test4(study_data = study_data, meta_data = meta_data)))
print(head(acc_test4(study_data = study_data, meta_data = meta_data,
  label_col = LABEL)))
try(acc_test2(study_data = NULL, meta_data = meta_data))
}

}
\seealso{
acc_margins
}
