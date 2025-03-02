% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acc_distributions_ecdf.R
\name{acc_distributions_ecdf}
\alias{acc_distributions_ecdf}
\title{ECDF plots for distribution checks}
\usage{
acc_distributions_ecdf(
  resp_vars = NULL,
  group_vars = NULL,
  study_data,
  label_col,
  item_level = "item_level",
  meta_data = item_level,
  meta_data_v2,
  n_group_max = getOption("dataquieR.max_group_var_levels_in_plot",
    dataquieR.max_group_var_levels_in_plot_default),
  n_obs_per_group_min = getOption("dataquieR.min_obs_per_group_var_in_plot",
    dataquieR.min_obs_per_group_var_in_plot_default)
)
}
\arguments{
\item{resp_vars}{\link{variable list} the names of the measurement variables}

\item{group_vars}{\link{variable list} the name of the observer, device or
reader variable}

\item{study_data}{\link{data.frame} the data frame that contains the measurements}

\item{label_col}{\link{variable attribute} the name of the column in the metadata
with labels of variables}

\item{item_level}{\link{data.frame} the data frame that contains metadata
attributes of study data}

\item{meta_data}{\link{data.frame} old name for \code{item_level}}

\item{meta_data_v2}{\link{character} path to workbook like metadata file, see
\code{\link{prep_load_workbook_like_file}} for details.
\strong{ALL LOADED DATAFRAMES WILL BE PURGED},
using \code{\link{prep_purge_data_frame_cache}},
if you specify \code{meta_data_v2}.}

\item{n_group_max}{maximum number of categories to be displayed individually
for the grouping variable (\code{group_vars}, devices / examiners)}

\item{n_obs_per_group_min}{minimum number of data points per group to create
a graph for an individual category of the \code{group_vars} variable}
}
\value{
A \link{list} with:
\itemize{
\item \code{SummaryPlotList}: \link{list} of \link[ggplot2:ggplot]{ggplot2::ggplot}s for each response variable in
\code{resp_vars}.
}
}
\description{
Data quality indicator checks "Unexpected location" and "Unexpected
proportion" if a grouping variable is included: Plots of
empirical cumulative distributions for the subgroups.

\link{Descriptor}
}
\seealso{
\href{https://dataquality.qihs.uni-greifswald.de/VIN_acc_impl_distributions.html}{Online Documentation}
}
