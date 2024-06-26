% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acc_multivariate_outlier.R
\name{acc_multivariate_outlier}
\alias{acc_multivariate_outlier}
\title{Calculate and plot Mahalanobis distances}
\usage{
acc_multivariate_outlier(
  variable_group = NULL,
  id_vars = NULL,
  label_col,
  n_rules = 4,
  max_non_outliers_plot = 10000,
  criteria = c("tukey", "3sd", "hubert", "sigmagap"),
  study_data,
  meta_data
)
}
\arguments{
\item{variable_group}{\link{variable list} the names of the continuous
measurement variables building
a group, for that multivariate outliers
make sense.}

\item{id_vars}{\link{variable} optional, an ID variable of
the study data. If not specified row numbers are used.}

\item{label_col}{\link{variable attribute} the name of the column in the metadata
with labels of variables}

\item{n_rules}{\link{numeric} from=1 to=4. the no. of rules that must be violated
to classify as outlier}

\item{max_non_outliers_plot}{\link{integer} from=0. Maximum number of non-outlier
points to be plot. If more
points exist, a subsample will
be plotted only. Note, that
sampling is not deterministic.}

\item{criteria}{\link{set} tukey | 3SD | hubert | sigmagap. a vector with
methods to be used for detecting outliers.}

\item{study_data}{\link{data.frame} the data frame that contains the measurements}

\item{meta_data}{\link{data.frame} the data frame that contains metadata
attributes of study data}
}
\value{
a list with:
\itemize{
\item \code{SummaryTable}: \link{data.frame} underlying the plot
\item \code{SummaryPlot}: \link{ggplot2} outlier plot
\item \code{FlaggedStudyData} \link{data.frame} contains the original data frame with
the additional columns \code{tukey},
\verb{3SD},
\code{hubert}, and \code{sigmagap}. Every
observation
is coded 0 if no outlier was detected in
the respective column and 1 if an
outlier was detected. This can be used
to exclude observations with outliers.
}
}
\description{
A standard tool to detect multivariate outliers is the Mahalanobis distance.
This approach is very helpful for the interpretation of the plausibility of a
measurement given the value of another.
In this approach the Mahalanobis distance is used as a univariate measure
itself. We apply the same rules for the identification of outliers as in
univariate outliers:
\itemize{
\item the classical approach from Tukey: \eqn{1.5 * IQR} from the
1st (\eqn{Q_{25}}) or 3rd (\eqn{Q_{75}}) quartile.
\item the 3SD approach, i.e. any measurement of the Mahalanobis
distance not in the interval of \eqn{\bar{x} \pm 3*\sigma} is considered an
outlier.
\item the approach from Hubert for skewed distributions which is embedded in the
R package \pkg{robustbase}
\item a completely heuristic approach named \eqn{\sigma}-gap.
}

For further details, please see the vignette for univariate outlier.

\link{Indicator}
}
\section{ALGORITHM OF THIS IMPLEMENTATION:}{
\itemize{
\item Implementation is restricted to variables of type float
\item Remove missing codes from the study data (if defined in the metadata)
\item The covariance matrix is estimated for all variables from \code{variable_group}
\item The Mahalanobis distance of each observation is calculated
\eqn{MD^2_i  = (x_i - \mu)^T \Sigma^{-1} (x_i -  \mu)}
\item The four rules mentioned above are applied on this distance for
each observation in the study data
\item An output data frame is generated that flags each outlier
\item A parallel coordinate plot indicates respective outliers
}

List function.
}

\seealso{
\href{https://dataquality.qihs.uni-greifswald.de/VIN_acc_impl_multivariate_outlier.html}{Online Documentation}
}
