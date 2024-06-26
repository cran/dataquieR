% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_ensure_in.R
\name{util_ensure_in}
\alias{util_ensure_in}
\title{similar to match.arg}
\usage{
util_ensure_in(x, set, err_msg, error = FALSE, applicability_problem = NA)
}
\arguments{
\item{x}{\link{character} vector of needles}

\item{set}{\link{character} vector representing the haystack}

\item{err_msg}{\link{character} optional error message. Use \%s twice, once for
the missing elements and once for proposals}

\item{error}{\link{logical} if \code{TRUE}, the execution will stop with an error,
if not all \code{x} are elements of \code{set}, otherwise,
it will throw a warning and "clean" the vector \code{x}
from unexpected elements.}

\item{applicability_problem}{\link{logical} error indicates unsuitable resp_vars}
}
\value{
\link{character} invisible(intersect(x, set))
}
\description{
will only warn and return a cleaned \code{x}.
}
\seealso{
Other robustness_functions: 
\code{\link{util_as_valid_missing_codes}()},
\code{\link{util_check_one_unique_value}()},
\code{\link{util_correct_variable_use}()},
\code{\link{util_empty}()},
\code{\link{util_ensure_character}()},
\code{\link{util_ensure_suggested}()},
\code{\link{util_expect_scalar}()},
\code{\link{util_fix_rstudio_bugs}()},
\code{\link{util_is_integer}()},
\code{\link{util_is_numeric_in}()},
\code{\link{util_is_valid_missing_codes}()},
\code{\link{util_match_arg}()},
\code{\link{util_observations_in_subgroups}()},
\code{\link{util_only_NAs}()},
\code{\link{util_stop_if_not}()},
\code{\link{util_warn_unordered}()}
}
\concept{robustness}
\concept{robustness_functions}
\keyword{internal}
