% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_eval_rule.R
\name{util_eval_rule}
\alias{util_eval_rule}
\title{Evaluate a parsed redcap rule for given study data}
\usage{
util_eval_rule(
  rule,
  ds1,
  meta_data = "item_level",
  use_value_labels,
  replace_missing_by = "NA",
  replace_limits = TRUE
)
}
\arguments{
\item{rule}{the redcap rule (parsed, already)}

\item{ds1}{the study data as prepared by \code{prep_prepare_dataframes}}

\item{meta_data}{the metadata}

\item{use_value_labels}{map columns with \code{VALUE_LABELS} as factor variables}

\item{replace_missing_by}{\link{enum} LABEL | INTERPRET | NA . Missing codes should
be replaced by the missing labels, the
\code{AAPOR} codes from the missing table or
by \code{NA}. Can also be an empty string to keep
the codes.}

\item{replace_limits}{\link{logical} replace hard limit violations by \code{NA}}
}
\value{
the result of the parsed rule
}
\description{
also allows to use \code{VAR_NAMES} in the rules,
if other labels have been selected
}
\seealso{
Other redcap: 
\code{\link{util_get_redcap_rule_env}()}
}
\concept{process}
\concept{redcap}
\keyword{internal}
