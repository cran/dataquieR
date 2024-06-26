% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_generate_anchor_tag.R
\name{util_generate_anchor_tag}
\alias{util_generate_anchor_tag}
\title{Generate a tag for a specific result}
\usage{
util_generate_anchor_tag(
  varname,
  callname,
  order_context = c("variable", "indicator"),
  name
)
}
\arguments{
\item{varname}{variable to create an anchor for}

\item{callname}{function call to create an anchor for}

\item{order_context}{anchor created on variable overview or indicator
overview page}

\item{name}{replaces \code{varname} and \code{callname}, must contain the \code{.}
separator, then}
}
\value{
the \code{htmltools} tag
}
\description{
for \code{dq_report2}
}
\seealso{
Other reporting_functions: 
\code{\link{util_alias2caption}()},
\code{\link{util_copy_all_deps}()},
\code{\link{util_create_page_file}()},
\code{\link{util_eval_to_dataquieR_result}()},
\code{\link{util_evaluate_calls}()},
\code{\link{util_float_index_menu}()},
\code{\link{util_generate_anchor_link}()},
\code{\link{util_generate_calls}()},
\code{\link{util_generate_calls_for_function}()},
\code{\link{util_load_manual}()},
\code{\link{util_make_data_slot_from_table_slot}()},
\code{\link{util_make_function}()},
\code{\link{util_order_by_order}()},
\code{\link{util_set_size}()}
}
\concept{process}
\concept{reporting_functions}
\keyword{internal}
