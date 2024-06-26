% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_extract_classes_by_functions.R
\name{prep_extract_classes_by_functions}
\alias{prep_extract_classes_by_functions}
\title{Extract old function based summary from data quality results}
\usage{
prep_extract_classes_by_functions(r)
}
\arguments{
\item{r}{\link{dq_report2}}
}
\value{
\link{data.frame} long format, compatible with \code{\link[=prep_summary_to_classes]{prep_summary_to_classes()}}
}
\description{
Extract old function based summary from data quality results
}
\seealso{
Other summary_functions: 
\code{\link{prep_combine_report_summaries}()},
\code{\link{prep_extract_summary}()},
\code{\link{prep_extract_summary.dataquieR_result}()},
\code{\link{prep_extract_summary.dataquieR_resultset2}()},
\code{\link{prep_render_pie_chart_from_summaryclasses_ggplot2}()},
\code{\link{prep_render_pie_chart_from_summaryclasses_plotly}()},
\code{\link{prep_summary_to_classes}()},
\code{\link{util_as_cat}()},
\code{\link{util_extract_indicator_metrics}()},
\code{\link{util_get_category_for_result}()},
\code{\link{util_get_colors}()},
\code{\link{util_get_html_cell_for_result}()},
\code{\link{util_get_labels_grading_class}()},
\code{\link{util_get_message_for_result}()},
\code{\link{util_get_rule_sets}()},
\code{\link{util_get_ruleset_formats}()},
\code{\link{util_get_thresholds}()},
\code{\link{util_html_table}()},
\code{\link{util_melt_summary}()},
\code{\link{util_sort_by_order}()}
}
\concept{summary_functions}
