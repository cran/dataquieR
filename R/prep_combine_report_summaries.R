#' Combine two report summaries
#'
#' @param ... objects returned by [prep_extract_summary]
#' @param summaries_list if given, [list] of objects returned by
#'                       [prep_extract_summary]
#' @param amend_segment_names [logical] use names of the `summaries_list` and
#'                            argument names as segment prefixes
#'
#' @return combined summaries
#' @family summary_functions
#' @export
prep_combine_report_summaries <- function(...,
                                          summaries_list,
                                          amend_segment_names = FALSE) {
  te <- topenv(parent.frame(1)) # see https://stackoverflow.com/a/27870803
  if (!(isNamespace(te) && getNamespaceName(te) == "dataquieR")) {
    lifecycle::deprecate_soft("2.1.0.9007",
                              "prep_combine_report_summaries()")
  }
  if (missing(summaries_list)) summaries_list <- list()
  if (!is.list(summaries_list)) {
    util_error("%s needs to be a list, if passed", sQuote("summaries_list"))
  }
  summaries_list <- c(list(...), summaries_list)
  util_expect_scalar(amend_segment_names, check_type = is.logical)

  if (amend_segment_names && any(util_empty(names(summaries_list)))) {
    util_error(
      "All summaries must be named arguments/list elements, if %s is %s",
      sQuote("amend_segment_names"),
      dQuote("TRUE"))
  }

  if (!all(vapply(summaries_list, function(summary) {
    inherits(summary, "dq_report2_summary") &&
      all(c("Data", "Table", "meta_data") %in% names(summary))
  }, FUN.VALUE = logical(1)))) {
    util_error(c("All summaries passed must be returned by %s or this function",
                 "i.e. feature %s, %s and %s"),
               sQuote("prep_extract_summary"),
               sQuote("Data"), sQuote("Table"), sQuote("meta_data"))
  }

  has_study_segment <- function(x) STUDY_SEGMENT %in% colnames(x)
  if (!all(vapply(FUN.VALUE = logical(1),
         lapply(summaries_list, `[[`, "Data"),
         has_study_segment
  ))) {
    util_error(
      c("All summaries passed must be returned by %s or this function,",
        "i.e., feature %s entries with %s columns"),
      sQuote("prep_extract_summary"),
      sQuote("Data"),
      sQuote(STUDY_SEGMENT))
  }

  if (!all(vapply(FUN.VALUE = logical(1),
                  lapply(summaries_list, `[[`, "Table"),
                  has_study_segment
  ))) {
    util_error(
      c("All summaries passed must be returned by %s or this function,",
        "i.e., feature %s entries with %s columns"),
      sQuote("prep_extract_summary"),
      sQuote("Table"),
      sQuote(STUDY_SEGMENT))
  }

  if (amend_segment_names) {
    summaries_list <- mapply(SIMPLIFY = FALSE,
                             summary = summaries_list,
                             name = names(summaries_list),
                             function(summary, name) {
                               summary$Data[[STUDY_SEGMENT]] <-
                                 paste(name, summary$Data[[STUDY_SEGMENT]],
                                       sep = ": ")
                               summary$Table[[STUDY_SEGMENT]] <-
                                 paste(name, summary$Table[[STUDY_SEGMENT]],
                                       sep = ": ")
                               if (!STUDY_SEGMENT %in%
                                   colnames(summary$meta_data)) {
                                 summary$meta_data[[STUDY_SEGMENT]] <- "Study"
                               }
                               summary$meta_data[[STUDY_SEGMENT]] <-
                                 paste(name, summary$meta_data[[STUDY_SEGMENT]],
                                       sep = ": ")
                               summary
                             })
  }

  summaries_list <- lapply(summaries_list, function(summary) {
    rownames(summary$Data) <- NULL
    rownames(summary$Table) <- NULL
    summary
  })

  combined <- list(
    Data =
      util_rbind(data_frames_list = lapply(summaries_list, `[[`, "Data")),
    Table =
      util_rbind(data_frames_list = lapply(summaries_list, `[[`, "Table")),
    meta_data =
      util_rbind(data_frames_list = lapply(summaries_list, `[[`, "meta_data"))
  )

  # VAR_NAMES may be ambiguous
  if (any(vapply(lapply(lapply(combined, `[[`, VAR_NAMES), duplicated),
         any,
         FUN.VALUE = logical(1)))) {
    util_warning( # FIXME: This warning is shown, even, if it is possible
      # FIXME: Allow to combine such reports, if the results are distinct,
      # still, i.e., one report with ICC, the other with item
      # missingness
      # by definiion, a message, but the effects are too big, so here a warning
      c("Some of the summaries comprise overlapping variables, will",
        "pick the first summary for each"),
      applicability_problem = TRUE,
      intrinsic_applicability_problem = FALSE)
    combined <-
      lapply(combined, function(x) {
        x <- x[!duplicated(x[[VAR_NAMES]]), , FALSE]
        x
      })
  }

  class(combined) <- "dq_report2_summary"

  combined
}
