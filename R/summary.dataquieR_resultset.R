#' Summarize a [dataquieR] report
#' @description
#' Summarizes a [dataquieR report][dq_report] extracting all GRADING
#'                                                 results.
#'
#' @param object [dataquieR report][dq_report].
#' @param ... not used yet.
#'
#' @return a [data.frame] with one row per variable and one column per `GRADING`
#'         result. Each function providing a `GRADING` conforming to the
#'         standards is represented by a column. `GRADING` expresses the
#'         presence of a problem with `0 = no | 1 = yes`
#' @export
#' @importFrom stats setNames
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#' # runs spuriously slow on rhub
#' load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
#'   environment())
#' load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
#'   environment())
#' report <- suppressWarnings(dq_report(
#'   variables = head(meta_data[[LABEL]], 5),
#'   study_data, meta_data,
#'   cores = 1,
#'   label_col = LABEL, dimensions =
#'   c( # for sake of speed, omit Accuracy here
#'        "Consistency")
#' ))
#' x <- summary(report)
#' }
summary.dataquieR_resultset <- function(object, ...) {
  .df <- as.data.frame(object)
  .res <- object$app_mat$SummaryTable
  .cols <- setdiff(colnames(.res), c(KEY_STUDY_SEGMENT, colnames(.res)[[1]]))
  if (length(.cols) > 0)
    .res[, .cols] <- NA
  .summarize_result <- function(.row) {
    result <- .row[["results"]]
    implementationform <- .row[["implementationform"]]
    # r <- data.frame()
    if ("SummaryTable" %in% names(result)) {
      SummaryTable <- result$SummaryTable
      if (is.data.frame(SummaryTable) && nrow(SummaryTable) > 0) {
        if (all(c("Variables", "GRADING") %in% colnames(SummaryTable))) {
          StudyVariable <- SummaryTable[["Variables"]]
          GRADING <- util_as_numeric(SummaryTable[["GRADING"]])
          return(data.frame(implementationform = implementationform,
                            StudyVariable = StudyVariable, GRADING = GRADING,
                            stringsAsFactors = FALSE))
        }
      }
    } else if (any(vapply(result, function(x) "SummaryTable" %in% names(x),
                          FUN.VALUE = logical(1)))) {
      all_tables <- lapply(result, `[[`, "SummaryTable")
      all_tables <- all_tables[vapply(all_tables, is.data.frame, FUN.VALUE =
                                        logical(1))]
      all_results <- try(do.call(rbind, all_tables))
      all_results <- all_results %>%
        dplyr::group_by(Variables = get("Variables")) %>%
        dplyr::summarize(GRADING = max(GRADING, na.rm = TRUE))
      StudyVariable <- all_results[["Variables"]]
      GRADING <- util_as_numeric(all_results[["GRADING"]])
      if (length(GRADING) > 0 && length(GRADING == length(StudyVariable))) {
        return(unique(
          data.frame(implementationform = implementationform, StudyVariable =
                       StudyVariable, GRADING = GRADING,
                     stringsAsFactors = FALSE)
        ))
      }
    }
    NULL
  }
  if (nrow(.df) * ncol(.df) > 0) {
    all_gradings <- apply(.df, 1, .summarize_result)
  } else {
    all_gradings <- NULL
  }
  if (length(all_gradings) == 0) {
    util_warning(c(
      "No summary available for this report. None of the called",
      "implementation forms returned any GRADING column."),
      applicability_problem = FALSE)
    return(data.frame())
  }
  all_gradings <- do.call(rbind, all_gradings)
  r <- as.data.frame(
    reshape::cast(all_gradings, StudyVariable ~ implementationform,
                value = "GRADING", fun.aggregate = max, na.rm = TRUE,
                fill = NA_real_)
  )
  r$StudyVariable <- as.character(r$StudyVariable)
  if ((KEY_STUDY_SEGMENT %in% names(.res))) {
    r[[KEY_STUDY_SEGMENT]] <- mget(as.character(r$StudyVariable),
                                   as.environment(as.list(setNames(
                                     .res$KEY_STUDY_SEGMENT, nm =
                                       .res$Variables))),
                                   ifnotfound = NA_character_)
  } else { # nocov start
    # This should be dead code currently, since by default, KEY_STUDYY_SEGMENT
    # is amended if missing and set to "Study" by pro_applicability_matrix
    r[[KEY_STUDY_SEGMENT]] <- vapply(as.character(r$StudyVariable), FUN =
                                       function(x) "all variables", FUN.VALUE =
                                       character(1))
  } # nocov end
  r$AnyProblem <- rowSums(r[, intersect(.cols, colnames(r)), FALSE], na.rm =
                            TRUE) > 0
  class(r) <- c(paste0("summary_", dataquieR_resultset_class), class(r))
  r
}
