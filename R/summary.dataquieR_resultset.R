#' Summarize a [dataquieR] report
#' @description
#' Summarizes a [dataquieR report][dq_report] extracting all GRADING
#'                                                 results.
#'
#' @param object [dataquieR report][dq_report].
#' @param ... not used yet.
#' @param aspect what sort of issues to summarize
#' @param return_the_value [logical] return the `GRADING` or error message,
#'                                   a color otherwise.
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
summary.dataquieR_resultset <- function(object, aspect =
                                          c("issue",
                                            "applicability",
                                            "error"),
                                        return_the_value = TRUE,
                                        ...) { # TODO: add int_* and new output in acc_distributions
  util_expect_scalar(aspect, allow_more_than_one = TRUE, check_type =
                       is.character)
  aspect <- match.arg(aspect)
  util_expect_scalar(aspect)
  .df <- as.data.frame(object)
  .res <- object$app_mat$SummaryTable
  .cols <- setdiff(colnames(.res), c(STUDY_SEGMENT, colnames(.res)[[1]]))
  if (length(.cols) > 0)
    .res[, .cols] <- NA
  if (aspect %in% c("error", "applicability")) {
    fun.aggregate <- function(x, ...) {
      if (return_the_value) {
        if (length(x) == 0 || all(is.na(x))) {
          ""
        } else {
          paste(x, collapse = "\n")
        }
      } else {
        if (length(x) == 0 || all(is.na(x))) {
          util_as_color("green")
        } else {
          cl <- util_as_color(x)
          cl <- cl[!is.na(cl)]
          if (length(cl) == 0) {
            "green"
          } else {
            as.character(max(cl, na.rm = TRUE))
          }
        }
      }
    }
    .summarize_result <- function(.row) {
      result <- .row[["results"]]
      if (inherits(result, "try-error")) {
        applicability_problem <-
          identical(try(attr(attr(result, "condition"),
                             "applicability_problem"),
                        silent = TRUE), TRUE)
        if ((aspect == "applicability" && applicability_problem) ||
            (aspect == "error" && !applicability_problem)) {
          if (return_the_value) {
            g <- as.character(result)
          } else {
            g <- util_as_color("red")
          }
        } else {
          g <- NA
        }
      } else if (length(attr(result, "error")) +
                 length(attr(result, "warning")) +
                 length(attr(result, "message")) > 0) {
        applicability_problem_e <-
          vapply(lapply(attr(result, "error"), attr,
                        "applicability_problem"), isTRUE,
                 FUN.VALUE = logical(1))
        applicability_problem_w <-
          vapply(lapply(attr(result, "warning"), attr,
                        "applicability_problem"), isTRUE,
                 FUN.VALUE = logical(1))
        applicability_problem_m <-
          vapply(lapply(attr(result, "message"), attr,
                        "applicability_problem"), isTRUE,
                 FUN.VALUE = logical(1))
        if (aspect == "applicability") {
          ge <- unlist(lapply(attr(result, "error"), as.character),
                      recursive = FALSE)[applicability_problem_e]
          gw <- unlist(lapply(attr(result, "warning"), as.character),
                       recursive = FALSE)[applicability_problem_w]
          gm <- unlist(lapply(attr(result, "message"), as.character),
                       recursive = FALSE)[applicability_problem_m]
        } else {
          ge <- unlist(lapply(attr(result, "error"), as.character),
                       recursive = FALSE)[!applicability_problem_e]
          gw <- unlist(lapply(attr(result, "warning"), as.character),
                       recursive = FALSE)[!applicability_problem_w]
          gm <- unlist(lapply(attr(result, "message"), as.character),
                      recursive = FALSE)[!applicability_problem_m]
        }
        if (return_the_value) {
          g <- paste(c(ge, gw, gm), collapse = "\n")
        } else {
          if (length(ge) > 0)
            g <- util_as_color("red")
          else if (length(gw) > 0)
            g <- util_as_color("yellow")
          else if (length(gm) > 0)
            g <- util_as_color("green")
          else
            g <- util_as_color("green")
        }
      } else {
        g <- NA
      }
      if ("resp_vars" %in% names(.row)) {# FIXME: no error at all in a vectoized function ylields atable with a green na only.
        StudyVariable <- unlist(.row["resp_vars"], recursive = FALSE)
        if (length(StudyVariable) == 0) {
          StudyVariable <- object$app_mat$ReportSummaryTable$Variables
        }
        implementationform <- .row[["implementationform"]]
        return(unique(
          data.frame(implementationform = implementationform, StudyVariable =
                       StudyVariable, GRADING = g,
                     stringsAsFactors = FALSE)
        ))
      }
      return(NULL)
    }
  }
  if (aspect == "issue") {
    fun.aggregate <- function(x, ...) {
      if (length(x) == 0 || all(is.na(x))) {
        if (return_the_value) {
          NA_integer_
        } else {
          NA_character_
        }
      } else {
        if (!return_the_value) {
          as.character(max(util_as_color(x), na.rm = TRUE))
        } else {
          max(x, na.rm = TRUE)
        }
      }
    }
    .summarize_result <- function(.row) {
      result <- .row[["results"]]
      implementationform <- .row[["implementationform"]]
      # r <- data.frame()
      if ("SummaryTable" %in% names(result)) {
        SummaryTable <- result$SummaryTable
        if (is.data.frame(SummaryTable) && nrow(SummaryTable) > 0) {
          if (all(c("Variables", "GRADING") %in% colnames(SummaryTable))) {
            StudyVariable <- SummaryTable[["Variables"]]
            if (length(StudyVariable) == 0) {
              StudyVariable <- object$app_mat$ReportSummaryTable$Variables
            }
            if (return_the_value) {
              GRADING <- util_as_numeric(SummaryTable[["GRADING"]])
            } else {
              GRADING <- util_as_color(ifelse(util_as_numeric(SummaryTable[["GRADING"]]) == 1,
                                "red", "green"))
            }
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
        if (length(StudyVariable) == 0) { # TODO: Search these hotfixes and solve this slightly better. The report has now a NA-row for the REDCap contradictions call. However, the error was reported for the other con_con
          StudyVariable <- object$app_mat$ReportSummaryTable$Variables
        }
        if (return_the_value) {
          GRADING <- util_as_numeric(all_results[["GRADING"]])
        } else {
          GRADING <- util_as_color(ifelse(all_results[["GRADING"]] == 1,
                            "red", "green"))
        }
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
  all_gradings <- all_gradings[!is.na(all_gradings$GRADING), , drop = FALSE]
  if (!return_the_value)
    all_gradings$GRADING <- as.character(all_gradings$GRADING)
  suppressWarnings(
    r <- as.data.frame(
      reshape::cast(all_gradings, StudyVariable ~ implementationform,
                  value = "GRADING", fun.aggregate = fun.aggregate,
                  fill = fun.aggregate(c()))
    )
  )
  if (!return_the_value)
    r[, setdiff(colnames(r), "StudyVariable")] <-
    lapply(r[setdiff(colnames(r), "StudyVariable")], util_as_color)
  r$StudyVariable <- as.character(r$StudyVariable)
  if ((STUDY_SEGMENT %in% names(.res))) {
    r[[STUDY_SEGMENT]] <- mget(as.character(r$StudyVariable),
                                   as.environment(as.list(setNames(
                                     .res$STUDY_SEGMENT, nm =
                                       .res$Variables))),
                                   ifnotfound = NA_character_)
  } else { # nocov start
    # This should be dead code currently, since by default, STUDY_SEGMENT
    # is amended if missing and set to "Study" by pro_applicability_matrix
    r[[STUDY_SEGMENT]] <- vapply(as.character(r$StudyVariable), FUN =
                                       function(x) "all variables", FUN.VALUE =
                                       character(1))
  } # nocov end
  #if (class())
  if (!return_the_value) {
    empty <- rowSums((!is.na(r[, intersect(.cols, colnames(r)), FALSE]))) == 0
    r$AnyProblem[!empty] <- apply(r[!empty,
                                    intersect(.cols, colnames(r)), FALSE], 1,
                                    function(x) {
                                      as.character(max(util_as_color(x),
                                                       na.rm = TRUE))
                                    })
  } else if (is.numeric(as.matrix((r[, intersect(.cols, colnames(r))])))) {
    r$AnyProblem <- rowSums(r[, intersect(.cols, colnames(r)), FALSE], na.rm =
                              TRUE) > 0
  } else {
    r$AnyProblem <-
      rowSums(!util_empty(r[, intersect(.cols, colnames(r)), FALSE])) > 0
  }
  class(r) <- c(paste0("summary_", dataquieR_resultset_class), class(r))
  r
}
