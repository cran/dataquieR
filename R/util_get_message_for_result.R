#' Return messages/warnings/notes/error messages for a result
#'
#' @param result a `dataquieR_resultset2` result
#' @param aspect an aspect/problem category of results
#' @param collapse either a lambda function or a separator for combining
#'                 multiple messages for the same result
#' @param ... not used
#'
#' @return hover texts for results with data quality issues,
#'         run-time errors, warnings or notes (aka messages)
#'
#' @family summary_functions
#' @concept reporting
#' @keywords internal
util_get_message_for_result <- function(result,
                                   aspect = c("applicability", "error",
                                              "anamat", "indicator_or_descriptor"),
                                   collapse = "\n<br />\n", ...) {
  # check if the aspect is an allowed name (robustness)
  aspect <- util_match_arg(aspect, several_ok = FALSE)

  if(aspect == "indicator_or_descriptor"){
    return("")
  }

  if (!inherits(result, "dataquieR_result")) {
    return("No results computed")
  }

  if (!(aspect %in% c("applicability", "anamat"))) {
    expected_a_result <-
      (util_get_category_for_result(result, "applicability") %in% c(cat1, cat2, cat3))
    # TODO: Fix error must be added in squarereportrenderer
    expected_a_result <-
      expected_a_result && !is.na(util_get_category_for_result(result, "anamat"))

  } else {
    expected_a_result <- NA
  }
  msgs <- character(0)
  if (length(attr(result, "message")) > 0) {
    for (w in attr(result, "message")) {
      applicability_problem <- attr(w, "applicability_problem")
      if (is.null(applicability_problem) || is.na(applicability_problem)) {
        applicability_problem <- FALSE
      }
      intrinsic_applicability_problem <- attr(w,
                                              "intrinsic_applicability_problem")
      if (is.null(intrinsic_applicability_problem) ||
          is.na(intrinsic_applicability_problem)) {
        intrinsic_applicability_problem <- FALSE
      }
      if (aspect %in% c("applicability", "anamat")) {
        if (aspect == "applicability" &&
            applicability_problem && !intrinsic_applicability_problem) {
          msgs <- c(msgs, paste("<span class=\"dataquieR-message-message\">",
                                gsub("\n>.*$", "", #gsub("^.*?: ", "",
                                                  conditionMessage(w)),
                                "</span>"))
        } else if ((aspect == "anamat") &&
                    applicability_problem && intrinsic_applicability_problem) {
          msgs <- c(msgs, paste("<span class=\"dataquieR-message-message\">",
                                gsub("\n>.*$", "", #gsub("^.*?: ", "",
                                                  conditionMessage(w)),
                                "</span>"))
        }
      } else {
        if (!applicability_problem && !intrinsic_applicability_problem) {
          msgs <- c(msgs, paste("<span class=\"dataquieR-message-message\">",
                                gsub("\n>.*$", "", #gsub("^.*?: ", "",
                                                  conditionMessage(w)),
                                "</span>"))
        }
      }
    }
  }
  if (length(attr(result, "warning")) > 0) {
    for (w in attr(result, "warning")) {
      applicability_problem <- attr(w, "applicability_problem")
      if (is.null(applicability_problem) || is.na(applicability_problem)) {
        applicability_problem <- FALSE
      }
      intrinsic_applicability_problem <- attr(w,
                                              "intrinsic_applicability_problem")
      if (is.null(intrinsic_applicability_problem) ||
          is.na(intrinsic_applicability_problem)) {
        intrinsic_applicability_problem <- FALSE
      }
      if (aspect == "applicability") {
        if (applicability_problem && !intrinsic_applicability_problem) {
          msgs <- c(msgs, paste("<span class=\"dataquieR-warning-message\">",
                                gsub("\n>.*$", "", #gsub("^.*?: ", "",
                                     conditionMessage(w)),
                                "</span>"))
        }
      } else if (aspect == "anamat") {
        if (applicability_problem && intrinsic_applicability_problem) {
          msgs <- c(msgs, paste("<span class=\"dataquieR-warning-message\">",
                                gsub("\n>.*$", "", #gsub("^.*?: ", "",
                                     conditionMessage(w)),
                                "</span>"))
        }
      } else {
        if (!applicability_problem && !intrinsic_applicability_problem) {
          msgs <- c(msgs, paste("<span class=\"dataquieR-warning-message\">",
                                gsub("\n>.*$", "", #gsub("^.*?: ", "",
                                     conditionMessage(w)),
                                "</span>"))
        }
      }
    }
  }
  if (length(attr(result, "error")) > 0) {
    for (w in attr(result, "error")) {
      applicability_problem <- attr(w, "applicability_problem")
      if (is.null(applicability_problem) || is.na(applicability_problem)) {
        applicability_problem <- FALSE
      }
      intrinsic_applicability_problem <- attr(w,
                                              "intrinsic_applicability_problem")
      if (is.null(intrinsic_applicability_problem) ||
          is.na(intrinsic_applicability_problem)) {
        intrinsic_applicability_problem <- FALSE
      }
      if (aspect %in% c("applicability", "anamat")) {
        if (aspect == "applicability" &&
            applicability_problem && !intrinsic_applicability_problem) {
          msgs <- c(msgs, paste("<span class=\"dataquieR-error-message\">",
                                gsub("\n>.*$", "", #gsub("^.*?: ", "",
                                                  conditionMessage(w)),
                    "</span>"))
        } else if ((aspect == "anamat") &&
                    applicability_problem && intrinsic_applicability_problem) {
          msgs <- c(msgs, paste("<span class=\"dataquieR-error-message\">",
                                gsub("\n>.*$", "", #gsub("^.*?: ", "",
                                                  conditionMessage(w)),
                    "</span>"))
        }
      } else {
        if (!applicability_problem) {
          msgs <- c(msgs, paste("<span class=\"dataquieR-error-message\">",
                                gsub("\n>.*$", "", #gsub("^.*?: ", "",
                                                  conditionMessage(w)),
                    "</span>"))
        }
      }
    }
  }

  msgs <- unique(msgs)

  if (is.function(collapse)) {
    collapse(rev(msgs))
  } else {
    paste0(rev(msgs), collapse = collapse)
  }
}
