#' Return the category for a result
#'
#' messages do not cause any category, warnings are `cat3`, errors are `cat5`
#'
#' @param result a `dataquieR_resultset2` result
#' @param aspect an aspect/problem category of results
#'               (error, applicability error)
#' @param ... not used
#'
#' @return a category, see `util_as_cat()`
#'
#' @family summary_functions
#' @noRd
util_get_category_for_result <- function(result, aspect =
                                        c("applicability", "error",
                                          "anamat", "indicator_or_descriptor"),
                                      ...) {

  aspect <- util_match_arg(aspect, several_ok = FALSE)

  ##### preps -----
  util_ensure_suggested("htmltools", "Generating nice tables")

  ### indicator_or_descriptor -----

  if (aspect %in% c("indicator_or_descriptor")) {
    function_name <- attr(result, "function_name")
    if (is.null(function_name)) { # if we do not have a result, we cannot tell
      return(util_as_cat(NA))
    }
    if (get(function_name, .indicator_or_descriptor)) {
      return(cat1) # It's a data quality indicator
    } else {
      return(cat3) # It's a descriptor
    }
    # # TODO: Fix "error" must be added in squarereportrenderer
    # expected_a_result <-
    #   (util_get_category_for_result(
    #     result, "applicability") %in% c(cat1, cat2, cat3))
    # # TODO: Fix error must be added in squarereportrenderer
    # expected_a_result <-
    #   expected_a_result &&
    #   !is.na(util_get_category_for_result(result, "anamat"))
    # if (length(result) == 0 || inherits(result, "dataquieR_NULL")) {
    #   if (expected_a_result)
    #     return(cat4) # result missing for unknown reasons
    #   else
    #     return(cat3) # result missing, but because of n.a. (intrinsic or due to missing metadata)
    # }
    #
    # if (is.data.frame(result$SummaryTable)) { # TODO: This is not enough, now we have not only GRADING and we have SegmentTable, ... -- better mark indicator functions as such.
    #   if (!is.null(result$SummaryTable$GRADING)) {
    #     if (all(is.na(result$SummaryTable$GRADING))) {
    #       return(cat4) # grading column exists but is empty
    #     } else {
    #       return(cat1) # It's a data quality indicator
    #     }
    #   } else {
    #     # no grading column provided
    #     return(cat3) # It's a descriptor
    #   }
    #   util_error("this should be unreachable. Internal error, please repor a bug, sorry.")
    # }
    # return(cat3) # It's a descriptor
  } else { ##### error or applicability or anamat -----
    if (length(attr(result, "error")) > 0) { # some error occurred ----
      util_stop_if_not(length(attr(result, "error")) == 1)
      cnd <- attr(result, "error")[[1]]
      applicability_problem <- attr(cnd, "applicability_problem")
      if (is.null(applicability_problem) || is.na(applicability_problem)) {
        applicability_problem <- FALSE
      }
      intrinsic_applicability_problem <- attr(cnd,
                                              "intrinsic_applicability_problem")
      if (is.null(intrinsic_applicability_problem) ||
          is.na(intrinsic_applicability_problem)) {
        intrinsic_applicability_problem <- FALSE
      }
      if (aspect == "anamat" && applicability_problem &&
          intrinsic_applicability_problem) {
        return(cat5)
      } else if (aspect == "anamat" && applicability_problem &&
          !intrinsic_applicability_problem) {
        return(cat1)
      } else if (aspect == "anamat" && !applicability_problem) {
        return(cat1)
      } else if (aspect == "applicability" && applicability_problem &&
                 !intrinsic_applicability_problem) {
        return(cat5) # applicability error was asked and occurred
      } else if (aspect == "error"  && !applicability_problem) {
        return(cat5) # other error was asked and occurred
      }
      return(cat3) # error of the other class (not asked now) occurred
    }
    # If we have warnings, we cannot return, we have to go over all warnings and return cat3, if we find a warning of the aspect's class
    res <- cat1
    if (length(attr(result, "warning")) > 0) { # some warning occurred ----
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
        if (aspect == "anamat" && applicability_problem &&
            intrinsic_applicability_problem) {
          # if (res != cat3)
          #   res <- washedoutgreen # maybe no example possible?
        } else if (aspect == "anamat" && applicability_problem &&
                   !intrinsic_applicability_problem) {
          # if (res != cat3)
          #   res <- cat1
        } else if (aspect == "anamat" && !applicability_problem) {
          # if (res != cat3)
          #   res <- cat1
        } else if (aspect == "applicability") {
          if (applicability_problem && !intrinsic_applicability_problem) {
            # res <- cat3
            return(cat3)
          }
        } else if (aspect == "error") {
          if (!applicability_problem) {
            # res <- cat3
            return(cat3)
          }
        } else util_error("internal error in get color for result: %s",
                          dQuote(aspect))
      }
    }
    return(cat1)
  }
}
