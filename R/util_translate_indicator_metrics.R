#' Translate standard column names to readable ones
#'
#' TODO: Duplicate of util_make_data_slot_from_table_slot ??
#'
#' @param colnames [character] the names to translate
#' @param short [logical] include unit letter in output
#' @param long [logical] include unit description in output
#' @param ignore_unknown [logical] do not replace unknown indicator metrics
#'                                 by `NA`, keep them
#'
#' @return translated names
#'
#' @keywords internal
util_translate_indicator_metrics <- function(colnames, short = FALSE,
                                             long = TRUE,
                                             ignore_unknown = FALSE) {
  util_expect_scalar(short, check_type = is.logical)
  util_expect_scalar(long, check_type = is.logical)
  util_expect_scalar(ignore_unknown, check_type = is.logical)
  util_stop_if_not(`Cannot create short labels that are long` =
                     !(short && long))
  abbreviationMetrics <- util_get_concept_info("abbreviationMetrics")
  dqi <- util_get_concept_info("dqi")
  new_colnames <-
    vapply(colnames, FUN.VALUE = character(1), FUN = function(x) {
    if (x %in% c("Variables", VAR_NAMES, STUDY_SEGMENT)) {
      return(x)
    }
    util_stop_if_not(length(x) == 1)
    nm <- strsplit(x, "_", fixed = TRUE)[[1]]
    if (length(nm) >= 2) {
      if (long) {
        m <- head(subset(abbreviationMetrics, get("Abbreviation") == nm[[1]],
                         "Metrics", drop = TRUE), 1)
      } else {
        m <- head(subset(abbreviationMetrics, get("Abbreviation") == nm[[1]],
                         "public_name", drop = TRUE), 1)
      }
      if (short) {
        # m <- head(subset(abbreviationMetrics, get("Abbreviation") == nm[[1]],
        #                  "public_name", drop = TRUE), 1)
        d <- head(subset(dqi, get("abbreviation") == paste(tail(nm, -1),
                                                           collapse = "_"),
                         "public_name", drop = TRUE), 1)
        if (all(is.na(d))) { # FIXME: sometimes, public_name is missing, sometimes, the abbreviation is not unique any more!!
          d <- head(subset(dqi, get("abbreviation") == paste(tail(nm, -1),
                                                             collapse = "_"),
                           "Name", drop = TRUE), 1)
        }
        d <- unique(d)
        d <- paste(d, collapse = " ")

      } else {
        d <- head(subset(dqi, get("abbreviation") == paste(tail(nm, -1),
                                                           collapse = "_"),
                         "Name", drop = TRUE), 1)
      }

      if (length(m) == length(d) && length(d) == 1 &&
          !util_empty(m) && !util_empty(d)) {
       #if (short)
       #   sprintf("%s", d)
       # else
          sprintf("%s (%s)", d, m)
      } else {
        NA_character_
      }
    } else {
      NA_character_
    }
  })
  if (ignore_unknown) {
    new_colnames[is.na(new_colnames)] <-
      colnames[is.na(new_colnames)]
  }
  new_colnames
}
