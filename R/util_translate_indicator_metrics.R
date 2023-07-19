#' Translate standard column names to readable ones
#'
#' @param colnames [character] the names to translate
#' @param short [logical] include unit in output
#'
#' @return translated names
util_translate_indicator_metrics <- function(colnames, short = FALSE) {
  util_expect_scalar(short, check_type = is.logical)
  abbreviationMetrics <- util_get_concept_info("abbreviationMetrics")
  dqi <- util_get_concept_info("dqi")
  vapply(colnames, FUN.VALUE = character(1), FUN = function(x) {
    if (x %in% c("Variables", VAR_NAMES)) {
      return(x)
    }
    util_stop_if_not(length(x) == 1)
    nm <- strsplit(x, "_", fixed = TRUE)[[1]]
    if (length(nm) >= 2) {
      m <- head(subset(abbreviationMetrics, get("Abbreviation") == nm[[1]],
                       "Metrics", drop = TRUE), 1)
      if (short) {
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
        if (short)
          sprintf("%s", d)
        else
          sprintf("%s (%s)", d, m)
      } else {
        NA_character_
      }
    } else {
      NA_character_
    }
  })
}
