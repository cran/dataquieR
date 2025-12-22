util_handle_complex_data_types <- function(meta_data) {

  melt <- function(meta_data, rows, column, value, force = FALSE, tp) {
    util_stop_if_not(is.data.frame(meta_data))
    util_expect_scalar(value)
    util_expect_scalar(column, check_type = is.character)
    util_expect_scalar(rows,
                       allow_more_than_one = TRUE,
                       min_length = nrow(meta_data),
                       max_length = nrow(meta_data),
                       check_type = is.logical)
    if (force) util_stop_if_not(!missing(tp))
    if (nrow(meta_data) == 0) {
      return(meta_data)
    }
    if (!column %in% colnames(meta_data)) {
      meta_data[[column]] <- NA_character_
    }
    if (force) {
      if (column != DATA_TYPE &&
          any(rows & !util_empty(meta_data[[column]]) &
          tolower(trimws(meta_data[[column]])) !=
          tolower(trimws(value)), na.rm = TRUE)) {
        util_warning(
          c("Overwriting some entries in %s in",
            "the %s, because they have %s set to %s"),
          dQuote(column),
          sQuote("meta_data"),
          sQuote(DATA_TYPE),
          dQuote(tp)
        )
      }
    } else {
      rows <- rows & util_empty(meta_data[[column]])
    }
    meta_data[rows, column] <- as.character(value)
    meta_data
  }

  if (DATA_TYPE %in% colnames(meta_data)) {
    # Handle non-basic data types

    tp <- tolower(trimws(meta_data[[DATA_TYPE]]))

    count_rows <- tp == "count"
    if (any(count_rows, na.rm = TRUE)) {
      meta_data <- melt(meta_data,
                        count_rows,
                        DATA_TYPE,
                        DATA_TYPES$INTEGER, force = TRUE, tp = tp)
      meta_data <- melt(meta_data,
                        count_rows,
                        SCALE_LEVEL,
                        SCALE_LEVELS$RATIO)
      meta_data <- melt(meta_data,
                        count_rows,
                        HARD_LIMITS,
                        "[0; Inf)")
      meta_data <- melt(meta_data,
                        count_rows,
                        EXTENDED_DATA_TYPE,
                        "count", force = TRUE, tp = tp)
      # TODO Implement these distributions in distributional indicator functions:
      # meta_data[[DISTRIBUTION]][count_rows] <- "Poisson | Negative Binomial"
    }

    unkown <- !tp %in% DATA_TYPES
    if (any(unkown)) {
      meta_data <- .util_fix_data_types(meta_data)
    }
  }

  return(meta_data)
}
