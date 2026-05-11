#' Bind data frames row-based
#'
#' if not all data frames share  all columns, missing columns will be filled with
#' `NA`s.
#'
#' @param ... [data.frame] none more more data frames
#' @param data_frames_list [list] optional, a list of data frames
#'
#' @return [data.frame] all data frames appended
#'
#' @examples
#' \dontrun{
#' util_rbind(head(cars), head(iris))
#' util_rbind(head(cars), tail(cars))
#' util_rbind(head(cars)[, "dist", FALSE], tail(cars)[, "speed", FALSE])
#' }
#'
#' @family data_management
#' @concept process
#' @noRd
util_rbind <- function(..., data_frames_list = list()) {
  data_frames_list <- c(list(...), data_frames_list)
  data_frames_list <- data_frames_list[!vapply(data_frames_list, is.null,
                                              FUN.VALUE = logical(1))]
  data_frames_list <-
    lapply(data_frames_list, util_expect_data_frame, dont_assign = TRUE)

  all_cn <- lapply(data_frames_list, colnames)

  tnsltd <- FALSE

  if (length(all_cn) > 0) {
    tnsltd <- inherits(all_cn[[1]], "dataquieR_translated")
  }

  if (tnsltd) {
    util_stop_if_not(
      "Internal error, sorry. Please report. Combining incompatibly translated column names (1)" =
        all(vapply(all_cn, inherits, "dataquieR_translated", FUN.VALUE = logical(1))))

    ns <- unique(vapply(all_cn, attr, "ns", FUN.VALUE = character(1)))
    lang <- unique(vapply(all_cn, attr, "lang", FUN.VALUE = character(1)))

    util_stop_if_not(
      "Internal error, sorry. Please report. Combining incompatibly translated column names (2)" =
        length(ns) == 1 &&
        length(lang) == 1)
  }


  all_cols <- unique(unlist(lapply(data_frames_list, colnames),
                            recursive = TRUE))

  data_frames_list <- lapply(data_frames_list, function(dfr) {
    if (nrow(dfr) > 0) {
      for (cl in setdiff(all_cols, colnames(dfr))) {
        dfr[[cl]] <- NA
      }
      dfr
    } else {
      NULL
    }
  })

  all_data_types <-
    vapply(setNames(nm = all_cols), function(cl) {
      dt <- unique(lapply(lapply(data_frames_list, `[[`, cl),
             attr,
             WELL_KNOWN_META_VARIABLE_NAMES$DATA_TYPE))
      if (length(dt) == 1) {
        r <- dt[[1]]
      } else {
        # util_warning("Internal error, sorry, please report: data types of a combined output table incompatible")
        r <- NA_character_
      }
      if (is.null(r)) {
        r <- NA_character_
      }
      r
    }, FUN.VALUE = character(1)) # This is used also from onLoad, so maybe, DATA_TYPE has not been defined, yet

  r <- do.call(rbind.data.frame, data_frames_list)

  if (length(all_data_types) == ncol(r)) {
    for (cl in colnames(r)) {
      attr(r[[cl]], WELL_KNOWN_META_VARIABLE_NAMES$DATA_TYPE) <-
        all_data_types[[cl]]
    }
  }

  if (tnsltd) {
    util_translated_colnames(r) <-
      util_attach_attr(
        colnames(r),
        class = "dataquieR_translated",
        names = unname(unlist(unique(lapply(all_cn, function(x) setNames(nm = as.character(x), ifelse(names(x) == "", as.character(x), names(x)))[colnames(r)])))),
        ns = ns,
        lang = lang
      )
  }

  r
}
