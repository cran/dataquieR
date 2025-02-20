#' Check for unexpected data record set
#'
#' @description
#' This function tests that the identifiers match a provided record set. It is possible to
#' check for unexpected data record sets by study segments or to consider only selected
#' segments.
#'
#' [Indicator]
#'
#' @param level [character] a character vector indicating whether the assessment should be conducted at the study level (level = "dataframe") or at the segment level (level = "segment").
#' @param ... Depending on `level`, passed to either
#'            [util_int_unexp_records_set_segment] or
#'            [util_int_unexp_records_set_dataframe]
#'
#' @inheritParams .template_function_indicator
#'
#' @return a [list]. Depending on `level`, see
#'   [util_int_unexp_records_set_segment] or
#'   [util_int_unexp_records_set_dataframe] for a description of the outputs.
#'
#' @export
#'
int_unexp_records_set <- function(level = c("dataframe", "segment"),
                                  study_data,
                                  item_level = "item_level",
                                  label_col,
                                  meta_data = item_level,
                                  meta_data_v2,
                                  ...) {
  util_maybe_load_meta_data_v2()

  level <- util_match_arg(level)
  fname <- rlang::call_name(rlang::frame_call())
  fname <- paste("util", fname, level, sep = "_")
  miss_label_col <- missing(label_col)
  if (miss_label_col) {
    label_col <- NULL
  }
  if (missing(study_data)) {
    cl_l <- list(fname, level = level, item_level = item_level,
                 meta_data = meta_data,
                 label_col = label_col, ...)
  } else {
    cl_l <- list(fname, level = level, item_level = item_level,
                 meta_data = meta_data, study_data = study_data,
                 label_col = label_col, ...)
  }
  if (missing(item_level) && !missing(meta_data)) {
    cl_l$item_level <- NULL
  }
  if (miss_label_col) {
    cl_l$label_col <- NULL
  }
  cl_l <- cl_l[names(cl_l) %in% c("", names(formals(fname)))]
  cl2 <- do.call("call",
                 cl_l)
  eval(cl2)

}
