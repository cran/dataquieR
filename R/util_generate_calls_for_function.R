#' Generate function calls for a given indicator function
#'
#' new reporting pipeline v2.0
#'
#' @param fkt the indicator function's name
#' @param meta_data the item level metadata data frame
#' @param label_col the label column
#' @param meta_data_segment segment level metadata
#' @param meta_data_dataframe data frame level metadata
#' @param meta_data_cross_item cross-item level metadata
#' @param specific_args argument overrides for specific functions
#' @param arg_overrides general argument overrides
#' @param resp_vars variables to be respected
#'
#' @return function calls for the given function
util_generate_calls_for_function <-
  function(fkt,
           meta_data,
           label_col,
           meta_data_segment,
           meta_data_dataframe,
           meta_data_cross_item,
           specific_args,
           arg_overrides,
           resp_vars) { # TODO: Document
  .meta_data_env$meta_data <- meta_data
  .meta_data_env$label_col <- label_col
  .meta_data_env$meta_data_segment <- meta_data_segment
  .meta_data_env$meta_data_dataframe <- meta_data_dataframe
  .meta_data_env$meta_data_cross_item <- meta_data_cross_item
  on.exit({
    .meta_data_env$meta_data <- NULL
    .meta_data_env$label_col <- NULL
    .meta_data_env$meta_data_segment <- NULL
    .meta_data_env$meta_data_dataframe <- NULL
    .meta_data_env$meta_data_cross_item <- NULL
    .meta_data_env$target_meta_data <- NULL
  })
  .to_fill <- formals(fkt)
  to_fill <- list()
  to_fill[intersect(names(.to_fill), names(arg_overrides))] <-
    arg_overrides[intersect(names(.to_fill), names(arg_overrides))]
  to_fill[intersect(names(.to_fill), names(specific_args[[fkt]]))] <-
    specific_args[[fkt]][
      intersect(names(.to_fill), names(specific_args[[fkt]]))]
  if ("study_data" %in% names(.to_fill)) {
    to_fill[["study_data"]] <- quote(study_data)
  }
  if ("meta_data_cross_item" %in% names(.to_fill)) {
    to_fill[["meta_data_cross_item"]] <- quote(meta_data_cross_item)
  }
  if ("meta_data_dataframe" %in% names(.to_fill)) {
    to_fill[["meta_data_dataframe"]] <- quote(meta_data_dataframe)
  }
  if ("meta_data_segment" %in% names(.to_fill)) {
    to_fill[["meta_data_segment"]] <- quote(meta_data_segment)
  }
  if ("meta_data" %in% names(.to_fill)) {
    to_fill[["meta_data"]] <- quote(meta_data)
  }
  if ("label_col" %in% names(.to_fill)) {
    to_fill[["label_col"]] <- label_col
  }
  if ("resp_vars" %in% names(.to_fill)) {
    if ("variable_group" %in% names(.to_fill)) {
      util_error("")
    }
    .meta_data_env$target_meta_data <- "item_level"
    lapply(setNames(nm = resp_vars), function(rv) {
      fillers <- names(.meta_data_env)
      fillers <- fillers[vapply(FUN.VALUE = logical(1),
                                fillers,
                                function(f) {
                                  is.function(.meta_data_env[[f]])
                                })]
      can_fill <- intersect(names(.to_fill), fillers)
      to_fill[can_fill] <- # TODO: Find a solution to deliver NULL as missing.
        lapply(setNames(nm = can_fill), function(filler) {
          .meta_data_env[[filler]](rv)
        })
      to_fill$resp_vars <- rv
      to_fill
    })
  } else if ("variable_group" %in% names(.to_fill)) {
    if ("resp_vars" %in% names(.to_fill)) {
      util_error("")
    }
    .meta_data_env$target_meta_data <- "cross-item_level"
    VARIABLE_LIST <- "VARIABLE_LIST" # TODO: Move to 000_globals.R
    CHECK_LABEL <- "CHECK_LABEL" # TODO: Move to 000_globals.R
    vl <- meta_data_cross_item[[VARIABLE_LIST]]
    vl[util_empty(vl)] <- NA_character_
    nm <- meta_data_cross_item[[CHECK_LABEL]]
    nm[util_empty(nm)] <- NA_character_
    if (any(is.na(nm))) {
      util_warning("Removing rows from %s because %s is missing.",
                   sQuote("cross-item_level"),
                   sQuote(CHECK_LABEL),
                   applicability_problem = TRUE)
    }
    vl <- setNames(vl[!is.na(nm) & ! is.na(vl)],
                   nm = nm[!is.na(nm) & ! is.na(vl)])
    # TODO: only select vl that incorporate variables that match var_list
    lapply(vl, function(vg) {
      if (!util_empty(vg)) {
        on.exit(.meta_data_env$meta_data_cross_item <- meta_data_cross_item)
        .meta_data_env$meta_data_cross_item <- meta_data_cross_item[
          (is.na(meta_data_cross_item[[VARIABLE_LIST]]) & is.na(vg)) |
            (meta_data_cross_item[[VARIABLE_LIST]] == vg), , drop = FALSE]
        fillers <- names(.meta_data_env)
        fillers <- fillers[vapply(FUN.VALUE = logical(1),
                                  fillers,
                                  function(f) {
                                    is.function(.meta_data_env[[f]])
                                  })]
        can_fill <- intersect(names(.to_fill), fillers)
        to_fill[can_fill] <- # TODO: Find a solution to deliver NULL as missing.
          lapply(setNames(nm = can_fill), function(filler) {
            .meta_data_env[[filler]](vg)
          })

        variable_group <- names(util_parse_assignments(vg))
        variable_group <- util_find_var_by_meta(variable_group,
                                                meta_data,
                                                label_col = label_col,
                                                target = label_col,
                                                ifnotfound = variable_group)
        to_fill$variable_group <- variable_group
      }
      to_fill
    })
  } else {
    # util_error("") not an error, may be a function w/o entity form the int dim
    list(`[ALL]` = to_fill)
  }
}
