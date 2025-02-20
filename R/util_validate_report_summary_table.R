#' Verify the class `ReportSummaryTable`
#'
#' @param tb [data.frame] object to be a `ReportSummaryTable`
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data. Used to translate
#'                               variable names, if given.
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @return [data.frame] maybe fixed `ReportSummaryTable`
#' @keywords internal
util_validate_report_summary_table <- function(tb, meta_data, label_col) {
  util_expect_data_frame(tb, c("Variables", "N"))
  # test, all columns except Variables numeric
  util_stop_if_not(
  `ReportSummaryTable: need to have numeric columns only (except "Variables")` =
                     all(vapply(tb[, setdiff(colnames(tb), c("Variables"))],
         is.numeric, FUN.VALUE = logical(1))))
  if (!is.factor(tb$Variables)) {
    tb$Variables <- as.character(tb$Variables)
  }
  util_stop_if_not(
    `ReportSummaryTables: The column Variables needs to be w/o NAs` =
      all(!is.na(tb$Variables)))

  tb_var_names <- NULL
  if (!is.null(attr(tb, "VAR_NAMES"))) {
    tb_var_names <- attr(tb, "VAR_NAMES")
    if (length(tb_var_names) != nrow(tb)) tb_var_names <- NULL
  }
  if (!missing(meta_data) && !missing(label_col)) {
    if (!(VARIABLE_ORDER %in% colnames(meta_data))) {
      meta_data[[VARIABLE_ORDER]] <-
        seq_len(nrow(meta_data))
    }
    # varorder <- prep_get_labels(
    #   resp_vars = tb$Variables,
    #   item_level = meta_data,
    #   label_col = VARIABLE_ORDER,
    #   resp_vars_match_label_col_only = TRUE,
    #   label_class = "SHORT",
    #   force_label_col = "TRUE"
    # )
    if (is.null(tb_var_names)) {
      tb_var_names <- try(prep_map_labels(
        as.character(tb$Variables),
        item_level = meta_data,
        to = VAR_NAMES,
        from = label_col), silent = TRUE)
      if (inherits(tb_var_names, "try-error")) tb_var_names <- NULL
    }
    if (is.factor(tb$Variables)) {
      levels(tb$Variables) <-
        prep_get_labels(
          resp_vars = levels(tb$Variables),
          item_level = meta_data,
          label_col = label_col,
          resp_vars_match_label_col_only = TRUE,
          label_class = "SHORT"
        )
    } else {
      tb$Variables <- prep_get_labels(
        resp_vars = tb$Variables,
        item_level = meta_data,
        label_col = label_col,
        resp_vars_match_label_col_only = TRUE,
        label_class = "SHORT"
      )
    }
    # tb[] <- tb[order(varorder), , FALSE]
  } else if (!missing(meta_data) || !missing(label_col)) {
    util_error(
      "ReportSummaryTable: Need to have either %s and %s or neither of them",
      sQuote("meta_data"), sQuote("label_col"))
  }
  # 'Variables' should be unique
  if (any(duplicated(tb$Variables))) {
    nm_dup <- tb$Variables[duplicated(tb$Variables)]
    ind_dup <- which(tb$Variables %in% nm_dup)
    if (!is.null(tb_var_names)) {
      tb$Variables[ind_dup] <-
        paste0(tb$Variables[ind_dup], " (", tb_var_names[ind_dup], ")")
    } else {
      tb[ind_dup, ] <- tb[ind_dup, ] %>%
        dplyr::group_by(Variables) %>%
        dplyr::mutate(Variables =
                        paste(Variables, paste0("(", dplyr::row_number(), ")"))) %>%
        as.data.frame()
    }
  }
  # If the first try fails, fix duplicates here strictly:
  if (any(duplicated(tb$Variables))) {
    nm_dup <- tb$Variables[duplicated(tb$Variables)]
    ind_dup <- which(tb$Variables %in% nm_dup)
    tb$Variables[ind_dup] <- paste(tb$Variables[ind_dup], seq_along(ind_dup))
  }
  util_stop_if_not(
    `ReportSummaryTables: The column Variables needs to be w/o duplicates` =
      !any(duplicated(tb$Variables)))
  # TODO: Also verify attributes
  class(tb) <-
    union("ReportSummaryTable", class(tb))
  if (is.null(attr(tb, "VAR_NAMES")) && !is.null(tb_var_names)) {
    attr(tb, "VAR_NAMES") <- tb_var_names
  }
  tb
}
