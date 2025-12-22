#' Convert missing codes in metadata format v1.0 and a missing-cause-table
#' to v2.0 missing list / jump list assignments
#'
#' The function has to working modes. If `replace_meta_data` is `TRUE`, by
#' default, if `cause_label_df` contains a column
#' named `resp_vars`, then the missing/jump codes in
#' `meta_data[, c(MISSING_CODES, JUMP_CODES)]` will be overwritten, otherwise,
#' it will be labeled using the `cause_label_df`.
#'
#' If a column `resp_vars` exists, then rows with a value in `resp_vars` will
#' only be used for the corresponding variable.
#'
#' @param item_level [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param meta_data [data.frame] old name for `item_level`
#' @param cause_label_df [data.frame] missing code table. If missing codes have
#'                                    labels the respective data frame can be
#'                                    specified here, see [cause_label_df]
#' @param assume_consistent_codes [logical] if TRUE and no labels are given and
#'                                          the same missing/jump code is used
#'                                          for more than one variable, the
#'                                          labels assigned for this code will
#'                                          be the same for all variables.
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param replace_meta_data [logical] if `TRUE`, ignore existing missing codes
#'                                    and jump codes and replace them with
#'                                    data from the `cause_label_df`. Otherwise,
#'                                    copy the labels from `cause_label_df` to
#'                                    the existing code columns.
#' @param meta_data_v2 [character] path to workbook like metadata file, see
#'                                 [`prep_load_workbook_like_file`] for details.
#'                                 **ALL LOADED DATAFRAMES WILL BE PURGED**,
#'                                 using [`prep_purge_data_frame_cache`],
#'                                 if you specify `meta_data_v2`.
#' @return [data.frame] updated metadata including all the code labels in
#'                      missing/jump lists
#' @seealso [prep_extract_cause_label_df]
#'
#' @export
#'
prep_add_cause_label_df <- function(item_level = "item_level",
                                    cause_label_df,
                                    label_col = VAR_NAMES,
                                    assume_consistent_codes = TRUE,
                                    replace_meta_data =
                                      ("resp_vars" %in%
                                         colnames(cause_label_df)),
                                    meta_data = item_level,
                                    meta_data_v2) {

  util_maybe_load_meta_data_v2()

  util_expect_data_frame(meta_data, c(DATA_TYPE))
  util_expect_data_frame(cause_label_df, c(CODE_VALUE, CODE_LABEL))
  if (CODE_CLASS %in% colnames(cause_label_df)) {
    cause_label_df[[CODE_CLASS]][is.na(cause_label_df[[CODE_CLASS]])] <-
      ""
  }
  if (length(label_col) != 1 ||
      is.na(label_col) ||
      !is.character(label_col)) {
    util_error("Need one character value in %s",
               dQuote("label_col"))
  }
  if (!label_col %in% colnames(meta_data)) {
    util_error("No column %s in %s", dQuote(label_col), dQuote("meta_data"))
  }
  if (length(assume_consistent_codes) != 1 ||
      is.na(assume_consistent_codes) ||
      !is.logical(assume_consistent_codes)) {
    util_error("Need one logical value in %s",
               dQuote("assume_consistent_codes"))
  }
  if (length(replace_meta_data) != 1 ||
      is.na(replace_meta_data) ||
      !is.logical(replace_meta_data)) {
    util_error("Need one logical value in %s", dQuote("replace_meta_data"))
  }

  have_resp_vars_col <- ("resp_vars" %in%
                           colnames(cause_label_df))

  amend_cl <- function(rv, lst, tp) {
    lst_nm <- sprintf("%s_LIST", lst)
    if (lst_nm %in% colnames(meta_data)) {
      cl <- meta_data[meta_data[[label_col]] == rv, lst_nm, TRUE]
    } else {
      cl <- NA_character_
    }
    if (any(is.na(cl)) && !replace_meta_data) {
      NA_character_
    } else {
      if (CODE_CLASS %in% colnames(cause_label_df)) {
        cause_label_df <- cause_label_df[
                                 cause_label_df[[CODE_CLASS]] == lst, , FALSE]
      } else if (replace_meta_data && lst != "MISSING") {
        cause_label_df <- subset(cause_label_df, FALSE)
      }
      if (have_resp_vars_col) {
        cause_label_df <- subset(cause_label_df,
                                 is.na(get("resp_vars")) |
                                 trimws(get("resp_vars")) == "" |
                                 get("resp_vars") == rv)
      }
      if (replace_meta_data) {
        cls <- cause_label_df$CODE_VALUE
      } else {
        if (tp %in% c(DATA_TYPES$DATETIME)) {
          cls <- util_parse_date(names(util_parse_assignments(cl)))
        } else if (tp %in% c(DATA_TYPES$TIME)) {
          cls <- util_parse_time(names(util_parse_assignments(cl)))
        } else {
          cls <- as.integer(names(util_parse_assignments(cl)))
        }
      }
      cldf <- cause_label_df[
                     as.character(cause_label_df[[CODE_VALUE]]) %in% as.character(cls),
                     , FALSE]
      miss <- cls[!(as.character(cls) %in%
                      as.character(cause_label_df[[CODE_VALUE]]))]
      if (length(miss) > 0) {
        if (assume_consistent_codes) {
          cldf1 <- data.frame(CODE_VALUE = miss,
                              CODE_LABEL = paste(lst, miss),
                              stringsAsFactors = FALSE)
        } else {
          cldf1 <- data.frame(CODE_VALUE = miss,
                              CODE_LABEL = paste(lst, rv, miss),
                              stringsAsFactors = FALSE)
        }
        cldf <- util_rbind(cldf, cldf1)
      }
      prep_deparse_assignments(codes = cldf$CODE_VALUE,
                               labels = cldf$CODE_LABEL)
    }
  }

  meta_data$MISSING_LIST <-
    vapply(mapply(
      rv = meta_data[[label_col]],
      tp = meta_data[[DATA_TYPE]],
      FUN = amend_cl,
      MoreArgs = list(lst = "MISSING")),
      as.character,
      FUN.VALUE = character(1))

  meta_data$JUMP_LIST <-
    vapply(mapply(
      rv = meta_data[[label_col]],
      tp = meta_data[[DATA_TYPE]],
      FUN = amend_cl,
      MoreArgs = list(lst = "JUMP")),
      as.character,
      FUN.VALUE = character(1))

  meta_data
}
