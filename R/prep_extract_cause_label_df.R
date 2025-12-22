#' Extract all missing/jump codes from metadata and export a
#' cause-label-data-frame
#'
#' @param item_level [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param meta_data [data.frame] old name for `item_level`
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param meta_data_v2 [character] path to workbook like metadata file, see
#'                                 [`prep_load_workbook_like_file`] for details.
#'                                 **ALL LOADED DATAFRAMES WILL BE PURGED**,
#'                                 using [`prep_purge_data_frame_cache`],
#'                                 if you specify `meta_data_v2`.
#'
#' @return [list] with the entries
#'   - `meta_data` [data.frame] a data frame that contains updated metadata --
#'                              you still need to add a column
#'                              [MISSING_LIST_TABLE] and add the
#'                              `cause_label_df` as such to the metadata
#'                              cache using [prep_add_data_frames()], manually.
#'   - `cause_label_df` [data.frame] missing code table. If missing codes have
#'                                    labels the respective data frame are
#'                                    specified here, see [cause_label_df].
#'
#' @export
#'
#' @seealso [prep_add_cause_label_df]
prep_extract_cause_label_df <- function(item_level = "item_level",
                                       label_col = VAR_NAMES,
                                       meta_data_v2,
                                       meta_data = item_level) {
  util_maybe_load_meta_data_v2()
  util_expect_scalar(label_col, check_type = is.character)
  util_expect_data_frame(meta_data, c(label_col, DATA_TYPE))
  if (JUMP_LIST %in% colnames(meta_data)) {
    jl <- util_parse_assignments(meta_data$JUMP_LIST,
                                 multi_variate_text = TRUE)
  } else {
    jl <- lapply(seq_len(nrow(meta_data)),
                 function(x) setNames(list(NA), NA_character_))
  }

  if (MISSING_LIST %in% colnames(meta_data)) {
    ml <- util_parse_assignments(meta_data$MISSING_LIST,
                               multi_variate_text = TRUE)
  } else {
    ml <- lapply(seq_len(nrow(meta_data)),
                 function(x) setNames(list(NA), NA_character_))
  }

  to_df <- function(l, cls) {
    cause_label_df <- mapply(l, meta_data[[label_col]],
                             meta_data[[DATA_TYPE]], FUN =
             function(x, v, tp) {
               if (!is.na(tp) && tp == DATA_TYPES$DATETIME) {
                 dfr <- data.frame(CODE_VALUE =
                                     as.character(
                                     suppressWarnings(util_parse_date(
                                       names(x)))),
                                   CODE_LABEL = unname(unlist(x)))
                 prfx <- as.character(dfr$CODE_LABEL) ==
                   as.character(dfr$CODE_VALUE)
                 prfx <- prfx[!is.na(prfx)]
                 if (any(prfx, na.rm = TRUE)) {
                   dfr[prfx, CODE_LABEL] <-
                     paste(cls, dfr[prfx, CODE_LABEL])
                 }
                 dfr$CODE_CLASS <- rep(cls, nrow(dfr))
                 dfr$resp_vars <- rep(v, nrow(dfr))
                 dfr[!is.na(dfr$CODE_VALUE), , FALSE]
               } else if (!is.na(tp) && tp == DATA_TYPES$TIME) {
                 dfr <- data.frame(CODE_VALUE =
                                     as.character(
                                       suppressWarnings(util_parse_time(
                                         names(x)))),
                                   CODE_LABEL = unname(unlist(x)))
                 prfx <- as.character(dfr$CODE_LABEL) ==
                   as.character(dfr$CODE_VALUE)
                 prfx <- prfx[!is.na(prfx)]
                 if (any(prfx, na.rm = TRUE)) {
                   dfr[prfx, CODE_LABEL] <-
                     paste(cls, dfr[prfx, CODE_LABEL])
                 }
                 dfr$CODE_CLASS <- rep(cls, nrow(dfr))
                 dfr$resp_vars <- rep(v, nrow(dfr))
                 dfr[!is.na(dfr$CODE_VALUE), , FALSE]
               } else {
                 dfr <- data.frame(CODE_VALUE =
                                     as.character(
                                     suppressWarnings(as.numeric(names(x)))),
                                   CODE_LABEL = unname(unlist(x)))
                 prfx <- dfr$CODE_LABEL == dfr$CODE_VALUE
                 prfx <- prfx[!is.na(prfx)]
                 if (any(prfx, na.rm = TRUE)) {
                   dfr[prfx, CODE_LABEL] <-
                     paste(cls, dfr[prfx, CODE_LABEL])
                 }
                 dfr$CODE_CLASS <- rep(cls, nrow(dfr))
                 dfr$resp_vars <- rep(v, nrow(dfr))
                 dfr[!is.na(dfr$CODE_VALUE), , FALSE]
               }
             }, SIMPLIFY = FALSE)
    do.call(rbind.data.frame, cause_label_df)
  }

  cause_label_df <- rbind.data.frame(to_df(jl, "JUMP"), to_df(ml, "MISSING"))

  dt <- !is.na(meta_data[[DATA_TYPE]]) &
    meta_data[[DATA_TYPE]] == DATA_TYPES$DATETIME

  to <- !is.na(meta_data[[DATA_TYPE]]) &
    meta_data[[DATA_TYPE]] == DATA_TYPES$TIME

  ml2 <- ml
  jl2 <- jl

  coll <- sprintf(" %s ", SPLIT_CHAR)
  coll_to <- function(x)
    paste(.util_format_hms(
      na.omit(suppressWarnings(util_parse_time(names(x))))),
          collapse = coll)
  coll_dt <- function(x)
    paste(na.omit(suppressWarnings(util_parse_date(names(x)))),
          collapse = coll)
  coll_ndt <- function(x)
    paste(na.omit(suppressWarnings(as.numeric(names(x)))),
          collapse = coll)

  ml_empty_on_purpose <- trimws(meta_data$MISSING_LIST) == SPLIT_CHAR
  ml2[dt] <- vapply(ml[dt], coll_dt, FUN.VALUE = character(1))
  ml2[to] <- vapply(ml[to], coll_to, FUN.VALUE = character(1))
  ml2[!dt & !to] <- vapply(ml[!dt & !to], coll_ndt, FUN.VALUE = character(1))
  ml2[trimws(ml2) == ""] <- NA
  meta_data$MISSING_LIST <- unlist(ml2, recursive = FALSE)

  jl_empty_on_purpose <- trimws(meta_data$JUMP_LIST) == SPLIT_CHAR
  jl2[dt] <- vapply(jl[dt], coll_dt, FUN.VALUE = character(1))
  jl2[to] <- vapply(jl[to], coll_to, FUN.VALUE = character(1))
  jl2[!dt & !to] <- vapply(jl[!dt & !to], coll_ndt, FUN.VALUE = character(1))
  jl2[trimws(jl2) == ""] <- NA
  meta_data$JUMP_LIST <- unlist(jl2, recursive = FALSE)

  meta_data$MISSING_LIST[is.na(meta_data$MISSING_LIST) & ml_empty_on_purpose] <-
    SPLIT_CHAR

  meta_data$JUMP_LIST[is.na(meta_data$JUMP_LIST) & jl_empty_on_purpose] <-
    SPLIT_CHAR


  return(list(
    meta_data = meta_data,
    cause_label_df = cause_label_df
  ))
}
