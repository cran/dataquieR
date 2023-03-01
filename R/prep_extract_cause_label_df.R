#' Extract all missing/jump codes from metadata and export a
#' cause-label-data-frame
#'
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#'
#' @return [list] with the entries
#'   - `meta_data` [data.frame] a data frame that contains updated metadata
#'   - `cause_label_df` [data.frame] missing code table. If missing codes have
#'                                    labels the respective data frame are
#'                                    specified here, see [cause_label_df]
#'
#' @export
#'
#' @seealso [prep_add_cause_label_df]
prep_extract_cause_label_df <- function(meta_data = "item_level",
                                       label_col = VAR_NAMES) {
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
                                     suppressWarnings(lubridate::as_datetime(
                                       names(x)))),
                                   CODE_LABEL = unname(unlist(x)))
                 prfx <- as.character(dfr$CODE_LABEL) ==
                   as.character(dfr$CODE_VALUE)
                 prfx <- prfx[!is.na(prfx)]
                 if (any(prfx, na.rm = TRUE)) {
                   dfr[prfx, "CODE_LABEL"] <-
                     paste(cls, dfr[prfx, "CODE_LABEL"])
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
                   dfr[prfx, "CODE_LABEL"] <-
                     paste(cls, dfr[prfx, "CODE_LABEL"])
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

  ml2 <- ml
  jl2 <- jl

  coll <- sprintf(" %s ", SPLIT_CHAR)
  coll_dt <- function(x)
    paste(na.omit(suppressWarnings(lubridate::as_datetime(names(x)))),
          collapse = coll)
  coll_ndt <- function(x)
    paste(na.omit(suppressWarnings(as.numeric(names(x)))),
          collapse = coll)

  ml_empty_on_purpose <- trimws(meta_data$MISSING_LIST) == SPLIT_CHAR
  ml2[dt] <- vapply(ml[dt], coll_dt, FUN.VALUE = character(1))
  ml2[!dt] <- vapply(ml[!dt], coll_ndt, FUN.VALUE = character(1))
  ml2[trimws(ml2) == ""] <- NA
  meta_data$MISSING_LIST <- unlist(ml2, recursive = FALSE)

  jl_empty_on_purpose <- trimws(meta_data$JUMP_LIST) == SPLIT_CHAR
  jl2[dt] <- vapply(jl[dt], coll_dt, FUN.VALUE = character(1))
  jl2[!dt] <- vapply(jl[!dt], coll_ndt, FUN.VALUE = character(1))
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
