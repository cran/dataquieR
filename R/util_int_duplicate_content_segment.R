#' Check for duplicated content
#'
#' @description
#' This function tests for duplicates entries in the data set. It is possible to
#' check duplicated entries by study segments or to consider only selected
#' segments.
#'
#' @param level [character] a character vector indicating whether the assessment should be conducted at the study level (level = "dataframe") or at the segment level (level = "segment").
#' @param identifier_name_list [vector] the vector that contains the name of the identifier to be used in the assessment. For the study level, corresponds to the names of the different data frames. For the segment level, indicates the name of the segments.
#' @param study_data [data.frame] the data frame that contains the measurements, mandatory.
#' @param meta_data [data.frame] the data frame that contains metadata attributes of the study data, mandatory.
#' @param id_vars_list [list] the list containing the identifier variables names to be used in the assessment.
#' @param unique_rows [vector] named. for each segment, either true/false or `no_id` to exclude ID variables from check
#'
#' @inheritParams .template_function_indicator
#'
#' @return a [list] with
#'   - `SegmentData`: data frame with the results of the quality check for duplicated entries
#'   - `SegmentTable`: data frame with selected duplicated entries check results, used for the data quality report.
#'   - `Other`: vector with row indices of duplicated entries, if any, otherwise NULL.
#'
#' @family integrity_indicator_functions
#' @concept integrity_indicator
#' @keywords internal
util_int_duplicate_content_segment <- function(level = c("segment"),
                                               identifier_name_list,
                                               id_vars_list,
                                               unique_rows,
                                               study_data,
                                               meta_data, # item_level,
                                               meta_data_segment = "segment_level",
                                               segment_level) {
  label_col <- VAR_NAMES
  util_ck_arg_aliases()

  # Segment level check ----

  # Check arguments ----

  level <- util_match_arg(level)

  if (missing(identifier_name_list) &&
      missing(id_vars_list) &&
      missing(meta_data_segment) &&
      formals()$meta_data_segment %in% prep_list_dataframes()) {
    meta_data_segment <- force(meta_data_segment)
  }

  if (missing(identifier_name_list) &&
      missing(id_vars_list) &&
      !missing(meta_data_segment)) {
    meta_data_segment <- prep_check_meta_data_segment(meta_data_segment)
    meta_data_segment <-
      meta_data_segment[
        !util_empty(meta_data_segment[[SEGMENT_UNIQUE_ROWS]]) & (
        trimws(tolower(meta_data_segment[[SEGMENT_UNIQUE_ROWS]])) ==
          "no_id" |
        !util_is_na_0_empty_or_false(
          meta_data_segment[[SEGMENT_UNIQUE_ROWS]])), , drop = FALSE]
    # TODO: if nothing left
    identifier_name_list <- meta_data_segment[[STUDY_SEGMENT]];
    id_vars_list <- lapply(setNames(meta_data_segment[[SEGMENT_ID_VARS]], # TODO: use the constants everywhere: meta_data_segment[[SEGMENT_ID_VARS]], not ...$SEGMENT_ID_VARS
                                    nm = meta_data_segment[[STUDY_SEGMENT]]),
                           util_parse_assignments,
                           multi_variate_text = TRUE
    )
    id_vars_list <- lapply(id_vars_list, unlist, recursive = TRUE)
    unique_rows <- setNames(meta_data_segment[[SEGMENT_UNIQUE_ROWS]],
                            nm = meta_data_segment[[STUDY_SEGMENT]])
  } else if (!missing(meta_data_segment)) {
    util_error(c("I have %s and one of the following: %s.",
                 "This is not supported, please provide",
                 "either %s or all of %s."),
               sQuote("meta_data_segment"),
               util_pretty_vector_string(
                 c("identifier_name_list",
                   "id_vars_list"
                 )),
               sQuote("meta_data_segment"),
               util_pretty_vector_string(
                 c("identifier_name_list",
                   "id_vars_list"
                 )))
  } else if (missing(meta_data_segment) && (
    missing(identifier_name_list) ||
    missing(id_vars_list)
  )) {
    util_error(c("I don't have %s and also miss at least",
                 "one of the following: %s.",
                 "This is not supported, please provide",
                 "either %s or all of %s."),
               sQuote("meta_data_segment"),
               util_pretty_vector_string(
                 c("identifier_name_list",
                   "id_vars_list"
                 )),
               sQuote("meta_data_segment"),
               util_pretty_vector_string(
                 c("identifier_name_list",
                   "id_vars_list"
                 )))
  }

  util_expect_scalar(identifier_name_list,
                     allow_null = TRUE,
                     allow_more_than_one = TRUE,
                     check_type = is.character)

  if (missing(unique_rows)) {
    unique_rows <- setNames(rep(NA_character_, length(identifier_name_list)),
                            identifier_name_list);
  }
  util_expect_scalar(unique_rows, allow_more_than_one = TRUE,
                     check_type =
                       function(x) {
                         all(util_empty(x) | tolower(trimws(x)) %in%
                               c("f", "t", "true", "false", "no_id"))
                       })
  unique_rows <- tolower(trimws(unique_rows))
  unique_rows[util_empty(unique_rows)] <- "false"

  # map metadata to study data
  prep_prepare_dataframes(.allow_empty = TRUE)

  # check that specified segments are included in the metadata
  old_identifier_name_list <- identifier_name_list
  identifier_name_list <- intersect(identifier_name_list,
                                 meta_data[[STUDY_SEGMENT]])

  if (length(old_identifier_name_list) > length(identifier_name_list)) {
    util_message(
      "The segments in the %s do not match the segments in %s, considering only the intersection",
      dQuote("meta_data"),
      dQuote("meta_data_segment"),
      applicability_problem = TRUE,
      intrinsic_applicability_problem = FALSE
    )
  }

  # Check for duplicated content  ----

  result <- lapply(setNames(nm = identifier_name_list), function(current_segment) {
    vars_in_current_segment <-
      util_get_vars_in_segment(segment = current_segment,
                               meta_data = meta_data,
                               label_col = VAR_NAMES)

    if (trimws(tolower(unique_rows[[current_segment]])) == "no_id") {
      vars_in_current_segment <- setdiff(vars_in_current_segment,
                                         id_vars_list[[current_segment]])
    }

    ds1_seg <- ds1[, intersect(colnames(ds1), vars_in_current_segment), FALSE]

    ds1_seg <- util_remove_empty_rows(ds1_seg)

    n_uniq <- nrow(unique(ds1_seg))

    res_tmp <- data.frame(
      check.names = FALSE,
      "Check" = "Duplicate records",
      "Segment" = current_segment,
      "Any duplicates" = ifelse(n_uniq < nrow(ds1_seg), TRUE, FALSE),
      "Number of duplicates" = nrow(ds1_seg) - n_uniq,
      "Percentage of duplicates" =
        round(100 * (nrow(ds1_seg) - n_uniq) / nrow(ds1_seg), 3),
      "GRADING" = ifelse(n_uniq < nrow(ds1_seg), 1, 0),
      stringsAsFactors = FALSE
    )

    if (res_tmp[[3]]) { # only if there are any duplicated observations
      vec_dup <- which(duplicated(ds1_seg)) # FIXME: the positions in the reduced data frame are mostly useless
      vec_dup <- NULL # TODO: Remove after fixing
    } else {
      vec_dup <- NULL
    }

    return(list(res_tmp, vec_dup))
  })

  res_df <- do.call(rbind.data.frame, lapply(result, `[[`, 1))
  duplicates_df <- do.call(cbind.data.frame, lapply(result, `[[`, 2))

  res_pipeline <- data.frame(
    "Segment" = res_df$Segment,
    "NUM_int_sts_dupl_content" = res_df$`Number of duplicates`, # TODO: Nowhere use numbers, always names, and return ordere here to use standardized names. labels for the not-pipelin-res can be read from an external file, late
    "PCT_int_sts_dupl_content" = res_df$`Percentage of duplicates`,
    "GRADING" = res_df$GRADING,
    stringsAsFactors = FALSE
  )

  return(list(
    SegmentData = res_df,
    SegmentTable = res_pipeline,
    Other = duplicates_df
  ))


}

