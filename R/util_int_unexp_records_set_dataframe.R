#' Check for unexpected data record set
#'
#' @description
#' This function tests that the identifiers match a provided record set. It is possible to
#' check for unexpected data record sets by study segments or to consider only selected
#' segments.
#'
#' @inheritParams .template_function_indicator
#' @param level [character] a character vector indicating whether the assessment should be conducted at the study level (level = "dataframe") or at the segment level (level = "segment").
#' @param id_vars_list [list] the list containing the identifier variables names to be used in the assessment.
#' @param identifier_name_list [list] the list that contains the name of the identifier to be used in the assessment. For the study level, corresponds to the names of the different data frames. For the segment level, indicates the name of the segments.
#' @param valid_id_table_list [list] the reference list with the identifier variable values.
#' @param meta_data_record_check_list [character] a character vector indicating the type of check to conduct, either "subset" or "exact".
#' @param ... not used
#'
#' @return a [list] with
#'   - `SegmentData`: data frame with the results of the quality check for unexpected data elements
#'   - `SegmentTable`: data frame with selected unexpected data elements check results, used for the data quality report.
#'   - `UnexpectedRecords`: vector with row indices of duplicated records, if any, otherwise NULL.
#'
#' @family integrity_indicator_functions
#' @concept integrity_indicator
#' @keywords internal
util_int_unexp_records_set_dataframe <- function(level = c("dataframe"),
                                  id_vars_list,
                                  identifier_name_list,
                                  valid_id_table_list,
                                  meta_data_record_check_list,
                                  meta_data_dataframe = "dataframe_level",
                                  ...,
                                  dataframe_level) {

  util_ck_arg_aliases()

  # 1. Dataframe level check ----

  # Checks/Prepare arguments ----
  level <- util_match_arg(level)

  if (missing(id_vars_list) &&
      missing(identifier_name_list) &&
      missing(valid_id_table_list) &&
      missing(meta_data_record_check_list) &&
      missing(meta_data_dataframe) &&
      formals()$meta_data_dataframe %in% prep_list_dataframes()) {
    meta_data_dataframe <- force(meta_data_dataframe)
  }

  if (missing(id_vars_list) &&
      missing(identifier_name_list) &&
      missing(valid_id_table_list) &&
      missing(meta_data_record_check_list) &&
      !missing(meta_data_dataframe)) {
      meta_data_dataframe <- prep_check_meta_data_dataframe(meta_data_dataframe)
      meta_data_dataframe <- meta_data_dataframe[
        vapply(meta_data_dataframe[[DF_NAME]],
               function(x) { !util_is_try_error(try(prep_get_data_frame(data_frame_name = x,
                                                                    keep_types = TRUE), silent = TRUE)) },
               FUN.VALUE = logical(1))
        , , drop = FALSE]
      meta_data_dataframe <- meta_data_dataframe[
        vapply(meta_data_dataframe[[DF_ID_REF_TABLE]],
               function(x) { !util_is_try_error(try(prep_get_data_frame(data_frame_name = x), silent = TRUE)) },
               FUN.VALUE = logical(1))
        , , drop = FALSE]
      # TODO: if nothing left
      id_vars_list <- lapply(setNames(meta_data_dataframe[[DF_ID_VARS]],
                                      nm = meta_data_dataframe[[DF_NAME]]),
                             util_parse_assignments,
                             multi_variate_text = TRUE );
      id_vars_list <- lapply(id_vars_list, unlist, recursive = TRUE)
      identifier_name_list <- meta_data_dataframe[[DF_NAME]];
      valid_id_table_list <- meta_data_dataframe[[DF_ID_REF_TABLE]];
      meta_data_record_check_list <- meta_data_dataframe[[DF_RECORD_CHECK]];
  } else if (!missing(meta_data_dataframe)) {
    util_error(c("I have %s and one of the following: %s.",
                 "This is not supported, please provide",
                 "either %s or all of %s."),
               sQuote("meta_data_dataframe"),
               util_pretty_vector_string(
                 c("id_vars_list",
                   "identifier_name_list",
                   "valid_id_table_list",
                   "meta_data_record_check_list"
                   )),
               sQuote("meta_data_dataframe"),
               util_pretty_vector_string(
                 c("id_vars_list",
                   "identifier_name_list",
                   "valid_id_table_list",
                   "meta_data_record_check_list"
                 )))
  } else if (missing(meta_data_dataframe) && (
    missing(id_vars_list) ||
    missing(identifier_name_list) ||
    missing(valid_id_table_list) ||
    missing(meta_data_record_check_list)
  )) {
    util_error(c("I don't have %s and also miss at least",
                 "one of the following: %s.",
                 "This is not supported, please provide",
                 "either %s or all of %s."),
               sQuote("meta_data_dataframe"),
               util_pretty_vector_string(
                 c("id_vars_list",
                   "identifier_name_list",
                   "valid_id_table_list",
                   "meta_data_record_check_list"
                 )),
               sQuote("meta_data_dataframe"),
               util_pretty_vector_string(
                 c("id_vars_list",
                   "identifier_name_list",
                   "valid_id_table_list",
                   "meta_data_record_check_list"
                 )))
  }
  .nrw <- length(identifier_name_list)

  util_stop_if_not(length(id_vars_list) == .nrw)
  util_stop_if_not(length(identifier_name_list) == .nrw)
  util_stop_if_not(length(valid_id_table_list) == .nrw)
  util_stop_if_not(length(meta_data_record_check_list) == .nrw)

  names(id_vars_list) <- identifier_name_list
  names(meta_data_record_check_list) <- identifier_name_list
  names(valid_id_table_list) <- identifier_name_list

  # Check for unexpected records  ----

  result <- lapply(setNames(nm = identifier_name_list), function(current_df) {

    # Convert data from list to data frame
    # data_current_df <- do.call(rbind.data.frame, study_data[current_df])
    data_current_df <- util_expect_data_frame(current_df, dont_assign = TRUE)

    valid_id_table <- valid_id_table_list[[current_df]]
    util_expect_data_frame(valid_id_table)

    id_vars <- id_vars_list[[current_df]] # TODO: id_vars may be unavailable.
    id_vars <- id_vars[!util_empty(id_vars)]

    if (length(id_vars) == 0) {
      util_warning(
        "No %s defined in %s, skipping the check for unexpected record set",
        dQuote("DF_ID_VARS"),
        dQuote("meta_data_studies"),
        applicability_problem = TRUE,
        intrinsic_applicability_problem = TRUE
      )
      return(
        res_pipeline <- data.frame(
          "Level" = "Data frame",
          "NUM_int_sts_setrc" = 0,
          "PCT_int_sts_setrc" = 0,
          "GRADING" = 0,
          stringsAsFactors = FALSE
        )[FALSE, , drop = FALSE]
      )
    }

    # Select ids from data
    data_ids <- util_remove_empty_rows(data_current_df, id_vars = id_vars)

    metadata_ids <- valid_id_table
    metadata_ids <- metadata_ids[!util_empty(metadata_ids)]

    if (length(id_vars) > 1) {
      util_warning(c("Check for multiple IDs is not currently supported,",
                     "but you could assign value labels to the each variable"),
                   applicability_problem = TRUE,
                   intrinsic_applicability_problem = TRUE)
      return(
        res_pipeline <- data.frame(
          "Level" = "Data frame",
          "NUM_int_sts_setrc" = 0,
          "PCT_int_sts_setrc" = 0,
          "GRADING" = 0,
          stringsAsFactors = FALSE
        )[FALSE, , drop = FALSE]
      )

      # IDEA: supported by reference ids written as c("9871 | 4567", "id | exdate")
    }

    # Check membership of the id vectors
    unex_records_tmp <- data_ids[[id_vars]][!(data_ids[[id_vars]] %in%
                                                metadata_ids)]
    miss_records_tmp <- metadata_ids[!(metadata_ids %in%
                                         data_ids[[id_vars]])]

    # TODO: check definition of exact and subset
    if (all(data_ids[[id_vars]] %in% metadata_ids) &&
        all(metadata_ids %in% data_ids[[id_vars]])) {
      match_actual <- "exact"
    } else if (all(data_ids[[id_vars]] %in% metadata_ids)) {
      match_actual <- "subset"
    } else if (all(metadata_ids %in% data_ids[[id_vars]])) {
      match_actual <- "superset"
    } else {
      match_actual <- "mismatch"
    }
    match_expected <- meta_data_record_check_list[[current_df]]

    res_tmp <- data.frame(
      check.names = FALSE,
      "Check" = "Record set",
      "Data frame" = current_df,
      "Unexpected records in set?" = !(length(unex_records_tmp) == 0),
      "Number of records in data" = length(data_ids[, id_vars]),
      "Number of records in metadata" = length(metadata_ids),
      "Number of mismatches" =
        length(unex_records_tmp),
      "Percentage of mismatches" =
        abs(round(100 * length(unex_records_tmp) / length(metadata_ids), 3)),
      "Expected match type" = match_expected,
      "Actual match type" = match_actual,
      "GRADING" = ifelse((match_expected == match_actual), 0, 1),
      stringsAsFactors = FALSE
    )

    if (any(!util_empty(unex_records_tmp))) {
      vec_unex_records <- unex_records_tmp[!util_empty(unex_records_tmp)]
    } else {
      vec_unex_records <- NULL
    }

    mism_lin <- as.character(which(!(data_ids[[id_vars]] %in%
                                   metadata_ids)))

    return(list(res_tmp, data.frame(Dataframe = rep(current_df,
                                            length(vec_unex_records)),
                                    UnexpectedID = vec_unex_records,
                                    Line = mism_lin)))
  })

  res_df <- do.call(rbind.data.frame, lapply(result, `[[`, 1))
  unex_records_df <- dplyr::bind_rows(lapply(result, `[[`, 2))

  res_pipeline <- data.frame(
    "Level" = "Dataframe",
    "DF_NAME" = res_df[, 2],
    "NUM_int_sts_setrc" = res_df[, 6],
    "PCT_int_sts_setrc" = res_df[, 7],
    "GRADING" = res_df$GRADING,
    stringsAsFactors = FALSE
  )

  return(list(
    DataframeData = res_df,
    DataframeTable = res_pipeline,
    Other = unex_records_df
  ))

}
