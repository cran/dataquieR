#' Check for unexpected data record set
#'
#' @description
#' This function tests that the identifiers match a provided record set. It is possible to
#' check for unexpected data record sets by study segments or to consider only selected
#' segments.
#'
#' @param level [character] a character vector indicating whether the assessment should be conducted at the study level (level = "dataframe") or at the segment level (level = "segment").
#' @param id_vars_list [list] the list containing the identifier variables names to be used in the assessment.
#' @param identifier_name_list [list] the list that contains the name of the identifier to be used in the assessment. For the study level, corresponds to the names of the different data frames. For the segment level, indicates the name of the segments.
#' @param valid_id_table_list [list] the reference list with the identifier variable values.
#' @param meta_data_record_check_list [character] a character vector indicating the type of check to conduct, either "subset" or "exact".
#'
#' @return a [list] with
#'   - `SegmentData`: data frame with the results of the quality check for unexpected data elements
#'   - `SegmentTable`: data frame with selected unexpected data elements check results, used for the data quality report.
#'   - `UnexpectedRecords`: vector with row indices of duplicated records, if any, otherwise NULL.
#'
util_int_unexp_records_set_dataframe <- function(level = c("dataframe"),
                                  id_vars_list, # TODO: Don't pass all columns separately
                                  identifier_name_list,
                                  valid_id_table_list,
                                  meta_data_record_check_list) {



  # 1. Dataframe level check ----

  # Checks/Prepare arguments ----
  level <- util_match_arg(level)

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
        "No %d defined in %d, skipping the check for unexpected record set",
        dQuote("DF_ID_VARS"),
        dQuote("meta_data_studies")
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
      util_warning("Check for mutliple IDs is not currently supported")
      return(
        res_pipeline <- data.frame(
          "Level" = "Data frame",
          "NUM_int_sts_setrc" = 0,
          "PCT_int_sts_setrc" = 0,
          "GRADING" = 0,
          stringsAsFactors = FALSE
        )[FALSE, , drop = FALSE]
      )

      # TODO: supported by reference ids written as c("9871 | 4567", "id | exdate")
    }

    # Check membership of the id vectors
    unex_records_tmp <- setdiff(data_ids[[id_vars]], metadata_ids)

    # TODO: check definition of exact and subset
    if (length(unex_records_tmp) == 0) {
      match_actual <- "exact"
    } else if (length(unex_records_tmp) > 0) {
      match_actual <- "subset"
    }
    match_expected <- meta_data_record_check_list[current_df]

    res_tmp <- data.frame(
      check.names = FALSE,
      "Check" = "Record set",
      "Data frame" = current_df,
      "Unexpected records in set" = !(length(unex_records_tmp) == 0),
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

    return(list(res_tmp, vec_unex_records))
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
    UnexpectedRecords = unex_records_df
  ))

}
