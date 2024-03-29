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
#' @param study_data [data.frame] the data frame that contains the measurements, mandatory.
#' @param meta_data [data.frame] the data frame that contains metadata attributes of the study data, mandatory.
#'
#' @return a [list] with
#'   - `SegmentData`: data frame with the results of the quality check for unexpected data elements
#'   - `SegmentTable`: data frame with selected unexpected data elements check results, used for the data quality report.
#'   - `UnexpectedRecords`: vector with row indices of duplicated records, if any, otherwise NULL.
#'
#' @examples
#' \dontrun{
#' study_data <- readRDS(system.file("extdata", "ship.RDS",
#'   package = "dataquieR"
#' ))
#' meta_data <- readRDS(system.file("extdata", "ship_meta.RDS",
#'   package = "dataquieR"
#' ))
#' md1_segment <- readRDS(system.file("extdata", "meta_data_segment.RDS",
#'   package = "dataquieR"
#' ))
#' ids_segment <- readRDS(system.file("extdata", "meta_data_ids_segment.RDS",
#'   package = "dataquieR"
#' ))
#'
#' # TODO: update examples
#' int_unexp_records_set(
#'   level = "segment",
#'   identifier_name_list = c("INTERVIEW", "LABORATORY"),
#'   valid_id_table_list = ids_segment,
#'   meta_data_record_check = md1_segment[,
#'     c("STUDY_SEGMENT", "SEGMENT_RECORD_CHECK")],
#'   study_data = study_data,
#'   meta_data = meta_data
#' )
#' }
#'
#' @family integrity_indicator_functions
#' @concept integrity_indicator
#' @keywords internal
util_int_unexp_records_set_segment <- function(level = c("segment"),
                                  id_vars_list,
                                  identifier_name_list,
                                  valid_id_table_list,
                                  meta_data_record_check_list,
                                  study_data, meta_data) {



  # 1. Segment level check ----

  # Checks arguments ----
  level <- util_match_arg(level)

  .nrw <- length(identifier_name_list)

  util_stop_if_not(length(id_vars_list) == .nrw)
  util_stop_if_not(length(identifier_name_list) == .nrw)
  util_stop_if_not(length(valid_id_table_list) == .nrw)
  util_stop_if_not(length(meta_data_record_check_list) == .nrw)

  names(id_vars_list) <- identifier_name_list
  names(meta_data_record_check_list) <- identifier_name_list
  names(valid_id_table_list) <- identifier_name_list

  prep_prepare_dataframes()

  # meta_data$STUDY_SEGMENT <-
  #   util_map_labels(meta_data$STUDY_SEGMENT,
  #                   to = label_col,
  #                   meta_data = meta_data,
  #                   ifnotfound = meta_data$STUDY_SEGMENT)


  # Checks segments ----

  segments <- identifier_name_list

  # TODO: check
  if (missing(segments) || is.null(segments)) {
    # assign all segments
    meta_data_segment_reduced_record <- data.frame()
    segments <- meta_data_segment_reduced_record$STUDY_SEGMENT
  }

  # check that specified segments are included in the metadata
  old_segments <- segments
  segments <- intersect(segments, meta_data$STUDY_SEGMENT)

  if (length(old_segments) > length(segments)) {
    util_warning(
      "The segments in the %s do not match the segments in %s, considering only the intersection",
      dQuote("meta_data"),
      dQuote("meta_data_segment"),
      applicability_problem = TRUE,
      intrinsic_applicability_problem = TRUE
    )
  }

  # Check for unexpected records  ----
  names(meta_data_record_check_list) <- segments
  names(valid_id_table_list) <- segments

  result <- lapply(setNames(nm = segments), function(current_segment) {
    valid_id_table <- valid_id_table_list[[current_segment]]
    util_expect_data_frame(valid_id_table)

    vars_in_current_segment <- meta_data[
      meta_data[[STUDY_SEGMENT]] == current_segment,
      label_col,
      drop = TRUE]

    id_vars <- id_vars_list[[current_segment]]
    id_vars <- id_vars[!util_empty(id_vars)]

    if (length(id_vars) == 0) {
      util_warning(
        "No %d defined in %d, skipping the check for unexpected record set",
        dQuote("SEGMENT_ID_VARS"),
        dQuote("meta_data_segment"),
        applicability_problem = TRUE,
        intrinsic_applicability_problem = TRUE
      )
      return(
        res_pipeline <- data.frame(
          "Level" = "Segment",
          "NUM_int_sts_setrc" = 0,
          "PCT_int_sts_setrc" = 0,
          "GRADING" = 0,
          stringsAsFactors = FALSE
        )[FALSE, , drop = FALSE]
      )
    }

    id_vars <-
      util_ensure_in(id_vars,
                     colnames(ds1),
                     err_msg =
                       c(sprintf("ID variables in current segment %s",
                                 dQuote(current_segment)),
                         ": Missing %s from the study data,",
                         "did you mean %s? I'll remove the missing entries"))

    vars_in_current_segment <-
      util_ensure_in(vars_in_current_segment,
                     colnames(ds1),
                     err_msg =
                       c(sprintf("Study variables from current segment %s",
                                 dQuote(current_segment)),
                         ": Missing %s from the study data,",
                         "did you mean %s? I'll remove the missing entries"))


    # Select ids from variables in each segment from data
    data_ids_1 <- ds1[, c(id_vars, vars_in_current_segment)]
    data_ids <- util_remove_empty_rows(data_ids_1, id_vars = id_vars)

    metadata_ids <- valid_id_table[, current_segment] # TODO: check if valid_id_table has a column named current_segment, else take id column, if there is no id then take the first column. If the table is empty, set metadata IDs to character(0)
    metadata_ids <- metadata_ids[!util_empty(metadata_ids)]

    # TODO: fix
    if (length(id_vars) > 1) {
      util_warning("Check for multiple IDs is not currently supported",
                   applicability_problem = TRUE,
                   intrinsic_applicability_problem = TRUE)
      return(
        res_pipeline <- data.frame(
          "Level" = "Segment",
          "NUM_int_sts_setrc" = 0,
          "PCT_int_sts_setrc" = 0,
          "GRADING" = 0,
          stringsAsFactors = FALSE
        )[FALSE, , drop = FALSE]
      )

      # TODO: supported by reference ids written as c("9871 | 4567", "id | exdate")
    }

    # Check membership of the id vectors
    unex_records_tmp <- setdiff(data_ids[id_vars], metadata_ids)

    # TODO: check definition of exact and subset
    if (length(unex_records_tmp) == 0) {
      match_actual <- "exact"
    } else if (length(unex_records_tmp) > 0) {
      match_actual <- "subset"
    }
    match_expected <- meta_data_record_check_list[[current_segment]]

    res_tmp <- data.frame(
      check.names = FALSE,
      "Check" = "Record set",
      "Segment" = current_segment,
      "Unexpected records in set" = !(length(unex_records_tmp) == 0),
      "Number of records in data" = length(data_ids$id),
      "Number of records in metadata" = length(metadata_ids),
      "Number of mismatches" =
        length(unex_records_tmp),
      "Percentage of mismatches" =
        abs(round(100 * length(unex_records_tmp) / length(metadata_ids), 3)),
      "Expected match type" = match_expected,
      "Actual match type" = match_actual,
      "GRADING" = ifelse((match_expected[1] == match_actual), 0, 1),
      stringsAsFactors = FALSE
    )

    # TODO: not working when multiple id vars are used
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
    "Segment" = res_df$Segment,
    "NUM_int_sts_setrc" = res_df$`Number of mismatches`,
    "PCT_int_sts_setrc" = res_df$`Percentage of mismatches`,
    "GRADING" = res_df$GRADING,
    stringsAsFactors = FALSE
  )

  return(list(
    SegmentData = res_df,
    SegmentTable = res_pipeline,
    UnexpectedRecords = unex_records_df
  ))

}
