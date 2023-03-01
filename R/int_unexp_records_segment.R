#' Check for unexpected data record count within segments
#' @description
#' This function contrasts the expected record number in each study segment in
#' the metadata with the actual record number in each segment data frame.
#'
#' @param data_record_count [integer]  an integer vector with the number of expected data records, mandatory.
#' @param study_segment [character] a character vector indicating the name of each study data frame, mandatory.
#' @param study_data [data.frame] the data frame that contains the measurements, mandatory.
#' @param meta_data [data.frame] the data frame that contains metadata attributes of the study data, mandatory.
#'
#' @return a [list] with
#'   - `SegmentData`: data frame with the results of the quality check for unexpected data elements
#'   - `SegmentTable`: data frame with selected unexpected data elements check results, used for the data quality report.
#'
#' @details
#' The current implementation does not take into account jump or missing codes, the function is rather based on checking whether NAs are present in the study data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' study_data <- readRDS(system.file("extdata", "ship.RDS", package = "dataquieR"))
#' meta_data <- readRDS(system.file("extdata", "ship_meta.RDS", package = "dataquieR"))
#'
#' int_unexp_records_segment(
#'   study_segment = c("PART_STUDY", "PART_INTERVIEW"),
#'   data_record_count = c(3000, 1100),
#'   study_data = study_data,
#'   meta_data = meta_data
#' )
#' }
#'
int_unexp_records_segment <- function(study_segment,
                                      data_record_count, # TODO: DONT PASS 2 VECTORS FOR ASSINGMENTS
                                      study_data,
                                      meta_data) {

  # Preps and checks ----
  prep_prepare_dataframes(.allow_empty = TRUE)

  # meta_data$STUDY_SEGMENT <-
  #   util_map_labels(meta_data$STUDY_SEGMENT,
  #                   meta_data = meta_data,
  #                   to = label_col,
  #                   ifnotfound = meta_data$STUDY_SEGMENT)

  # Check arguments ----

  util_expect_scalar(study_segment,
                     allow_more_than_one = TRUE,
                     allow_null = TRUE,
                     check_type = is.character)

  util_expect_scalar(data_record_count,
                     allow_more_than_one = TRUE,
                     allow_null = TRUE,
                     check_type = util_all_is_integer)

  util_stop_if_not(length(data_record_count) ==
                     length(study_segment))

  # check that specified segments are included in the metadata
  old_segments <- study_segment
  segments <- intersect(study_segment, meta_data$STUDY_SEGMENT)

  if (length(old_segments) > length(segments)) {
    util_warning(
      c("The segments in the %s do not match the segments in %s,",
        "considering only the intersection"),
      dQuote("meta_data"),
      dQuote("meta_data_segment"),
      applicability_problem = TRUE
    )
  }

  # Check for unexpected records  ----
  names(data_record_count) <- segments

  result <- lapply(setNames(nm = segments), function(current_segment) {

    vars_in_current_segment <-
      util_get_vars_in_segment(current_segment, meta_data = meta_data,
                               label_col = label_col)

    vars_in_current_segment <- intersect(colnames(ds1),
                                         vars_in_current_segment)

    data_records_0 <- util_remove_empty_rows(ds1[, c(vars_in_current_segment)])
    data_records_1 <- subset(data_records_0,
      rowSums(is.na(data_records_0)) != length(vars_in_current_segment),
      drop = FALSE
    )

    # TODO: use `util_observation_expected`
    # The user could have more control to specify which subset of jump codes should be used
    # data_records_1 <- subset(rowSums(data_records_0 %in% participation_jump_codes) == length(vars_in_current_segment))

    # Select segment variables from data
    data_records_cnt <- nrow(data_records_1)
    metadata_records_cnt <- data_record_count[[current_segment]]

    res_tmp <- data.frame(
      check.names = FALSE,
      "Segment" = current_segment,
      "Check" = "Records",
      "Unexpected records" = !(data_records_cnt == metadata_records_cnt),
      "Number of records in data" = data_records_cnt,
      "Number of records in metadata" = metadata_records_cnt,
      "Number of mismatches" =
        abs(round(data_records_cnt - metadata_records_cnt, 3)),
      "Percentage of mismatches" =
        abs(round(100 * (data_records_cnt - metadata_records_cnt) /
                    metadata_records_cnt, 3)),
      "GRADING" = ifelse(data_records_cnt == metadata_records_cnt, 0, 1),
      stringsAsFactors = FALSE
    )
    rownames(res_tmp) <- NULL
    return(res_tmp)
  })

  res_df <- do.call(rbind.data.frame, result)

  rownames(res_df) <- NULL

  res_pipeline <- data.frame(
    "Segment" = res_df$Segment,
    "NUM_int_sts_countre" = res_df$`Number of mismatches`,
    "PCT_int_sts_countre" = res_df$`Percentage of mismatches`,
    "GRADING" = res_df$GRADING,
    stringsAsFactors = FALSE
  )

  return(list(
    SegmentData = res_df,
    SegmentTable = res_pipeline
  ))
}
