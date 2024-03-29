#' Check for unexpected data record count at the data frame level
#'
#' @description
#' This function contrasts the expected record number in each study in
#' the metadata with the actual record number in each study data frame.
#'
#' [Indicator]
#'
#' @param data_record_count [integer]  an integer vector with the number of expected data records per study data frame, mandatory.
#' @param identifier_name_list [character] a character vector indicating the name of each study data frame, mandatory.
#'
#' @return a [list] with
#'   - `DataframeData`: data frame with the results of the quality check for unexpected data elements
#'   - `DataframeTable`: data frame with selected unexpected data elements check results, used for the data quality report.
#'
#' @export
int_unexp_records_dataframe <- function(identifier_name_list, # TODO: Don't pass an assignments as two separate vectors.
                              data_record_count) {

  # Checks arguments ----

  util_expect_scalar(identifier_name_list,
                     allow_more_than_one = TRUE,
                     allow_null = TRUE,
                     check_type = is.character)

  util_expect_scalar(data_record_count,
                     allow_more_than_one = TRUE,
                     allow_null = TRUE,
                     check_type = is.numeric)

  util_stop_if_not(length(identifier_name_list) == length(data_record_count),
                   label =
                     sprintf("In %s, %s and %s should have the same length: %s",
                             dQuote("int_unexp_elements"),
                             dQuote("identifier_name_list"),
                             dQuote("data_record_count"),
                             "They represent a mapping."
                     ))

  # Check for unexpected records  ----

  names(data_record_count) <- identifier_name_list

  result <- lapply(setNames(nm = identifier_name_list), function(current_df) {

    # Convert data from list to data frame
    data_current_df <- util_expect_data_frame(current_df, dont_assign = TRUE)

    # Select variables from data and metadata
    data_records <- nrow(data_current_df)
    metadata_records <- data_record_count[[current_df]]

    res_tmp <- data.frame(
      check.names = FALSE,
      "Check" = "Records",
      "Data frame" = current_df,
      "Unexpected records" = !(data_records == metadata_records),
      "Number of records in data" = data_records,
      "Number of records in metadata" = metadata_records,
      "Number of mismatches" =
        abs(round(data_records - metadata_records, 3)),
      "Percentage of mismatches" =
        abs(round(100 * ( data_records - metadata_records ) / metadata_records, 3)),
      "GRADING" = ifelse(data_records == metadata_records, 0, 1),
      stringsAsFactors = FALSE
    )

    return(res_tmp)
  })

  res_df <- do.call(rbind.data.frame, result)

  res_pipeline <- data.frame( # TODO: make res_df from these names, not vice versa.
    "Level" = "Dataframe",
    "DF_NAME" = res_df[["Data frame"]],
    "NUM_int_sts_countre" = res_df[["Number of mismatches"]],
    "PCT_int_sts_countre" = res_df[["Percentage of mismatches"]],
    "GRADING" = res_df[["GRADING"]],
    stringsAsFactors = FALSE
  )

  return(list(
    DataframeData = res_df,
    DataframeTable = res_pipeline
  ))
}
