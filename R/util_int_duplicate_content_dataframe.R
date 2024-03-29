#' Check for duplicated content
#'
#' @description
#' This function tests for duplicates entries in the data set. It is possible to
#' check duplicated entries by study segments or to consider only selected
#' segments.
#'
#' @param level [character] a character vector indicating whether the assessment should be conducted at the study level (level = "dataframe") or at the segment level (level = "segment").
#' @param identifier_name_list [vector] the vector that contains the name of the identifier to be used in the assessment. For the study level, corresponds to the names of the different data frames. For the segment level, indicates the name of the segments.
#'
#' @return a [list] with
#'   - `SegmentData`: data frame with the results of the quality check for duplicated entries
#'   - `SegmentTable`: data frame with selected duplicated entries check results, used for the data quality report.
#'   - `Duplicates`: vector with row indices of duplicated entries, if any, otherwise NULL.
#'
#' @family integrity_indicator_functions
#' @concept integrity_indicator
#' @keywords internal
util_int_duplicate_content_dataframe <- function(level = c("dataframe"),
                                                 identifier_name_list) {

  # Dataframe level check ----

  # Check arguments ----

  level <- util_match_arg(level)

  util_expect_scalar(identifier_name_list,
                     allow_null = TRUE,
                     allow_more_than_one = TRUE,
                     check_type = is.character)

  # Check for duplicated content  ----

  result <- lapply(setNames(nm = identifier_name_list), function(current_df) {
    # browser()

    # Convert data from list to data frame
    data_current_df <- util_expect_data_frame(current_df, dont_assign = TRUE)

    n_uniq <- nrow(unique(data_current_df))

    res_tmp <- data.frame(
      check.names = FALSE,
      "Check" = "Duplicates",
      "Data frame" = current_df,
      "Any duplicates" = ifelse(n_uniq < nrow(data_current_df), TRUE, FALSE),
      "Number of duplicates" = nrow(data_current_df) - n_uniq,
      "Percentage of duplicates" =
        round(100 * (nrow(data_current_df) - n_uniq) / nrow(data_current_df), 3),
      "GRADING" = ifelse(n_uniq < nrow(data_current_df), 1, 0),
      stringsAsFactors = FALSE
    )

    if (res_tmp[[3]]) { # only if there are any duplicated observations
      vec_dup <- which(duplicated(data_current_df)) # FIXME: Fix, this will omit the first elements, see id check for more hints, also on allowed repeats.
      vec_dup <- NULL # TODO: remove after fix
    } else {
      vec_dup <- NULL
    }

    return(list(res_tmp, vec_dup))
  })

  res_df <- do.call(rbind.data.frame, lapply(result, `[[`, 1))
  duplicates_df <- do.call(cbind.data.frame, lapply(result, `[[`, 2))

  res_pipeline <- data.frame(
    "Level" = "Dataframe",
    "DF_NAME" = res_df[["Data frame"]],
    "NUM_int_sts_dupl_content" = res_df[["Number of duplicates"]],
    "PCT_int_sts_dupl_content" = res_df[["Percentage of duplicates"]],
    "GRADING" = res_df[["GRADING"]],
    stringsAsFactors = FALSE
  )

  return(list(
    DataframeData = res_df,
    DataframeTable = res_pipeline,
    Duplicates = duplicates_df
  ))

}
