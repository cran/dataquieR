#' Check for unexpected data element count
#'
#' @description
#' This function contrasts the expected element number in each study in
#' the metadata with the actual element number in each study data frame.
#'
#' [Indicator]
#'
#' @param data_element_count [integer]  an integer vector with the number of expected data elements, mandatory.
#' @param identifier_name_list [character] a character vector indicating the name of each study data frame, mandatory.
#'
#' @inheritParams .template_function_indicator
#'
#' @return a [list] with
#'   - `DataframeData`: data frame with the results of the quality check for unexpected data elements
#'   - `DataframeTable`: data frame with selected unexpected data elements check results, used for the data quality report.
#'
#' @export
int_unexp_elements <- function(identifier_name_list,
                               data_element_count,
                               meta_data_dataframe = "dataframe_level",
                               meta_data_v2,
                               dataframe_level) {

  # Checks arguments ------------------------------------------------------------
  util_maybe_load_meta_data_v2()

  util_ck_arg_aliases()

  if (missing(identifier_name_list) &&
      missing(data_element_count) &&
      missing(meta_data_dataframe) &&
      formals()$meta_data_dataframe %in% prep_list_dataframes()) {
    meta_data_dataframe <- force(meta_data_dataframe)
  }

  if (missing(identifier_name_list) &&
      missing(data_element_count) &&
      !missing(meta_data_dataframe)) {
    meta_data_dataframe <- prep_check_meta_data_dataframe(meta_data_dataframe)
    meta_data_dataframe <- meta_data_dataframe[
      vapply(meta_data_dataframe[[DF_NAME]],
             function(x) { !util_is_try_error(try(prep_get_data_frame(data_frame_name = x,
                                                                      keep_types = TRUE), silent = TRUE)) },
             FUN.VALUE = logical(1))
      , , drop = FALSE]
    meta_data_dataframe <- meta_data_dataframe[
      !util_empty(meta_data_dataframe[[DF_ELEMENT_COUNT]])
      , , drop = FALSE]
    # TODO: if nothing left
    identifier_name_list <- meta_data_dataframe[[DF_NAME]];
    data_element_count <- meta_data_dataframe[[DF_ELEMENT_COUNT]]
  } else if (!missing(meta_data_dataframe)) {
    util_error(c("I have %s and one of the following: %s.",
                 "This is not supported, please provide",
                 "either %s or all of %s."),
               sQuote("meta_data_dataframe"),
               util_pretty_vector_string(
                 c("identifier_name_list",
                   "data_element_count"
                 )),
               sQuote("meta_data_dataframe"),
               util_pretty_vector_string(
                 c("identifier_name_list",
                   "data_element_count"
                 )))
  } else if (missing(meta_data_dataframe) && (
    missing(identifier_name_list) ||
    missing(data_element_count)
  )) {
    util_error(c("I don't have %s and also miss at least",
                 "one of the following: %s.",
                 "This is not supported, please provide",
                 "either %s or all of %s."),
               sQuote("meta_data_dataframe"),
               util_pretty_vector_string(
                 c("identifier_name_list",
                   "data_element_count"
                 )),
               sQuote("meta_data_dataframe"),
               util_pretty_vector_string(
                 c("identifier_name_list",
                   "data_element_count"
                 )))
  }

  util_expect_scalar(identifier_name_list,
                     allow_more_than_one = TRUE,
                     allow_null = TRUE,
                     check_type = is.character)

  util_expect_scalar(data_element_count,
                     allow_more_than_one = TRUE,
                     allow_null = TRUE,
                     check_type = is.numeric)

  util_stop_if_not(length(identifier_name_list) == length(data_element_count),
                   label =
                     sprintf("In %s, %s and %s should have the same length: %s",
                             dQuote("int_unexp_elements"),
                             dQuote("identifier_name_list"),
                             dQuote("data_element_count"),
                             "They represent a mapping."
                             ))

  # Check for unexpected elements  ---------------------------------------------

  names(data_element_count) <- identifier_name_list

  result <- lapply(setNames(nm = identifier_name_list), function(current_df) {

    # Convert data from list to data frame
    data_current_df <- util_expect_data_frame(current_df, dont_assign = TRUE)

    # Select variables from data and metadata
    data_elements <- ncol(data_current_df)
    metadata_elements <- data_element_count[[current_df]]

    res_tmp <- data.frame(
      check.names = FALSE,
      "Check" = "Elements",
      "Data frame" = current_df,
      "Unexpected elements" = !(data_elements == metadata_elements),
      "Number of elements in data" = data_elements,
      "Number of elements in metadata" = metadata_elements,
      "Number of mismatches" =
        abs(round(data_elements - metadata_elements, 3)),
      "Percentage of mismatches" =
        abs(round(100 * ( data_elements - metadata_elements ) / metadata_elements, 3)),
      "GRADING" = ifelse(data_elements == metadata_elements, 0, 1),
      stringsAsFactors = FALSE
    )

    return(res_tmp)
  })

  res_df <- do.call(rbind.data.frame, result)

  res_pipeline <- data.frame( # TODO: make res_df from these names, not vice versa.
    "Level" = "Dataframe",
    "DF_NAME" = res_df[["Data frame"]],
    "NUM_int_sts_countel" = res_df[["Number of mismatches"]],
    "PCT_int_sts_countel" = res_df[["Percentage of mismatches"]],
    "GRADING" = res_df[["GRADING"]],
    stringsAsFactors = FALSE
  )

  return(list(
    DataframeData = res_df,
    DataframeTable = res_pipeline
  ))
}
