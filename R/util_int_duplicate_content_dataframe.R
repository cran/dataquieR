#' Check for duplicated content
#'
#' @description
#' This function tests for duplicates entries in the data set. It is possible to
#' check duplicated entries by study segments or to consider only selected
#' segments.
#'
#' @inheritParams .template_function_indicator
#' @param level [character] a character vector indicating whether the assessment should be conducted at the study level (level = "dataframe") or at the segment level (level = "segment").
#' @param identifier_name_list [vector] the vector that contains the name of the identifier to be used in the assessment. For the study level, corresponds to the names of the different data frames. For the segment level, indicates the name of the segments.
#' @param id_vars_list [list] the list containing the identifier variables names to be used in the assessment.
#' @param unique_rows [vector] named. for each data frame, either true/false or `no_id` to exclude ID variables from check
#' @param ... Not used.
#'
#' @return a [list] with
#'   - `SegmentData`: data frame with the results of the quality check for duplicated entries
#'   - `SegmentTable`: data frame with selected duplicated entries check results, used for the data quality report.
#'   - `Other`: vector with row indices of duplicated entries, if any, otherwise NULL.
#'
#' @family integrity_indicator_functions
#' @concept integrity_indicator
#' @keywords internal
util_int_duplicate_content_dataframe <- function(level = c("dataframe"),
                                                 identifier_name_list,
                                                 id_vars_list,
                                                 unique_rows,
                                                 meta_data_dataframe = "dataframe_level",
                                                 ...,
                                                 dataframe_level) {
  # Dataframe level check ----

  #meta_data_dup_rows_1[[DF_NAME]]

  # Check arguments ----

  util_ck_arg_aliases()

  level <- util_match_arg(level)


  if (missing(identifier_name_list) &&
      missing(id_vars_list) &&
      missing(meta_data_dataframe) &&
      formals()$meta_data_dataframe %in% prep_list_dataframes()) {
    meta_data_dataframe <- force(meta_data_dataframe)
  }

  if (missing(identifier_name_list) &&
      missing(id_vars_list) &&
      !missing(meta_data_dataframe)) {
    meta_data_dataframe <- prep_check_meta_data_dataframe(meta_data_dataframe)
    meta_data_dataframe <- meta_data_dataframe[
      vapply(meta_data_dataframe[[DF_NAME]],
             function(x) { !util_is_try_error(try(prep_get_data_frame(data_frame_name = x,
                                                                      keep_types = TRUE), silent = TRUE)) },
             FUN.VALUE = logical(1))
      , , drop = FALSE]
    meta_data_dataframe <-
      meta_data_dataframe[
        !util_empty(meta_data_dataframe[[DF_UNIQUE_ROWS]]) & (
          trimws(tolower(meta_data_dataframe[[DF_UNIQUE_ROWS]])) ==
            "no_id" |
          !util_is_na_0_empty_or_false(meta_data_dataframe[[DF_UNIQUE_ROWS]])
        ), ]
    identifier_name_list <- meta_data_dataframe[[DF_NAME]];
    id_vars_list <- lapply(setNames(meta_data_dataframe[[DF_ID_VARS]],
                                    nm = meta_data_dataframe[[DF_NAME]]),
                           util_parse_assignments,
                           multi_variate_text = TRUE )

    id_vars_list <- lapply(id_vars_list, unlist, recursive = TRUE)
    unique_rows <- setNames(meta_data_dataframe[[DF_UNIQUE_ROWS]],
                            nm = meta_data_dataframe[[DF_NAME]])
  } else if (!missing(meta_data_dataframe)) {
    util_error(c("I have %s and one of the following: %s.",
                 "This is not supported, please provide",
                 "either %s or all of %s."),
               sQuote("meta_data_dataframe"),
               util_pretty_vector_string(
                 c("identifier_name_list",
                   "id_vars_list"
                 )),
               sQuote("meta_data_dataframe"),
               util_pretty_vector_string(
                 c("identifier_name_list",
                   "id_vars_list"
                 )))
  } else if (missing(meta_data_dataframe) && (
    missing(identifier_name_list) ||
    missing(id_vars_list)
  )) {
    util_error(c("I don't have %s and also miss at least",
                 "one of the following: %s.",
                 "This is not supported, please provide",
                 "either %s or all of %s."),
               sQuote("meta_data_dataframe"),
               util_pretty_vector_string(
                 c("identifier_name_list",
                   "id_vars_list"
                 )),
               sQuote("meta_data_dataframe"),
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

  # Check for duplicated content  ----

  result <- lapply(setNames(nm = identifier_name_list), function(current_df) {

    if (trimws(tolower(unique_rows[[current_df]])) == "no_id") {
      idvl <- id_vars_list[[current_df]]
    } else {
      idvl <- character(0)
    }

    # Convert data from list to data frame
    data_current_df <- util_expect_data_frame(current_df, dont_assign = TRUE)

    data_current_df <-
      data_current_df[, setdiff(colnames(data_current_df),
                                idvl), FALSE]


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
    Other = duplicates_df
  ))

}
