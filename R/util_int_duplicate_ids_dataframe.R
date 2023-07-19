#' Check for duplicated IDs
#'
#' @description
#' This function tests for duplicates entries in identifiers. It is possible to
#' check duplicated identifiers by study segments or to consider only selected
#' segments.
#'
#' @param id_vars_list [list] id variable names for each segment or data frame
#' @param meta_data [data.frame] the data frame that contains metadata attributes of the study data, mandatory.
#' @param level [character] a character vector indicating whether the assessment should be conducted at the study level (level = "dataframe") or at the segment level (level = "segment").
#' @param repetitions [vector] an integer vector indicating the number of allowed repetitions in the id_vars. Currently, no repetitions are supported. # TODO
#' @param identifier_name_list [vector] the segments or data frame names being assessed
#'
#' @return a [list] with
#'   - `DataframeData`: data frame with the results of the quality check for duplicated identifiers
#'   - `DataframeTable`: data frame with selected duplicated identifiers check results, used for the data quality report.
#'   - `Duplicates`: vector with row indices of duplicated identifiers, if any, otherwise NULL.
#'
#'
util_int_duplicate_ids_dataframe <- function(level = c("dataframe"),
                                        id_vars_list,
                                        identifier_name_list,
                                        repetitions,
                                        meta_data = NULL) {


  # Checks  ----
  if (!missing(repetitions)
      && length(repetitions) > 0) {
    # TODO: use something similar to `table` instead of duplicated to get the lengths of each duplicate group
    util_error("%s is not yet supported by %s",
               sQuote("repetitions"),
               sQuote(sys.call()[[1]]))
  }

  # ID variable(s) must be specified
  util_stop_if_not(is.list(id_vars_list))
  util_stop_if_not(all(vapply(id_vars_list, is.character,
                              FUN.VALUE = logical(1))))
  # util_error(
  #   c("No ID variables specified - checks for duplicates not possible."),
  #   applicability_problem = TRUE
  # )

  util_expect_scalar(identifier_name_list,
                     allow_null = TRUE,
                     allow_more_than_one = TRUE,
                     check_type = is.character)

  util_stop_if_not(length(identifier_name_list) == length(id_vars_list))
  util_ensure_in(names(id_vars_list), identifier_name_list)
  util_ensure_in(identifier_name_list, names(id_vars_list))

  level <- util_match_arg(level)


  # TODO: update function to allow for multiple id vars, e.g. "id | exdate".


  # 1. Dataframe level check ----

  result <- lapply(setNames(nm = identifier_name_list), function(current_df) {

    # Convert data from list to data frame
    data_current_df <- util_expect_data_frame(current_df, dont_assign = TRUE)

    id_vars <- id_vars_list[[current_df]]

    id_cols <- intersect(id_vars, colnames(data_current_df))

    if (length(id_cols) == 0) {
      util_message(
        c("None of the ID variables (%s) in the metadata are included in",
          "the study data frame %s. Ignore that data frame."),
        util_pretty_vector_string(id_vars),
        dQuote(current_df),
        applicability_problem = TRUE
      )
      return(data.frame( # dummy
        check.names = FALSE,
        "Check" = "IDs",
        "Data frame" = current_df,
        "Any duplicates" = NA,
        "Number of duplicates" = NA_real_,
        "Percentage of duplicates" =
          NA_real_,
        "GRADING" = NA_real_,
        stringsAsFactors = FALSE
      ))
    }

    n_uniq <- sum(!duplicated(data_current_df[, id_cols]))

    res_tmp <- data.frame(
      check.names = FALSE,
      "Check" = "IDs",
      "Data frame" = current_df,
      "Any duplicates" = ifelse(n_uniq < nrow(data_current_df), TRUE, FALSE),
      "Number of duplicates" = nrow(data_current_df) - n_uniq,
      "Percentage of duplicates" =
        round(100 * (nrow(data_current_df) - n_uniq) / nrow(data_current_df), 3),
      "GRADING" = ifelse(n_uniq < nrow(data_current_df), 1, 0),
      stringsAsFactors = FALSE
    )

    if (res_tmp[[3]]) { # only if there are any duplicated observations
      vec_dup <- which(duplicated(data_current_df[id_vars])) # FIXME: This omits the first occurrences of each duplicate group.
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
    "DF_NAME" = res_df[, 2],
    "NUM_int_sts_dupl_ids" = res_df[, 4],
    "PCT_int_sts_dupl_ids" = res_df[, 5],
    "GRADING" = res_df$GRADING,
    stringsAsFactors = FALSE
  )

  return(list(
    DataframeData = res_df,
    DataframeTable = res_pipeline,
    Duplicates = duplicates_df # TODO: what is this? FlaggedStudyDataList?
  ))
}
