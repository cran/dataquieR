#' Check for duplicated IDs
#'
#' @description
#' This function tests for duplicates entries in identifiers. It is possible to
#' check duplicated identifiers by study segments or to consider only selected
#' segments.
#'
#' @inheritParams .template_function_indicator
#' @param id_vars_list [list] id variable names for each segment or data frame
#' @param level [character] a character vector indicating whether the assessment should be conducted at the study level (level = "dataframe") or at the segment level (level = "segment").
#' @param repetitions [vector] an integer vector indicating the number of allowed repetitions in the `id_vars`.
#' @param identifier_name_list [vector] the segments or data frame names being assessed
#' @param ... not used.
#'
#' @return a [list] with
#'   - `DataframeData`: data frame with the results of the quality check for duplicated identifiers
#'   - `DataframeTable`: data frame with selected duplicated identifiers check results, used for the data quality report.
#'   - `Other`: named list with inner lists of unique cases containing each the
#'              row indices of duplicated identifiers separated by "|" , if any.
#'              outer names are names of the data frames
#'
#' @family integrity_indicator_functions
#' @concept integrity_indicator
#' @keywords internal
util_int_duplicate_ids_dataframe <- function(level = c("dataframe"),
                                        id_vars_list,
                                        identifier_name_list,
                                        repetitions,
                                        meta_data_dataframe = "dataframe_level",
                                        ...,
                                        dataframe_level) {

  # Checks  ----
  util_ck_arg_aliases()

  level <- util_match_arg(level)


  if (missing(id_vars_list) &&
      missing(identifier_name_list) &&
      missing(repetitions) &&
      missing(meta_data_dataframe) &&
      formals()$meta_data_dataframe %in% prep_list_dataframes()) {
    meta_data_dataframe <- force(meta_data_dataframe)
  }

  if (missing(id_vars_list) &&
      missing(identifier_name_list) &&
      missing(repetitions) &&
      !missing(meta_data_dataframe)) {
    meta_data_dataframe <- prep_check_meta_data_dataframe(meta_data_dataframe)
    meta_data_dataframe <- meta_data_dataframe[
      vapply(meta_data_dataframe[[DF_NAME]],
             function(x) { !util_is_try_error(try(prep_get_data_frame(data_frame_name = x,
                                                                      keep_types = TRUE), silent = TRUE)) },
             FUN.VALUE = logical(1))
      , , drop = FALSE]
    meta_data_dataframe <- meta_data_dataframe[
      !util_empty(meta_data_dataframe[[DF_UNIQUE_ID]]) &
        as.numeric(meta_data_dataframe[[DF_UNIQUE_ID]]) > 0 &
        !util_empty(meta_data_dataframe[[DF_ID_VARS]]), ,
      drop = FALSE
    ]

    identifier_name_list <- meta_data_dataframe[[DF_NAME]];

    id_vars_list <- lapply(setNames(meta_data_dataframe[[DF_ID_VARS]],
                                    nm = meta_data_dataframe[[DF_NAME]]),
                           util_parse_assignments,
                           multi_variate_text = TRUE)

    id_vars_list <- lapply(lapply(id_vars_list, `[[`, 1), unlist,
                           recursive = FALSE)

    repetitions <- meta_data_dataframe[[DF_UNIQUE_ID]]

  } else if (!missing(meta_data_dataframe)) {
    util_error(c("I have %s and one of the following: %s.",
                 "This is not supported, please provide",
                 "either %s or all of %s."),
               sQuote("meta_data_dataframe"),
               util_pretty_vector_string(
                 c("id_vars_list",
                   "identifier_name_list",
                   "repetitions"
                 )),
               sQuote("meta_data_dataframe"),
               util_pretty_vector_string(
                 c("id_vars_list",
                   "identifier_name_list",
                   "repetitions"
                 )))
  } else if (missing(meta_data_dataframe) && (
    missing(identifier_name_list) ||
    missing(id_vars_list) ||
    missing(repetitions)
  )) {
    util_error(c("I don't have %s and also miss at least",
                 "one of the following: %s.",
                 "This is not supported, please provide",
                 "either %s or all of %s."),
               sQuote("meta_data_dataframe"),
               util_pretty_vector_string(
                 c("id_vars_list",
                   "identifier_name_list",
                   "repetitions"
                 )),
               sQuote("meta_data_dataframe"),
               util_pretty_vector_string(
                 c("id_vars_list",
                   "identifier_name_list",
                   "repetitions"
                 )))
  }


  util_expect_scalar(identifier_name_list,
                     allow_null = TRUE,
                     allow_more_than_one = TRUE,
                     check_type = is.character)


  util_expect_scalar(repetitions,
                     allow_more_than_one = TRUE,
                     allow_na = FALSE,
                     check_type = util_all_is_integer)

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
  util_stop_if_not(length(identifier_name_list) == length(repetitions))
  util_ensure_in(names(id_vars_list), identifier_name_list)
  util_ensure_in(identifier_name_list, names(id_vars_list))

  names(repetitions) <- identifier_name_list

  level <- util_match_arg(level)


  # 1. Dataframe level check ----

  result <- lapply(setNames(nm = identifier_name_list), function(current_df) {

    # Convert data from list to data frame
    data_current_df <- util_expect_data_frame(current_df, dont_assign = TRUE)

    reps <- repetitions[[current_df]]

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

    dup_info <- util_find_duplicated_rows(
      study_data = data_current_df,
      id_vars = id_vars,
      repeptitions = reps)

    n_uniq <- nrow(data_current_df) - sum(dup_info$unexp_reps)

    res_tmp <- data.frame(
      check.names = FALSE,
      "Check" = "IDs",
      "Data frame" = current_df,
      "Any duplicates" = !!nrow(dup_info),
      "Number of duplicates" = nrow(data_current_df) - n_uniq,
      "Percentage of duplicates" =
        round(100 * (nrow(data_current_df) - n_uniq) /
                nrow(data_current_df), 3),
      "GRADING" = ifelse(n_uniq < nrow(data_current_df), 1, 0),
      "ID Vars" = prep_deparse_assignments(id_cols, mode = "string_codes"),
      stringsAsFactors = FALSE
    )

    if (res_tmp[[3]]) { # only if there are any duplicated observations
      vec_dup <- dup_info$which
    } else {
      vec_dup <- NULL
    }

    return(list(res_tmp, vec_dup))
  })

  res_df <- do.call(rbind.data.frame, lapply(result, `[[`, 1))
  duplicates_df <- lapply(result, `[[`, 2)

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
    Other = duplicates_df
  ))
}

util_find_duplicated_rows <- function(study_data,
                                      id_vars,
                                      repeptitions = 1) {
  util_expect_data_frame(study_data, col_names = id_vars)
  study_data <- study_data[, id_vars, FALSE]
  empty_sd <- study_data
  empty_sd[] <- lapply(study_data, util_empty)
  study_data[as.matrix(empty_sd)] <- NA
  r <- as.data.frame(table(study_data, useNA = "no"))
  r <- r[r$Freq > repeptitions, , FALSE]
  nsd <- cbind(study_data,
               ..rownr = seq_len(nrow(study_data)))
  r$which <- lapply(seq_len(nrow(r)),
                    function(my_dup_rw) {
                      my_dup <- r[my_dup_rw, , FALSE]
                      dd <- merge(nsd, my_dup, by = id_vars)
                      prep_deparse_assignments(dd$..rownr,
                                               mode = "string_codes")
                    })
  r$unexp_reps <- r$Freq - repeptitions
  r
}
