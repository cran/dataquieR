#' Check for duplicated IDs
#'
#' @description
#' This function tests for duplicates entries in identifiers. It is possible to
#' check duplicated identifiers by study segments or to consider only selected
#' segments.
#'
#' @param id_vars_list [list] id variable names for each segment or data frame
#' @param study_data [data.frame] the data frame that contains the measurements, mandatory.
#' @param meta_data [data.frame] the data frame that contains metadata attributes of the study data, mandatory.
#' @param level [character] a character vector indicating whether the assessment should be conducted at the study level (level = "dataframe") or at the segment level (level = "segment").
#' @param repetitions [vector] an integer vector indicating the number of allowed repetitions in the id_vars. Currently, no repetitions are supported.
#' @param study_segment [vector] the segments or data frame names being assessed
#'
#' @inheritParams .template_function_indicator
#'
#' @return a [list] with
#'   - `SegmentData`: data frame with the results of the quality check for duplicated identifiers
#'   - `SegmentTable`: data frame with selected duplicated identifiers check results, used for the data quality report.
#'   - `Other`: named list with inner lists of unique cases containing each the
#'              row indices of duplicated identifiers separated by "|" , if any.
#'              outer names are names of the segments.
#'              Use [prep_get_study_data_segment()] to get the data frame the
#'              indices refer to.
#'
#' @family integrity_indicator_functions
#' @concept integrity_indicator
#' @keywords internal
util_int_duplicate_ids_segment <- function(level = c("segment"),
                                           id_vars_list,
                                           study_segment,
                                           repetitions,
                                           study_data,
                                           meta_data,
                                           meta_data_segment = "segment_level",
                                           segment_level
                                           ) {

  util_ck_arg_aliases()

  level <- util_match_arg(level)

  # Segment level ----

  if (missing(id_vars_list) &&
      missing(study_segment) &&
      missing(repetitions) &&
      missing(meta_data_segment) &&
      formals()$meta_data_segment %in% prep_list_dataframes()) {
    meta_data_segment <- force(meta_data_segment)
  }

  if (missing(id_vars_list) &&
      missing(study_segment) &&
      missing(repetitions) &&
      !missing(meta_data_segment)) {
    meta_data_segment <- prep_check_meta_data_segment(meta_data_segment)

    meta_data_segment <-
      meta_data_segment[!util_empty(meta_data_segment[[SEGMENT_ID_VARS]]), ,
                        drop = FALSE
      ]
    meta_data_segment <-
      meta_data_segment[!util_empty(meta_data_segment[[SEGMENT_UNIQUE_ID]]), ,
                        drop = FALSE
      ]
    # TODO: if nothing left
    id_vars_list <- lapply(setNames(meta_data_segment[[SEGMENT_ID_VARS]],
                                    nm = meta_data_segment[[STUDY_SEGMENT]]),
                           util_parse_assignments,
                           multi_variate_text = TRUE );
    id_vars_list <- lapply(id_vars_list, unlist, recursive = TRUE)
    id_vars_list <- lapply(setNames(meta_data_segment[[SEGMENT_ID_VARS]],
                                    nm = meta_data_segment[[STUDY_SEGMENT]]),
                           util_parse_assignments,
                           multi_variate_text = TRUE
    )
    id_vars_list <- lapply(id_vars_list, unlist, recursive = TRUE)

    id_vars_list <- lapply(id_vars_list,
                           util_map_labels,
                           meta_data = meta_data,
                           to = label_col)

    study_segment <- meta_data_segment[[STUDY_SEGMENT]];
    repetitions <- meta_data_segment[[SEGMENT_UNIQUE_ID]]
  } else if (!missing(meta_data_segment)) {
    util_error(c("I have %s and one of the following: %s.",
                 "This is not supported, please provide",
                 "either %s or all of %s."),
               sQuote("meta_data_segment"),
               util_pretty_vector_string(
                 c("id_vars_list",
                   "study_segment",
                   "repetitions",
                   "meta_data_segment"
                 )),
               sQuote("meta_data_segment"),
               util_pretty_vector_string(
                 c("id_vars_list",
                   "study_segment",
                   "repetitions",
                   "meta_data_segment"
                 )))
  } else if (missing(meta_data_segment) && (
    missing(id_vars_list) ||
    missing(study_segment) ||
    missing(repetitions)
  )) {
    util_error(c("I don't have %s and also miss at least",
                 "one of the following: %s.",
                 "This is not supported, please provide",
                 "either %s or all of %s."),
               sQuote("meta_data_segment"),
               util_pretty_vector_string(
                 c("id_vars_list",
                   "study_segment",
                   "repetitions"
                 )),
               sQuote("meta_data_segment"),
               util_pretty_vector_string(
                 c("id_vars_list",
                   "study_segment",
                   "repetitions"
                 )))
  }


  # Checks ----

  # map metadata to study data
  prep_prepare_dataframes(.allow_empty = TRUE)

  # ID variable(s) must be specified
  util_stop_if_not(is.list(id_vars_list))
  util_stop_if_not(all(vapply(id_vars_list, is.character,
                              FUN.VALUE = logical(1))))

  util_expect_scalar(study_segment,
                     allow_null = TRUE,
                     allow_more_than_one = TRUE,
                     check_type = is.character)

  util_stop_if_not(length(study_segment) == length(id_vars_list))
  util_stop_if_not(length(study_segment) == length(repetitions))
  names(repetitions) <- study_segment

  util_ensure_in(names(id_vars_list), study_segment)
  util_ensure_in(study_segment, names(id_vars_list))

  level <- util_match_arg(level)

  # Check segments ----
  # check that specified segments are included in the metadata

  old_study_segment <- study_segment
  study_segment <- intersect(study_segment, meta_data[[STUDY_SEGMENT]])

  if (length(old_study_segment) > length(study_segment)) {
    util_message(
      "The study_segment in the %s do not match the study_segment in %s, considering only the intersection",
      dQuote("meta_data"),
      dQuote("meta_data_segment"),
      applicability_problem = TRUE,
      intrinsic_applicability_problem = FALSE
    )
  }

  label_col <- attr(ds1, "label_col")

  # Check for duplicated record IDs  ----
  result <- lapply(setNames(nm = study_segment), function(current_segment) {

    reps <- repetitions[[current_segment]]

    id_vars <- id_vars_list[[current_segment]]
    id_vars <- util_find_var_by_meta(resp_vars = id_vars,
                                     meta_data = meta_data,
                                     label_col = label_col,
                                     target = label_col,
                                     ifnotfound = id_vars)
    id_vars <-
      util_ensure_in(id_vars,
                     colnames(ds1),
                     err_msg =
                       c(sprintf("ID variables in current segment %s",
                                 dQuote(current_segment)),
                         ": Missing %s from the study data,",
                         "did you mean %s? I'll remove the missing entries"),
                     applicability_problem = TRUE)


    if (length(id_vars) == 0) {
      util_warning(
        "The %s (%s) in the %s are not included in the %s",
        "ID variables",
        util_pretty_vector_string(id_vars_list[[current_segment]]),
        "metadata",
        "study data",
        applicability_problem = TRUE
      )
      return(data.frame(
        check.names = FALSE,
        "Check" = "IDs",
        "Segment" = current_segment,
        "Any duplicates" = NA,
        "Number of duplicates" = NA_real_,
        "Percentage of duplicates" = NA_real_,
        "GRADING" = NA_real_,
        stringsAsFactors = FALSE
      ))
    }

    vars_in_current_segment <-
      util_get_vars_in_segment(
        segment = current_segment,
        meta_data = meta_data,
        label_col = label_col
      )


    vars_in_current_segment <-
      util_ensure_in(vars_in_current_segment,
                     colnames(ds1),
                     err_msg =
                       c(sprintf("Study variables from current segment %s",
                                 dQuote(current_segment)),
                         ": Missing %s from the study data,",
                         "did you mean %s? I'll remove the missing entries"),
                     applicability_problem = TRUE)


    ds1_vars_in_current_segment <- ds1[, c(id_vars, vars_in_current_segment)]

    ds_reduced <- util_remove_empty_rows(
      ds1_vars_in_current_segment,
      id_vars = id_vars)


    dup_info <- util_find_duplicated_rows(
      study_data = ds_reduced,
      id_vars = id_vars,
      repeptitions = reps)

    n_uniq <- nrow(ds_reduced) - sum(dup_info$unexp_reps)

    n_total <-
      nrow(ds_reduced)

    res_tmp <- data.frame(
      check.names = FALSE,
      "Check" = "IDs",
      "Segment" = current_segment,
      "Any duplicates" = !!nrow(dup_info),
      "Number of duplicates" = n_total - n_uniq,
      "Percentage of duplicates" =
        round(100 * (n_total - n_uniq) /
                n_total, 3),
      "GRADING" = as.integer(n_uniq < n_total),
      "ID Vars" = prep_deparse_assignments(id_vars, mode = "string_codes"),
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
    "Segment" = res_df$Segment,
    "NUM_int_sts_dupl_ids" = res_df$`Number of duplicates`,
    "PCT_int_sts_dupl_ids" = res_df$`Percentage of duplicates`,
    "GRADING" = res_df$GRADING,
    stringsAsFactors = FALSE
  )

  return(list(
    SegmentData = res_df,
    SegmentTable = res_pipeline,
    Other = duplicates_df
  ))

}
