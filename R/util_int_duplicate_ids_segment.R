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
#' @param repetitions [vector] an integer vector indicating the number of allowed repetitions in the id_vars. Currently, no repetitions are supported. # TODO
#' @param study_segment [vector] the segments or data frame names being assessed
#'
#' @return a [list] with
#'   - `SegmentData`: data frame with the results of the quality check for duplicated identifiers
#'   - `SegmentTable`: data frame with selected duplicated identifiers check results, used for the data quality report.
#'   - `Duplicates`: vector with row indices of duplicated identifiers, if any, otherwise NULL.
#'
#'
util_int_duplicate_ids_segment <- function(level = c("segment"),
                                           id_vars_list,
                                           study_segment,
                                           repetitions,
                                           study_data,
                                           meta_data) {


  # Segment level ----

  # Checks ----

  # map metadata to study data
  prep_prepare_dataframes(.allow_empty = TRUE)

  if (!missing(repetitions)
      && length(repetitions) > 0) {
    util_error("%s is not yet supported by %s",
               sQuote("repetitions"),
               sQuote(sys.call()[[1]]))
  }

  # ID variable(s) must be specified
  util_stop_if_not(is.list(id_vars_list))
  util_stop_if_not(all(vapply(id_vars_list, is.character,
                              FUN.VALUE = logical(1))))

  util_expect_scalar(study_segment,
                     allow_null = TRUE,
                     allow_more_than_one = TRUE,
                     check_type = is.character)

  util_stop_if_not(length(study_segment) == length(id_vars_list)) # TODO: Don't pass columns separately
  util_ensure_in(names(id_vars_list), study_segment)
  util_ensure_in(study_segment, names(id_vars_list))

  level <- util_match_arg(level)

  # TODO: update function to allow for multiple id vars, e.g. "id | exdate" -- this is then already a vector here, see wrapper int_all_datastructure_segment.

  # Check segments ----
  # check that specified segments are included in the metadata

  old_study_segment <- study_segment
  study_segment <- intersect(study_segment, meta_data[[STUDY_SEGMENT]])

  if (length(old_study_segment) > length(study_segment)) {
    util_warning(
      "The study_segment in the %s do not match the study_segment in %s, considering only the intersection",
      dQuote("meta_data"),
      dQuote("meta_data_segment"),
      applicability_problem = TRUE
    )
  }

  # Check for duplicated record IDs  ----
  result <- lapply(setNames(nm = study_segment), function(current_segment) {

    id_vars <- id_vars_list[[current_segment]]
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
        "meta data",
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

    n_uniq <-
      nrow(
        ds_reduced[!duplicated(ds_reduced[id_vars]), ])

    n_total <-
      nrow(ds_reduced)

    res_tmp <- data.frame(
      check.names = FALSE,
      "Check" = "IDs",
      "Segment" = current_segment,
      "Any duplicates" = (n_uniq < n_total),
      "Number of duplicates" = n_total - n_uniq,
      "Percentage of duplicates" =
        round(100 * (n_total - n_uniq) / n_total, 3),
      "GRADING" = as.integer(n_uniq < n_total),
      stringsAsFactors = FALSE
    )

    if (res_tmp[[3]]) { # only if there are any duplicated observations
      vec_dup <- which(duplicated(ds1_vars_in_current_segment[id_vars])) # FIXME: This will only report the tail of each duplicate group excluding the first occurrences of something duplicated
      vec_dup <- NULL # TODO: remove line after fix
    } else {
      vec_dup <- NULL
    }

    return(list(res_tmp, vec_dup))
  })

  res_df <- do.call(rbind.data.frame, lapply(result, `[[`, 1))
  duplicates_df <- do.call(cbind.data.frame, lapply(result, `[[`, 2))

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
    Duplicates = duplicates_df
  ))

}
