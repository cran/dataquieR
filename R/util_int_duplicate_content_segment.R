#' Check for duplicated content
#'
#' @description
#' This function tests for duplicates entries in the data set. It is possible to
#' check duplicated entries by study segments or to consider only selected
#' segments.
#'
#' @param level [character] a character vector indicating whether the assessment should be conducted at the study level (level = "dataframe") or at the segment level (level = "segment").
#' @param study_segment [vector] the vector that contains the name of the identifier to be used in the assessment. For the study level, corresponds to the names of the different data frames. For the segment level, indicates the name of the segments.
#' @param study_data [data.frame] the data frame that contains the measurements, mandatory.
#' @param meta_data [data.frame] the data frame that contains metadata attributes of the study data, mandatory.
#'
#' @return a [list] with
#'   - `SegmentData`: data frame with the results of the quality check for duplicated entries
#'   - `SegmentTable`: data frame with selected duplicated entries check results, used for the data quality report.
#'   - `Duplicates`: vector with row indices of duplicated entries, if any, otherwise NULL.
#'
#' @examples
#' \dontrun{
#' study_data <- readRDS(system.file("extdata", "ship.RDS", package = "dataquieR"))
#' meta_data <- readRDS(system.file("extdata", "ship_meta.RDS", package = "dataquieR"))
#'
#' # Segment level
#' int_duplicate_content(
#'   level = "segment",
#'   study_segment = c("INTRO", "INTERVIEW"),
#'   study_data = study_data,
#'   meta_data = meta_data
#' )
#'
#' # Studies or data frame level
#' study_tables <- list(
#'   "sd1" = readRDS(system.file("extdata", "ship.RDS", package = "dataquieR")),
#'   "sd2" = readRDS(system.file("extdata", "ship.RDS", package = "dataquieR"))
#' )
#'
#' int_duplicate_content(
#'   level = "dataframe",
#'   study_segment = c("sd1", "sd2"),
#'   study_data = study_tables,
#'   meta_data = meta_data
#' )
#' }
#'
#' @family integrity_indicator_functions
#' @concept integrity_indicator
#' @keywords internal
util_int_duplicate_content_segment <- function(level = c("segment"),
                                               study_segment,
                                               study_data,
                                               meta_data) {
  # Segment level check ----

  # Check arguments ----

  level <- util_match_arg(level)

  # map metadata to study data
  prep_prepare_dataframes(.allow_empty = TRUE)

  # check that specified segments are included in the metadata
  old_study_segment <- study_segment
  study_segment <- intersect(study_segment,
                                 meta_data[[STUDY_SEGMENT]])

  if (length(old_study_segment) > length(study_segment)) {
    util_message(
      "The segments in the %s do not match the segments in %s, considering only the intersection",
      dQuote("meta_data"),
      dQuote("meta_data_segment"),
      applicability_problem = TRUE,
      intrinsic_applicability_problem = FALSE
    )
  }

  # Check for duplicated content  ----

  result <- lapply(setNames(nm = study_segment), function(current_segment) {
    vars_in_current_segment <-
      util_get_vars_in_segment(segment = current_segment,
                               meta_data = meta_data,
                               label_col = VAR_NAMES)

    ds1_seg <- util_remove_empty_rows(ds1) # - TODO: id_vars relevant?

    n_uniq <- nrow(unique(ds1_seg))

    res_tmp <- data.frame(
      check.names = FALSE,
      "Check" = "Duplicate records",
      "Segment" = current_segment,
      "Any duplicates" = ifelse(n_uniq < nrow(ds1_seg), TRUE, FALSE),
      "Number of duplicates" = nrow(ds1_seg) - n_uniq,
      "Percentage of duplicates" =
        round(100 * (nrow(ds1_seg) - n_uniq) / nrow(ds1_seg), 3),
      "GRADING" = ifelse(n_uniq < nrow(ds1_seg), 1, 0),
      stringsAsFactors = FALSE
    )

    if (res_tmp[[3]]) { # only if there are any duplicated observations
      vec_dup <- which(duplicated(ds1_seg)) # FIXME: the positions in the reduced data frame are mostly useless
      vec_dup <- NULL # TODO: Remove after fixing
    } else {
      vec_dup <- NULL
    }

    return(list(res_tmp, vec_dup))
  })

  res_df <- do.call(rbind.data.frame, lapply(result, `[[`, 1))
  duplicates_df <- do.call(cbind.data.frame, lapply(result, `[[`, 2))

  res_pipeline <- data.frame(
    "Segment" = res_df$Segment,
    "NUM_int_sts_dupl_content" = res_df$`Number of duplicates`, # TODO: Nowhere use numbers, always names, and return ordere here to use standardized names. labels for the not-pipelin-res can be read from an external file, late
    "PCT_int_sts_dupl_content" = res_df$`Percentage of duplicates`,
    "GRADING" = res_df$GRADING,
    stringsAsFactors = FALSE
  )

  return(list(
    SegmentData = res_df,
    SegmentTable = res_pipeline,
    Duplicates = duplicates_df
  ))


}

