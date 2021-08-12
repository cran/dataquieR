#' Generate a stratified full DQ report
#'
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param ... arguments to be passed to all called indicator functions if
#'            applicable.
#' @param label_col [variable attribute] the name of the column in the
#'                                       metadata with labels of variables
#' @param meta_data_split [variable attribute] name of a meta data attribute to
#'                                             split the report in sections of
#'                                             variables, e.g. all blood-
#'                                             pressure. By default, reports are
#'                                             split by [KEY_STUDY_SEGMENT]
#'                                             if available.
#' @param study_data_split [variable] Name of a study variable to stratify the
#'                                    report by, e.g. the study centers.
#' @param ... passed through to [dq_report]
#'
#' @seealso [dq_report]
#'
#' @export
#'
#' @examples
#' \dontrun{ # really long-running example.
#' load(system.file("extdata", "study_data.RData", package = "dataquieR"))
#' load(system.file("extdata", "meta_data.RData", package = "dataquieR"))
#' rep <- dq_report_by(study_data, meta_data, label_col =
#'   LABEL, study_data_split = "CENTER_0")
#' rep <- dq_report_by(study_data, meta_data,
#'   label_col = LABEL, study_data_split = "CENTER_0",
#'   meta_data_split = NULL
#' )
#' }
dq_report_by <- function(study_data, meta_data, label_col, meta_data_split =
                           KEY_STUDY_SEGMENT, study_data_split, ...) {
  if (missing(label_col)) {
    label_col <- VAR_NAMES
  }
  util_prepare_dataframes(.replace_missings = FALSE)
  if (!missing(study_data_split)) {
    .study_data_split <- study_data_split
  }
  if (label_col != VAR_NAMES && !missing(study_data_split)) {
    study_data_split <-
      util_map_labels(study_data_split, meta_data, VAR_NAMES, label_col)
  }
  if (missing(study_data_split) || is.null(study_data_split) ||
      !study_data_split %in% colnames(study_data)) {
    .sd_list <- list(all_observations = study_data)
  } else {
    .sd_list <- split(study_data, study_data[[study_data_split]])
    names(.sd_list) <- paste(.study_data_split, "=", names(.sd_list))
  }
  if (missing(meta_data_split)) {
    if (KEY_STUDY_SEGMENT %in% colnames(meta_data)) {
      meta_data_split <- KEY_STUDY_SEGMENT
    } else {
      meta_data_split <- NULL
    }
  } else if (!is.null(meta_data_split)) {
    if (!(meta_data_split %in% colnames(meta_data))) {
      util_error("No meta data attribute %s found for segmenting DQ report.",
                 dQuote(meta_data_split))
    }
    if (meta_data_split != KEY_STUDY_SEGMENT) {
      util_error(
        "Only KEY_STUDY_SEGMENT is supported for meta_data_split up to now.")
    }
  }
  key_cols <- colnames(meta_data)[startsWith(colnames(meta_data), "KEY_")]
  split_segments <- FALSE
  if (!is.null(meta_data_split)) {
    split_segments <- TRUE
    segments <- unique(meta_data[[meta_data_split]])
    if (label_col != VAR_NAMES && all(segments %in% meta_data[[VAR_NAMES]])) {
      segmentNames <- util_map_labels(segments, meta_data, label_col)
    } else {
      segmentNames <- segments
    }
    vars_in_segment <- lapply(setNames(segments, nm = segmentNames),
                              function(segment) {
      vars <- meta_data[meta_data[[meta_data_split]] == segment, VAR_NAMES]
      repeat {
        referred_vars <-
          unique(unlist(meta_data[meta_data[[VAR_NAMES]] %in% vars, key_cols],
                        recursive = TRUE))
        referred_vars <- referred_vars[!is.na(referred_vars)]
        if (all(referred_vars %in% vars)) {
          break
        } else {
          vars <- union(vars, referred_vars)
        }
      }
      vars
    })
  } else {
    vars_in_segment <- list(all_variables = meta_data[[VAR_NAMES]])
  }

  lapply(.sd_list, function(sd, md = meta_data) {
    lapply(vars_in_segment, function(vars_in_segment) {
      before <- length(vars_in_segment)
      vars_in_segment <- intersect(vars_in_segment, colnames(sd))
      after <- length(vars_in_segment)
      if (after < before) {
        util_warning(
          "Lost %d variables due to mapping problems. %d variables left.",
          before - after, after, applicability_problem = TRUE)
      }
      sd <- sd[, vars_in_segment, FALSE]
      md <- md[md[[VAR_NAMES]] %in% vars_in_segment, , FALSE]
      dq_report(study_data = sd, meta_data = md, label_col = label_col,
                split_segments = split_segments, ...)
    })
  })
}
