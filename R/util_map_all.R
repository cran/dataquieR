#' Maps label column meta data on study data variable names
#'
#' Maps a certain label column from the meta data
#' to the study data frame.
#'
#' @param label_col the variable of the metadata that contains the variable
#'                  names of the study data
#' @param study_data the name of the data frame that contains the measurements
#' @param meta_data the name of the data frame that contains metadata attributes
#'                  of study data
#'
#' @return [list] with slot `df` with a study data frame with mapped column
#'                names
#'
#' @importFrom utils adist
util_map_all <- function(label_col = VAR_NAMES,
                         study_data,
                         meta_data) {

  ################# checks ##################

  if (length(label_col) != 1 || !is.character(label_col)) {
    util_error(
      c("label_col must be exactly 1 meta data attribute,",
        "neither a vector nor NULL."), applicability_problem = TRUE)
  }

  if (!label_col %in% colnames(meta_data)) {
    fuzzy_match <- vapply(
      label_col,
      function(v) {
        colnames(meta_data)[which.min(adist(trimws(v),
                                            trimws(colnames(meta_data)),
                                            ignore.case = TRUE, fixed = TRUE))]
      },
      ""
    )
    util_error("label_col %s not found in meta data. Did you mean %s?",
               dQuote(label_col), dQuote(fuzzy_match),
               applicability_problem = TRUE)
  }

  if (!VAR_NAMES %in% colnames(meta_data)) {
    util_error("VAR_NAMES not found in meta data.",
               applicability_problem = TRUE)
  }

  if (any(duplicated(meta_data[[VAR_NAMES]]), na.rm = TRUE)) {
    util_error(
      c("The following variable names are duplicated in the meta",
        "data and cannot be used as label therefore: %s"),
      paste0(collapse = ", ",
             dQuote(unique(meta_data[[VAR_NAMES]][(
               duplicated(meta_data[[VAR_NAMES]]))]))),
      applicability_problem = TRUE
    )
  }

  if (any(duplicated(meta_data[[label_col]]), na.rm = TRUE)) {
    util_error(
      c("The following %s are duplicated in the meta",
        "data and cannot be used as label therefore: %s"),
      sQuote(label_col),
      paste0(collapse = ", ",
             dQuote(unique(meta_data[[label_col]][(
               duplicated(meta_data[[label_col]]))]))),
      applicability_problem = TRUE
    )
  }

  if (any(is.na(meta_data[[VAR_NAMES]]))) {
    util_error(
      c("For the following variables, some variable",
        "names are missing in the meta data: %s"),
      paste0("Variable No. #", which(is.na(meta_data[[VAR_NAMES]])),
             collapse = ", "), applicability_problem = TRUE
    )
  }

  if (any(is.na(meta_data[[label_col]]))) {
    util_error(
      c("For the following variables, some %s are missing in the meta data and",
        "cannot be used as label therefore: %s"),
      sQuote(label_col),
      paste0("Variable No. #", which(is.na(meta_data[[label_col]])),
             collapse = ", "),
      applicability_problem = TRUE
    )
  }

  ################# mapping ##################

  # select only relevant variables from study_data
  lost <- sum(!(colnames(study_data) %in% meta_data[[VAR_NAMES]])) /
    ncol(study_data)
  if (lost > 0) {
    util_warning(
      "Lost %g%% of the study data because of missing/not assignable meta-data",
      round(lost * 100, 1),
      applicability_problem = TRUE)
    message(sprintf(
      paste("Did not find any meta data for the following",
             "variables from the study data: %s"),
      paste0(dQuote(colnames(study_data)[!(colnames(study_data) %in%
                                             meta_data[[VAR_NAMES]])]),
             collapse = ", ")
    ))
  }
  unlost <- sum(!(meta_data[[VAR_NAMES]] %in% colnames(study_data))) /
    nrow(meta_data)
  if (unlost > 0) {
    util_warning(
      "Lost %g%% of the meta data because of missing/not assignable study-data",
      round(unlost * 100, 1),
      applicability_problem = TRUE)
    message(sprintf(
      paste("Found meta data for the following variables",
            "not found in the study data: %s"),
      paste0(dQuote(meta_data[[VAR_NAMES]][!(meta_data[[VAR_NAMES]] %in%
                                               colnames(study_data))]),
             collapse = ", ")
    ))
  }

  ds1 <- study_data[, colnames(study_data) %in% meta_data[[VAR_NAMES]],
                    drop = FALSE]

  # select variable name from meta_data
  cn <- util_map_labels(colnames(ds1), meta_data, label_col, ifnotfound = "")

  invalid <- trimws(cn) == ""
  invalid[is.na(invalid)] <- TRUE

  dups <- duplicated(cn)
  dups[is.na(dups)] <- TRUE

  e <- character(0)
  if (any(invalid)) {
    e <- c(e, sprintf(
      "Mapping of meta on study data yielded invalid variable labels: %s",
      paste0(dQuote(cn[invalid]), collapse = ", ")
    ))
  }

  if (any(dups)) {
    e <- c(e, sprintf(
      "Mapping of meta on study data yielded duplicated variable labels: %s",
      paste0(dQuote(cn[dups]), collapse = ", ")
    ))
  }

  if (length(e) > 0) {
    util_error(paste0(e, collapse = "\n"), applicability_problem = TRUE)
  }

  colnames(ds1) <- cn

  return(list(df = ds1))
}
