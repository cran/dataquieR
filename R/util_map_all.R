#' Maps label column metadata on study data variable names
#'
#' Maps a certain label column from the metadata
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

  if (!nrow(meta_data)) {
    util_error("%s is empty", sQuote("meta_data"))
  }

  if (length(label_col) != 1 || !is.character(label_col)) {
    util_error(
      c("label_col must be exactly 1 metadata attribute,",
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
    util_error("label_col %s not found in metadata. Did you mean %s?",
               dQuote(label_col), dQuote(fuzzy_match),
               applicability_problem = TRUE)
  }

  if (!VAR_NAMES %in% colnames(meta_data)) {
    util_error("VAR_NAMES not found in metadata.",
               applicability_problem = TRUE)
  }

  if (any(duplicated(meta_data[[VAR_NAMES]]), na.rm = TRUE)) {
    util_error(
      c("The following variable names are duplicated in the metadata",
        "and cannot be used as label therefore: %s"),
      paste0(collapse = ", ",
             dQuote(unique(meta_data[[VAR_NAMES]][(
               duplicated(meta_data[[VAR_NAMES]]))]))),
      applicability_problem = TRUE
    )
  }

  if (any(duplicated(meta_data[[label_col]]), na.rm = TRUE)) {
    util_error(
      c("The following %s are duplicated in the metadata",
        "and cannot be used as label therefore: %s"),
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
        "names are missing in the metadata: %s"),
      paste0("Variable No. #", which(is.na(meta_data[[VAR_NAMES]])),
             collapse = ", "), applicability_problem = TRUE
    )
  }

  if (any(is.na(meta_data[[label_col]]))) {
    util_error(
      c("For the following variables, some %s are missing in the metadata and",
        "cannot be used as label therefore: %s"),
      sQuote(label_col),
      paste0("Variable No. #", which(is.na(meta_data[[label_col]])),
             collapse = ", "),
      applicability_problem = TRUE
    )
  }

  ################# mapping ##################

  # select only relevant variables from study_data
  counter <- sum(!(colnames(study_data) %in% meta_data[[VAR_NAMES]]))
  if (ncol(study_data) > 0) {
    lost <- counter / ncol(study_data)
  } else {
    if (counter > 0) {
      lost <- Inf
    } else {
      lost <- 0
    }
  }

  if (lost > 0 && !getOption("dataquieR.RECORD_MISSMATCH_CHECKTYPE", # TODO: This may not be a suitable option name: Replace all occurences in dataquieR by ELEMENT_MISSMATCH_CHECKTYPE
                             "exact") %in%
      c("none", "subset_m")) {
    util_warning(
      "Lost %g%% of the study data because of missing/not assignable metadata",
      round(lost * 100, 1),
      applicability_problem = TRUE, integrity_indicator = "int_sts_element")
    util_message(
      paste("Did not find any metadata for the following",
             "variables from the study data: %s"),
      paste0(dQuote(colnames(study_data)[!(colnames(study_data) %in%
                                             meta_data[[VAR_NAMES]])]),
             collapse = ", "), integrity_indicator = "int_sts_element"
    )
  }

  counter <- sum(!(meta_data[[VAR_NAMES]] %in% colnames(study_data)))
  if (nrow(meta_data) > 0) {
    unlost <- counter / nrow(meta_data)
  } else {
    if (counter > 0) {
      unlost <- Inf
    } else {
      unlost <- 0
    }
  }

  if (unlost > 0 && !getOption("dataquieR.RECORD_MISSMATCH_CHECKTYPE",
                               "exact") %in%
      c("none", "subset_u")) {
    util_warning(
      "Lost %g%% of the metadata because of missing/not assignable study data",
      round(unlost * 100, 1),
      applicability_problem = TRUE, integrity_indicator = "int_sts_element",
      intrinsic_applicability_problem = TRUE)
    util_message(
      paste("Found metadata for the following variables",
            "not found in the study data: %s"),
      paste0(dQuote(meta_data[[VAR_NAMES]][!(meta_data[[VAR_NAMES]] %in%
                                               colnames(study_data))]),
             collapse = ", "), integrity_indicator = "int_sts_element"
    )
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
      "Mapping of metadata on study data yielded invalid variable labels: %s",
      paste0(dQuote(cn[invalid]), collapse = ", ")
    ))
  }

  if (any(dups)) {
    e <- c(e, sprintf(
      "Mapping of metadata on study data yielded duplicated variable labels: %s",
      paste0(dQuote(cn[dups]), collapse = ", ")
    ))
  }

  if (length(e) > 0) {
    util_error(paste0(e, collapse = "\n"), applicability_problem = TRUE)
  }

  colnames(ds1) <- cn

  return(list(df = ds1))
}
