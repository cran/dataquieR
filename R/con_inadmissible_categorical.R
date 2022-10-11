#' Detects variable levels not specified in metadata
#'
#' @description
#' For each categorical variable, value lists should be defined in the metadata.
#'  This implementation will examine, if all observed levels in the study data
#'  are valid.
#'
#' @details
#' ### ALGORITHM OF THIS IMPLEMENTATION:
#'
#'  - Remove missing codes from the study data (if defined in the metadata)
#'  - Interpretation of variable specific VALUE_LABELS as supplied in the
#'    metadata.
#'  - Identification of measurements not corresponding to the expected
#'    categories. Therefore two output data frames are generated:
#'    - on the level of observation to flag each undefined category, and
#'    - a summary table for each variable.
#'  - Values not corresponding to defined categories are removed in a data frame
#'    of modified study data
#'
#' @export
#'
#' @param resp_vars [variable list] the name of the measurement variables
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param threshold [numeric] from=0 to=100. a numerical value ranging
#'                                           from 0-100. Not yet implemented.
#'
#' @return a list with:
#' - `SummaryTable`: data frame summarizing inadmissible categories with the
#' columns:
#'   - `Variables`: variable name/label
#'   - `OBSERVED_CATEGORIES`: the categories observed in the study data
#'   - `DEFINED_CATEGORIES`: the categories defined in the metadata
#'   - `NON_MATCHING`: the categories observed but not defined
#'   - `NON_MATCHING_N`: the number of observations with categories not defined
#'   - `GRADING`: indicator TRUE/FALSE if inadmissible categorical values were
#'     observed
#' - `ModifiedStudyData`: study data having inadmissible categories removed
#' - `FlaggedStudyData`: study data having cases with inadmissible categories
#' flagged
#'
#' @importFrom utils capture.output
#' @seealso
#' [Online Documentation](
#' https://dataquality.ship-med.uni-greifswald.de/VIN_con_impl_inadmissible_categorical.html
#' )
con_inadmissible_categorical <- function(resp_vars = NULL, study_data,
                                         meta_data, label_col,
                                         threshold = NULL) {
  rvs <- resp_vars

  as_numeric_now_warn <- function(...) {
    suppressWarnings(as.numeric(...))
  }

  # map meta to study
  util_prepare_dataframes()

  util_correct_variable_use("resp_vars",
    allow_more_than_one = TRUE,
    allow_null = TRUE,
    allow_any_obs_na = TRUE,
    need_type = "integer | string"
  )


  # no variables defined?
  if (length(rvs) == 0) {
    if (all(is.na(meta_data[[VALUE_LABELS]]))) {
      util_error(paste0("No Variables with defined VALUE_LABELS."),
                 applicability_problem = TRUE)
    } else {
      util_warning("All variables with VALUE_LABELS in the metadata are used.",
                   applicability_problem = TRUE)
      rvs <-
        intersect(meta_data[[label_col]][!(is.na(meta_data[[VALUE_LABELS]]))],
                  colnames(ds1))
    }
  } else {
    # limits defined at all?
    if (all(is.na(
          meta_data[[VALUE_LABELS]][meta_data[[label_col]] %in% rvs]))) {
      util_error("No Variables with defined VALUE_LABELS.",
                 applicability_problem = TRUE)
    }
    # no limits for some variables?
    rvs2 <- meta_data[[label_col]][!(is.na(meta_data[[VALUE_LABELS]])) &
                                     meta_data[[label_col]] %in% rvs]
    if (length(rvs2) < length(rvs)) {
      util_warning(paste0("The variables ", rvs[!(rvs %in% rvs2)],
                          " have no defined VALUE_LABELS."),
                   applicability_problem = TRUE)
    }
    rvs <- rvs2
  }

  # Calculations ---------------------------------------------------------------

  sumdf1 <- data.frame(Variables = rvs)

  # which categories were used in the data
  obs_cats <- function(x) {
    as.character(unique(x[!is.na(x)]))
  }
  sumdf1$OBSERVED_CATEGORIES <- lapply(ds1[, rvs, drop = FALSE], obs_cats)

  # which categories were defined in metadata
  defcats <- lapply(lapply(setNames(util_map_labels(rvs,
                                                meta_data = meta_data,
                                                to = VALUE_LABELS,
                                                from = label_col,
                                                ifnotfound = NA), nm = rvs),
                       util_parse_assignments), unlist) # get for all item

  sumdf1$DEFINED_CATEGORIES <- mapply(lapply(defcats, names), defcats,
                                      SIMPLIFY = FALSE,
                                      FUN = setNames) # swap names and values

  # which used categories were not defined
  whichnot <- lapply(
    seq_along(sumdf1$OBSERVED_CATEGORIES),
    function(x) !(sumdf1$OBSERVED_CATEGORIES[[x]] %in%
                    sumdf1$DEFINED_CATEGORIES[[x]])
  )

  # extract those undefined
  sumdf1$NON_MATCHING <- lapply(
    seq_along(sumdf1$OBSERVED_CATEGORIES),
    function(x) paste0(sumdf1$OBSERVED_CATEGORIES[[x]][whichnot[[x]]])
  )

  sumdf1$NON_MATCHING <- lapply(
    sumdf1$NON_MATCHING,
    function(x) if (identical(x, character(0))) NA_character_ else x
  )

  sumdf1$NON_MATCHING_N <- NA

  # msdf = modified study data
  msdf <- ds1

  # count obs with undefined categories
  for (i in 1:dim(sumdf1)[1]) {
    if (!is.na(sumdf1$NON_MATCHING[i])) {
      if (all(!is.na(as_numeric_now_warn(unlist(sumdf1$NON_MATCHING[i]))))) {
        sumdf1$NON_MATCHING_N[i] <- sum(ds1[[paste(sumdf1$Variables[i])]] %in%
                                          as_numeric_now_warn(unlist(
                                            sumdf1$NON_MATCHING[i])),
          na.rm = TRUE
        )
        msdf[[paste(sumdf1$Variables[i])]][
          msdf[[paste(sumdf1$Variables[i])]] %in%
            as_numeric_now_warn(unlist(sumdf1$NON_MATCHING[i]))] <- NA
        ds1[[paste0(sumdf1$Variables[i], "_IAV")]] <-
          ifelse(ds1[[paste(sumdf1$Variables[i])]] %in%
                   as_numeric_now_warn(unlist(sumdf1$NON_MATCHING[i])), 1, 0)
      } else {
        sumdf1$NON_MATCHING_N[i] <- sum(ds1[[paste(sumdf1$Variables[i])]] %in%
                                          unlist(sumdf1$NON_MATCHING[i]),
          na.rm = TRUE
        )
        msdf[[paste(sumdf1$Variables[i])]][
          msdf[[paste(sumdf1$Variables[i])]] %in%
            as_numeric_now_warn(unlist(sumdf1$NON_MATCHING[i]))] <- NA
        ds1[[paste0(sumdf1$Variables[i], "_IAV")]] <-
          ifelse(ds1[[paste(sumdf1$Variables[i])]] %in%
                   as_numeric_now_warn(unlist(sumdf1$NON_MATCHING[i])), 1, 0)
      }
    }
  }

  # any IAVs?
  checkIAV <- names(ds1)[grep("_IAV", names(ds1))]
  if (length(checkIAV) > 0) {
    util_warning(paste0("The following variable(s): ",
                        paste0(checkIAV, collapse = ", "),
                        " flag(s) inadmissible values."),
                 applicability_problem = FALSE)
  }

  # attribute
  attr(msdf, "rmIAVcat") <- TRUE

  # handle lists
  sumdf1[["OBSERVED_CATEGORIES"]] <-
    as.vector(unlist(lapply(sumdf1[["OBSERVED_CATEGORIES"]],
                            paste, collapse = ", ")))
  sumdf1[["DEFINED_CATEGORIES"]] <-
    as.vector(unlist(lapply(sumdf1[["DEFINED_CATEGORIES"]],
                            paste, collapse = ", ")))
  sumdf1[["NON_MATCHING"]] <-
    as.vector(unlist(lapply(sumdf1[["NON_MATCHING"]],
                            paste, collapse = ", ")))
  sumdf1$NON_MATCHING <-
    ifelse(sumdf1$NON_MATCHING == "NA", "", sumdf1$NON_MATCHING)
  sumdf1$NON_MATCHING_N <-
    ifelse(is.na(sumdf1$NON_MATCHING_N), 0, sumdf1$NON_MATCHING_N)
  sumdf1[["GRADING"]] <-
    ifelse(sumdf1$NON_MATCHING_N > 0, 1, 0)

  return(list(SummaryTable = sumdf1, ModifiedStudyData = msdf,
              FlaggedStudyData = ds1))
}
