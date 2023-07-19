#' Detects variable levels not specified in metadata
#'
#' @description
#' For each categorical variable, value lists should be defined in the metadata.
#'  This implementation will examine, if all observed levels in the study data
#'  are valid.
#'
#' @details
#' ### Algorithm of this implementation:
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
#' @param threshold_value [numeric] from=0 to=100. a numerical value ranging
#'                                           from 0-100.
# @param use_value_labels [logical] If set to `TRUE`, the output will report value
#                             labels. Otherwise, the output will give only the
#                             numerical codes of value labels.

#'
#' @return a list with:
#' - `SummaryData`: data frame summarizing inadmissible categories with the
#' columns:
#'   - `Variables`: variable name/label
#'   - `OBSERVED_CATEGORIES`: the categories observed in the study data
#'   - `DEFINED_CATEGORIES`: the categories defined in the metadata
#'   - `NON_MATCHING`: the categories observed but not defined
#'   - `NON_MATCHING_N`: the number of observations with categories not defined
#'   - `NON_MATCHING_N_PER_CATEGORY`: the number of observations for each of the
#'                unexpected categories
#'   - `GRADING`: indicator TRUE/FALSE if inadmissible categorical values were
#'     observed (more than indicated by the `threshold_value`)
#' - `SummaryTable`: data frame for the `dataquieR` pipeline reporting the number
#'                   and percentage of inadmissible categorical values
#' - `ModifiedStudyData`: study data having inadmissible categories removed
#' - `FlaggedStudyData`: study data having cases with inadmissible categories
#' flagged
#'
#' @importFrom utils capture.output
#' @seealso
#' [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_con_impl_inadmissible_categorical.html
#' )
con_inadmissible_categorical <- function(resp_vars = NULL, study_data,
                                         meta_data, label_col,
                                         threshold_value = 0
                                         #, use_value_labels = FALSE
                                         ) {

  # preps ----------------------------------------------------------------------

  as_numeric_now_warn <- function(...) {
    suppressWarnings(as.numeric(...))
  }

  # map metadata to study data
  prep_prepare_dataframes()

  if (!VALUE_LABELS %in% colnames(meta_data)) {
    util_error(paste0("Function con_inadmissible_categorical requires",
                      "the metadata column VALUE_LABELS."),
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }

  util_correct_variable_use("resp_vars",
                            allow_more_than_one = TRUE,
                            allow_null = TRUE,
                            allow_any_obs_na = TRUE,
                            need_type = "integer | string"
  )

  if (length(resp_vars) == 0) {
    # Select all variables with VALUE_LABELS (if any), if resp_vars were not specified.
    if (all(util_empty(meta_data[[VALUE_LABELS]]))) {
      util_error(paste0("No variables with defined VALUE_LABELS."),
                 applicability_problem = TRUE,
                 intrinsic_applicability_problem = TRUE)
    } else {
      util_message("All variables with VALUE_LABELS in the metadata are used.",
                   applicability_problem = TRUE,
                   intrinsic_applicability_problem = TRUE)
      resp_vars <-
        intersect(meta_data[[label_col]][!(util_empty(meta_data[[VALUE_LABELS]]))],
                  colnames(ds1))
    }
  } else {
    # Check for specified resp_vars whether VALUE_LABELS have been defined at all.
    if (all(util_empty(
      meta_data[[VALUE_LABELS]][meta_data[[label_col]] %in% resp_vars]))) {
      util_error("No variables with defined VALUE_LABELS.",
                 applicability_problem = TRUE,
                 intrinsic_applicability_problem = TRUE)
    }
    # check whether VALUE_LABELS are missing for some of the specified resp_vars
    rvs <- meta_data[[label_col]][!(util_empty(meta_data[[VALUE_LABELS]])) &
                                    meta_data[[label_col]] %in% resp_vars]
    if (length(rvs) < length(resp_vars)) {
      util_message(paste0("The variables ", resp_vars[!(resp_vars %in% rvs)],
                          " have no defined VALUE_LABELS."), # TODO: SCALE_TYPE, when avail.
                   applicability_problem = TRUE,
                   intrinsic_applicability_problem = TRUE)
    }
    resp_vars <- rvs
  }

  util_expect_scalar(threshold_value,
                     check_type = util_is_numeric_in(min = 0, max = 100))

  #util_expect_scalar(use_value_labels, check_type = is.logical)

  # Calculations ---------------------------------------------------------------

  sumdf1 <- data.frame(Variables = resp_vars)

  # Which categories were used in the study data?
  get_obs_cats <- function(x) {
    as.character(sort(unique(x[!is.na(x)])))
  }
  sumdf1$OBSERVED_CATEGORIES <- lapply(ds1[, resp_vars, drop = FALSE], get_obs_cats)

  # Which categories were defined in metadata?
  def_cats <- lapply(lapply(
    setNames(
      util_map_labels(
        resp_vars,
        meta_data = meta_data,
        to = VALUE_LABELS,
        from = label_col,
        ifnotfound = NA
      ),
      nm = resp_vars
    ),
    util_parse_assignments
  ), unlist)
  sumdf1$DEFINED_CATEGORIES <- mapply(lapply(def_cats, names), def_cats,
                                      SIMPLIFY = FALSE,
                                      FUN = setNames) # swap names and values

  # Which categories were used in the study data but not defined in the metadata?
  which_not <- lapply(
    seq_along(sumdf1$OBSERVED_CATEGORIES),
    function(x) !(sumdf1$OBSERVED_CATEGORIES[[x]] %in%
                    sumdf1$DEFINED_CATEGORIES[[x]])
  )
  sumdf1$NON_MATCHING <- lapply(
    seq_along(sumdf1$OBSERVED_CATEGORIES),
    function(x) paste0(sumdf1$OBSERVED_CATEGORIES[[x]][which_not[[x]]])
  )
  # If there are no unexpected categories, the entry should be NA (instead of "character(0)").
  sumdf1$NON_MATCHING <- lapply(
    sumdf1$NON_MATCHING,
    function(x) if (identical(x, character(0))) NA_character_ else x
  )

  # Count the number of non-matching observations in total and per unexpected category.
  sumdf1$NON_MATCHING_N <- NA
  sumdf1$NON_MATCHING_N_PER_CATEGORY <- NA
  # msdf = modified study data
  msdf <- ds1

  for (i in seq_len(nrow(sumdf1))) {
    if (!is.na(sumdf1$NON_MATCHING[i])) {
      if (all(!is.na(as_numeric_now_warn(unlist(sumdf1$NON_MATCHING[i]))))) {
        sumdf1$NON_MATCHING_N[i] <- sum(ds1[[paste(sumdf1$Variables[i])]] %in%
                                          as_numeric_now_warn(unlist(
                                            sumdf1$NON_MATCHING[i])),
          na.rm = TRUE
        )
        n_per_cat <- table(ds1[[sumdf1$Variables[i]]][which(
          ds1[[sumdf1$Variables[i]]] %in% as_numeric_now_warn(unlist(sumdf1$NON_MATCHING[i])))])
        sumdf1$NON_MATCHING_N_PER_CATEGORY[i] <- paste(
          paste0(n_per_cat, " (", dQuote(names(n_per_cat)), ")"),
          collapse = ", ")
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
        n_per_cat <- table(ds1[[sumdf1$Variables[i]]][which(
          ds1[[sumdf1$Variables[i]]] %in% unlist(sumdf1$NON_MATCHING[i]))])
        sumdf1$NON_MATCHING_N_PER_CATEGORY[i] <- paste(
          paste0(n_per_cat, " (", dQuote(names(n_per_cat)), ")"),
          collapse = ", ")
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
    if (!.called_in_pipeline) util_message(paste0("The following variable(s): ",
                        paste0(checkIAV, collapse = ", "),
                        " flag(s) inadmissible values."),
                 applicability_problem = FALSE)
  }

  # attribute
  attr(msdf, "rmIAVcat") <- TRUE

  # if (use_value_labels) {
  #   sumdf1$DEFINED_CATEGORIES <- lapply(sumdf1[["DEFINED_CATEGORIES"]], function(x) {
  #     if (all(!is.na(as_numeric_now_warn(x)))) {
  #       x <- paste0(x, " (", dQuote(names(x)), ")")
  #     }
  #     x
  #   })
  # }

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
  sumdf1$NON_MATCHING_N_PER_CATEGORY <-
    ifelse(is.na(sumdf1$NON_MATCHING_N_PER_CATEGORY), 0, sumdf1$NON_MATCHING_N_PER_CATEGORY)
  sumdf1[["GRADING"]] <-
    ifelse(sumdf1$NON_MATCHING_N > threshold_value / 100 * nrow(ds1), 1, 0)

  sumdf2 <- sumdf1[, c("Variables", "NON_MATCHING_N")]
  colnames(sumdf2)[2] <- "NUM_con_rvv_icat"
  sumdf2$PCT_con_rvv_icat <- round(sumdf2$NUM_con_rvv_icat / nrow(ds1) * 100, 1)
  sumdf2$GRADING <- sumdf1$GRADING
  sumdf2$FLG_con_rvv_icat <- ifelse(sumdf2$GRADING == 1, TRUE, FALSE)

  return(list(SummaryData = sumdf1,
              SummaryTable = sumdf2,
              ModifiedStudyData = msdf,
              FlaggedStudyData = ds1))
}
