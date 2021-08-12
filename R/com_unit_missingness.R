#' Counts all individuals with no measurements at all
#'
#' @description
#' This implementation examines a crude version of unit missingness or
#' unit-nonresponse (Kalton and Kasprzyk 1986), i.e. if all measurement
#' variables in the study data are missing for an observation it has unit
#' missingness.
#'
#' The function can be applied on stratified data. In this case strata_vars must
#' be specified.
#'
#' @details
#'
#' This implementations calculates a crude rate of unit-missingness. This type
#' of missingness may have several causes and is an important research outcome.
#' For example, unit-nonresponse may be selective regarding the targeted study
#' population or technical reasons such as record-linkage may cause
#' unit-missingness.
#'
#' It has to be discriminated form segment and item missingness, since different
#' causes and mechanisms may be the reason for unit-missingness.
#'
#' ### Hint
#' This function does not support a `resp_vars` argument but `id_vars`, which
#' have a roughly inverse logic behind: id_vars with values do not prevent a row
#' from being considered missing, because an ID is the only hint for a unit that
#' elsewise would not occur in the data at all.
#'
#' List function.
#'
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param id_vars [variable list] optional, a (vectorized) call of ID-variables
#'                                that should not be
#'                                considered in the calculation of unit-
#'                                missingness
#' @param strata_vars [variable] optional, a string or integer variable used for
#'                               stratification
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#'
#' @importFrom ggpubr ggballoonplot
#'
#' @return A list with:
#'   - `FlaggedStudyData`: [data.frame] with id-only-rows flagged in a column
#'                         `Unit_missing`
#'   - `SummaryData`: [data.frame] with numbers and percentages of unit
#'                                missingness
#'
#' @export
#'
#' @seealso
#' [Online Documentation](
#' https://dataquality.ship-med.uni-greifswald.de/VIN_com_impl_unit_missingness.html
#' )
com_unit_missingness <- function(study_data, meta_data, id_vars = NULL,
                                 strata_vars = NULL, label_col) {

  # map study and metadata
  util_prepare_dataframes()

  # correct variable usage
  util_correct_variable_use("id_vars",
    allow_more_than_one = TRUE,
    allow_null          = TRUE
  )

  util_correct_variable_use("strata_vars",
    allow_null = TRUE,
    need_type = "!float"
  )

  if (is.null(id_vars)) {
    util_warning(
      c("No ID-variables specified, all variables are",
        "considered to be measurements."),
      applicability_problem = TRUE
      )
  }

  # initialize result dataframe
  sumdf1 <- ds1

  # compute unit missingness for individuals having
  # NA in all columns (except ID-vars)
  if (!(is.null(id_vars)) || !(is.null(strata_vars))) {
    leave_out <- c()
    if (!(is.null(id_vars))) {
      util_correct_variable_use("id_vars",
        allow_na = TRUE, allow_more_than_one = TRUE,
        allow_null = TRUE, allow_all_obs_na = TRUE, allow_any_obs_na = TRUE
      )
      leave_out <- union(leave_out, id_vars)
    }
    if (!(is.null(strata_vars))) {
      util_correct_variable_use("strata_vars",
        allow_na = FALSE, allow_more_than_one = FALSE,
        allow_null = TRUE, allow_all_obs_na = FALSE, allow_any_obs_na = TRUE
      )
      leave_out <- union(leave_out, strata_vars)
    }
    sumdf1$Unit_missing <- as.integer(apply(ds1[, -which(names(ds1) %in%
                                                           leave_out)], 1,
                                            function(x) all(is.na(x))))
  } else {
    sumdf1$Unit_missing <- as.integer(apply(ds1, 1, function(x) all(is.na(x))))
  }

  UMR <- c(
    "N" = sum(sumdf1$Unit_missing, na.rm = TRUE),
    "%" = round(sum(sumdf1$Unit_missing, na.rm = TRUE) / dim(sumdf1)[1] * 100,
                digits = 2)
  )

  # summarize for strata_vars
  if (!(is.null(strata_vars))) {
    if (!(is.null(label_col)) & !(is.na(meta_data$VALUE_LABELS[
        meta_data[[label_col]]
          == strata_vars]))) {
      lab_string <- meta_data$VALUE_LABELS[meta_data$LABEL == strata_vars]
      sumdf1[[strata_vars]] <- util_assign_levlabs(sumdf1[[strata_vars]],
        string_of_levlabs = lab_string,
        splitchar = SPLIT_CHAR,
        assignchar = " = "
      )
    }

    sumdf2 <- as.data.frame.matrix(table(sumdf1[[strata_vars]],
                                         sumdf1$Unit_missing))
    if (!any(sumdf1$Unit_missing, na.rm = TRUE)) {
      sumdf2$N_UNIT_MISSINGS <- 0
    }
    colnames(sumdf2) <- c("N_OBS", "N_UNIT_MISSINGS")
    sumdf2[[strata_vars]] <- rownames(sumdf2)
    rownames(sumdf2) <- NULL
    sumdf2 <- sumdf2[, c(strata_vars, c("N_OBS", "N_UNIT_MISSINGS"))]
    sumdf2$"N_UNIT_MISSINGS_(%)" <- round(sumdf2$N_UNIT_MISSINGS /
                                            sumdf2$N_OBS * 100, digits = 2)
  }

  if (!(is.null(strata_vars))) {
    return(list(FlaggedStudyData = sumdf1, SummaryData = sumdf2))
  } else {
    return(list(FlaggedStudyData = sumdf1, SummaryData = UMR))
  }
}
