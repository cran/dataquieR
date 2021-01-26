#' Summarizes missingness for individuals in specific segments
#'
#' @description
#' ### This implementation can be applied in two use cases:
#'
#'  1. participation in study segments is not recorded by respective variables,
#'     e.g. a participant's refusal to attend a specific examination is not
#'     recorded.
#'  2. participation in study segments is recorded by respective
#'     variables.
#'
#' Use case *(1)* will be common in smaller studies. For the calculation of
#' segment missingness it is assumed that study variables are nested in
#' respective segments. This structure must be specified in the static metadata.
#' The R-function identifies all variables within each segment and returns TRUE
#' if all variables within a segment are missing, otherwise FALSE.
#'
#' Use case *(2)* assumes a more complex structure of study data and meta data.
#' The study data comprise so-called intro-variables (either TRUE/FALSE or codes
#' for non-participation). The column KEY_STUDY_SEGMENT in the metadata is
#' filled by variable-IDs indicating for each variable the respective
#' intro-variable. This structure has the benefit that subsequent calculation of
#' item missingness obtains correct denominators for the calculation of
#' missingness rates.
#'
#' @details
#' ### Implementation and use of thresholds
#' This implementation uses one threshold to discriminate critical from
#' non-critical values. If direction is high than all values below the
#' threshold_value are normal (displayed in dark blue in the plot and flagged
#' with grading = 0 in the dataframe). All values above the threshold_value are
#' considered critical. The more they deviate from the threshold the displayed
#' color shifts to dark red. All critical values are highlighted with grading =
#' 1 in the summary data frame. By default, highest values are always shown in
#' dark red irrespective of the absolute deviation.
#'
#' If direction is low than all values above the threshold_value are normal
#' (displayed in dark blue, grading = 0).
#'
#' ### Hint
#' This function does not support a `resp_vars` argument but `exclude_roles` to
#' specify variables not relevant for detecting a missing segment.
#'
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param group_vars [variable] the name of a variable used for grouping,
#'                              defaults to *NULL* for not grouping output
#' @param strata_vars [variable] the name of a variable used for stratification,
#'                               defaults to NULL for not grouping output
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param threshold_value [numeric] from=0 to=100. a numerical value ranging
#'                                                 from 0-100
#' @param direction [enum] low | high. "high" or "low", i.e. are deviations
#'                                     above/below the threshold critical
#' @param exclude_roles [variable roles] a character (vector) of variable roles
#'                                       not included
#'
#'
#' @return a list with:
#'   - `SummaryData`: data frame about segment missingness
#'   - `SummaryPlot`: ggplot2 heatmap plot: a heatmap-like graphic that
#'                    highlights critical values depending on the respective
#'                    threshold_value and direction.
#'
#' @export
#' @seealso
#' [Online Documentation](
#' https://dfg-qa.ship-med.uni-greifswald.de/VIN_com_impl_segment_missingness.html
#' )
com_segment_missingness <- function(study_data, meta_data, group_vars = NULL,
                                    strata_vars = NULL, label_col,
                                    threshold_value,
                                    direction, exclude_roles = "process") {

  #########
  # STOPS #
  #########
  if (missing(threshold_value) ||
      length(threshold_value) != 1 ||
      !is.numeric(threshold_value)) {
    util_warning(
      c("threshold_value should be a single number between 0 and 100.",
      "Invalid value specified, setting to 10%%."))
    threshold_value <- 10
  }

  if (missing(direction)) {
    util_warning(c(
      "No specification of threshold direction found.",
      "The function interprets values higher the threshold as violations."))
    direction <- "high"
  }

  if (length(direction) != 1) {
    util_error(
      "Parameter %s, if not missing, should be of length 1, but not %d.",
      dQuote("direction"), length(direction))
  }

  if (!(direction %in% c("low", "high"))) {
    util_error(
      "Parameter %s should be either %s or %s, but not %s.",
      dQuote("direction"),
      dQuote("low"), dQuote("high"), dQuote(direction)
    )
  }

  ####################
  # PREPS AND CHECKS #
  ####################

  # map meta to study
  util_prepare_dataframes()

  # correct variable usage
  util_correct_variable_use("strata_vars",
    allow_null = TRUE,
    need_type = "!float"
  )

  util_correct_variable_use("group_vars",
    allow_null = TRUE,
    need_type = "!float"
  )

  # browser()
  # exclude_roles = c("process", "not")

  # should some variables not be considered?
  if (VARIABLE_ROLE %in% names(meta_data)) {

    # a: not all roles specified found in metadata
    if (!(all(exclude_roles %in% meta_data[[VARIABLE_ROLE]]))) {
      if (any(exclude_roles %in% meta_data[[VARIABLE_ROLE]])) {
        util_warning(paste0(
          "Specified VARIABLE_ROLE(s): '",
          exclude_roles[!(exclude_roles %in% meta_data[[VARIABLE_ROLE]])],
          "' was not found in metadata, only: '",
          exclude_roles[exclude_roles %in% meta_data[[VARIABLE_ROLE]]],
          "' is used."
        ))

        exclude_roles <- exclude_roles[exclude_roles %in%
                                         meta_data[[VARIABLE_ROLE]]]

        which_vars_not <-
          meta_data[[label_col]][meta_data[[VARIABLE_ROLE]] %in% exclude_roles]
        if (missing(label_col)) {
          which_vars_not <-
            meta_data[[VAR_NAMES]][meta_data[[VARIABLE_ROLE]] %in%
                                     exclude_roles]
        }
        which_vars_not <- setdiff(which_vars_not, strata_vars)
        which_vars_not <- setdiff(which_vars_not, group_vars)
        if (length(intersect(names(ds1), which_vars_not)) > 0) {
          util_warning(paste0(
            "Study variables: ",
            paste(dQuote(intersect(names(ds1), which_vars_not)),
                  collapse = ", "),
            " are not considered due to their VARIABLE_ROLE."
          ))
        }
        ds1 <- ds1[, !(names(ds1) %in% which_vars_not)]
      } else {
        exclude_roles <- FALSE
        util_warning(
          c("Specified VARIABLE_ROLE(s) were not found in metadata.",
            "All variables are included here."))
      }


      # b: all roles are found in metadata
    } else {
      if (missing(exclude_roles)) {
        util_warning(
          c("Formal exclude_roles is used with default: all process variables",
            "are not included here."))
      }

      which_vars_not <- meta_data[[label_col]][meta_data[[VARIABLE_ROLE]] %in%
                                                 exclude_roles]
      if (missing(label_col)) {
        which_vars_not <- meta_data[[VAR_NAMES]][meta_data[[VARIABLE_ROLE]] %in%
                                                   exclude_roles]
      }
      which_vars_not <- setdiff(which_vars_not, strata_vars)
      which_vars_not <- setdiff(which_vars_not, group_vars)
      if (length(intersect(names(ds1), which_vars_not)) > 0) {
        util_warning(paste0(
          "Study variables: ", paste(dQuote(intersect(names(ds1),
                                                      which_vars_not)),
                                     collapse = ", "),
          " are not considered due to their VARIABLE_ROLE."
        ))
      }
      ds1 <- ds1[, !(names(ds1) %in% which_vars_not)]
    }
  } else {
    # since there are no roles defined exclusion is set to false
    exclude_roles <- FALSE
    util_warning(
      c("VARIABLE_ROLE has not been defined in the metadata,",
        "therefore all variables within segments are used."))
  }

  # Which segments?
  if (!("KEY_STUDY_SEGMENT" %in% names(meta_data))) {
    util_error("Metadata do not contain the column KEY_STUDY_SEGMENT.")
  }

  segs <- unique(meta_data$KEY_STUDY_SEGMENT[!(is.na(
    meta_data$KEY_STUDY_SEGMENT))])
  seg_names <- meta_data[[LONG_LABEL]][meta_data$VAR_NAMES %in% segs]

  if (length(seg_names) > 0) {
    names(segs) <- seg_names
  } else {
    names(segs) <- segs
  }

  # browser()
  # determine which vars per segment
  var_sets <- list()
  for (i in seq_along(segs)) {
    if (isFALSE(exclude_roles)) {
      meta_data_excl <- meta_data[!(meta_data$VAR_NAMES %in% segs), ]
      var_sets[[i]] <-
        meta_data_excl[[label_col]][meta_data_excl$KEY_STUDY_SEGMENT == segs[i]]
    } else {
      meta_data_excl <-
        meta_data[!(meta_data[[VARIABLE_ROLE]] %in% exclude_roles) &
                    !(meta_data$VAR_NAMES %in% segs), ]
      var_sets[[i]] <-
        meta_data_excl[[label_col]][meta_data_excl$KEY_STUDY_SEGMENT ==
                                      segs[i]][!is.na(meta_data_excl[[
                                        label_col]][
                                          meta_data_excl$KEY_STUDY_SEGMENT ==
                                            segs[i]])]
    }
  }

  # Which groups?
  if (!is.null(group_vars)) {
    # No. of group levels and labels
    ds1[[group_vars]] <- util_assign_levlabs(
      variable = ds1[[group_vars]],
      string_of_levlabs = meta_data$VALUE_LABELS[meta_data$LABEL == group_vars],
      splitchar = SPLIT_CHAR,
      assignchar = "="
    )
    gr <- unique(ds1[[group_vars]][!is.na(ds1[[group_vars]])])
    gr <- gr[order(gr)]
    # covariables for plot
    cvs <- c(group_vars, "Examinations")

    # missings in grouping variable?
    ds1 <- ds1[!is.na(ds1[[group_vars]]), ]
  } else {
    gr <- 1
    group_vars <- "Group"
    ds1$Group <- 1
    cvs <- "Examinations"
  }

  if (!is.null(strata_vars)) {
    # No. of strata levels and labels
    if (dim(ds1)[1] != dim(ds1[!is.na(strata_vars), ])[1]) {
      ds1 <- ds1[!is.na(strata_vars), ]
      util_warning(paste0("Some observations in ", strata_vars,
                          " are NA and were removed."))
    }

    ds1[[strata_vars]] <- util_assign_levlabs(
      variable = ds1[[strata_vars]],
      string_of_levlabs = meta_data$VALUE_LABELS[meta_data$LABEL ==
                                                   strata_vars],
      splitchar = SPLIT_CHAR,
      assignchar = "="
    )
    strata <- unique(ds1[[strata_vars]])[!is.na(unique(ds1[[strata_vars]]))]
    # covariables for plot
    cvs <- c(strata_vars, group_vars, "Examinations")
  }

  # create result dataframe by factor combinations
  if (is.null(strata_vars)) {
    res_df <- expand.grid(Group = gr, Examinations = names(segs))
    colnames(res_df) <- c(group_vars, "Examinations")
  } else {
    res_df <- expand.grid(
      Strata = strata, Group = gr, Examinations = seg_names,
      stringsAsFactors = TRUE
    )
    colnames(res_df) <- c(strata_vars, group_vars, "Examinations")
    res_df <- res_df[order(res_df[[strata_vars]], res_df[[group_vars]]), ]
  }

  ################
  # CALCULATIONS #
  ################

  myfun <- function(x) {
    all(is.na(x))
  }
  Ns <- c()
  Ms <- c()

  if (is.null(strata_vars)) {
    for (j in seq_along(segs)) {
      for (i in seq_along(gr)) {
        Ns <- c(Ns, dim(ds1[ds1[[group_vars]] == gr[i], ])[1])
        Ms <- c(Ms, sum(apply(ds1[ds1[[group_vars]] == gr[i],
                                  c(as.character(unlist(var_sets[j]))),
                                  drop = FALSE], 1, myfun)))
      }
    }
  } else {
    for (i in seq_along(strata)) {
      for (j in seq_along(gr)) {
        for (k in seq_along(segs)) {
          Ns <- c(Ns, dim(ds1[ds1[[strata_vars]] == strata[i] &
                                ds1[[group_vars]] == gr[j], ])[1])
          Ms <- c(Ms, sum(apply(ds1[
            ds1[[strata_vars]] == strata[i] & ds1[[group_vars]] == gr[j],
            c(as.character(unlist(var_sets[k])))
          ], 1, myfun)))
        }
      }
    }
  }

  res_df$"No. of Participants" <- Ns
  res_df$"No. of missing segments" <- Ms
  res_df$"(%) of missing segments" <- round(res_df$`No. of missing segments` /
                                              res_df$`No. of Participants` *
                                              100, digits = 2)
  res_df$"(%) of missing segments" <-
    as.numeric(res_df$"(%) of missing segments")
  res_df$threshold <- threshold_value
  res_df$direction <- direction

  if (direction == "high") {
    res_df$grading <- ifelse(res_df$"(%) of missing segments" >
                               threshold_value, 1, 0)
  } else {
    res_df$grading <- ifelse(res_df$"(%) of missing segments" <
                               threshold_value, 1, 0)
  }

  # PLOT
  inversion <- ifelse(direction == "low", 1, 0)

  # order result data frame by grouping variable
  if (is.null(strata_vars)) {
    # order result data frame by grouping variable
    res_df <- res_df[order(res_df[[group_vars]]), ]
    p <- util_heatmap_1th(
      df = res_df, cat_vars = cvs, values = "(%) of missing segments",
      right_intv = TRUE, threshold = threshold_value,
      invert = inversion
    )
  } else {
    # order result data frame by grouping variable
    res_df <- res_df[order(res_df[[strata_vars]], res_df[[group_vars]]), ]
    p <- util_heatmap_1th(
      df = res_df, cat_vars = cvs[-1], values = "(%) of missing segments",
      right_intv = TRUE, threshold = threshold_value,
      invert = inversion, strata = strata_vars
    )
  }

  return(list(SummaryData = res_df, SummaryPlot = p))
}
