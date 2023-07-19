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
#' Use case *(2)* assumes a more complex structure of study data and metadata.
#' The study data comprise so-called intro-variables (either TRUE/FALSE or codes
#' for non-participation). The column `PART_VAR` in the metadata is
#' filled by variable-IDs indicating for each variable the respective
#' intro-variable. This structure has the benefit that subsequent calculation of
#' item missingness obtains correct denominators for the calculation of
#' missingness rates.
#'
#' @details
#' ### Implementation and use of thresholds
#' This implementation uses one threshold to discriminate critical from
#' non-critical values. If direction is above than all values below the
#' threshold_value are normal (displayed in dark blue in the plot and flagged
#' with GRADING = 0 in the dataframe). All values above the threshold_value are
#' considered critical. The more they deviate from the threshold the displayed
#' color shifts to dark red. All critical values are highlighted with GRADING =
#' 1 in the summary data frame. By default, highest values are always shown in
#' dark red irrespective of the absolute deviation.
#'
#' If direction is below than all values above the threshold_value are normal
#' (displayed in dark blue, GRADING = 0).
#'
#' ### Hint
#' This function does not support a `resp_vars` argument but `exclude_roles` to
#' specify variables not relevant for detecting a missing segment.
#'
#' List function.
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
#' @param meta_data_segment [data.frame] Segment level metadata. Optional.
#' @param threshold_value [numeric] from=0 to=100. a numerical value ranging
#'                                                 from 0-100
#' @param direction [enum] low | high. "high" or "low", i.e. are deviations
#'                                     above/below the threshold critical. This argument is deprecated and replaced by *color_gradient_direction*.
#' @param color_gradient_direction [enum] above | below. "above" or "below", i.e. are deviations
#'                                     above or below the threshold critical? (default: above)
#' @param exclude_roles [variable roles] a character (vector) of variable roles
#'                                       not included
#'
#' @param expected_observations [enum] HIERARCHY | ALL | SEGMENT. If ALL, all
#'                                     observations are expected to comprise
#'                                     all study segments. If SEGMENT, the
#'                                     `PART_VAR` is expected to point
#'                                     to a variable with values of 0 and 1,
#'                                     indicating whether the variable was
#'                                     expected to be observed for each data
#'                                     row. If HIERARCHY, this is also
#'                                     checked recursively, so, if a variable
#'                                     points to such a participation variable,
#'                                     and that other variable does has also
#'                                     a `PART_VAR` entry pointing
#'                                     to a variable, the observation of the
#'                                     initial variable is only
#'                                     expected, if both segment variables are
#'                                     1.
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
#' https://dataquality.qihs.uni-greifswald.de/VIN_com_impl_segment_missingness.html
#' )
com_segment_missingness <- function(study_data, meta_data, group_vars = NULL,
                                    meta_data_segment,
                                    strata_vars = NULL, label_col,
                                    threshold_value,
                                    direction, color_gradient_direction,
                                    expected_observations = c("HIERARCHY",
                                                              "ALL",
                                                              "SEGMENT"),
                                    exclude_roles =
                                      c(VARIABLE_ROLES$PROCESS)) {

  #########
  # STOPS #
  #########
  if (!missing(meta_data_segment)) {
    meta_data_segment <- util_expect_data_frame(meta_data_segment)
  } else {
    meta_data_segment <- data.frame()
  }
  util_expect_scalar(expected_observations, allow_more_than_one = TRUE)
  expected_observations <- match.arg(expected_observations)
  util_expect_scalar(expected_observations)

  if (missing(threshold_value) ||
      length(threshold_value) != 1 ||
      !is.numeric(threshold_value)) {
    if (!missing(threshold_value) || !.called_in_pipeline) util_message(
      c("threshold_value should be a single number between 0 and 100.",
      "Invalid value specified, setting to 10%%."),
      applicability_problem = TRUE)
    threshold_value <- 10
  }

  if (missing(color_gradient_direction)) {
    if (!.called_in_pipeline) util_message(c(
      "No specification of color gradient direction found.",
      "The function interprets values above the threshold as violations."),
      applicability_problem = TRUE)
    color_gradient_direction <- "above"
  }

  if (length(color_gradient_direction) != 1) {
    util_error(
      "Parameter %s, if not missing, should be of length 1, but not %d.",
      dQuote("color_gradient_direction"), length(color_gradient_direction),
      applicability_problem = TRUE)
  }

  if (!all(color_gradient_direction %in% c("above", "below"))) {
    util_error(
      "Parameter %s should be either %s or %s, but not %s.",
      dQuote("color_gradient_direction"),
      dQuote("above"), dQuote("below"), dQuote(color_gradient_direction),
      applicability_problem = TRUE
    )
  }

  if (!(missing(direction))) {
    if (direction %in% c("high", "low")) {
      if ((direction == "low" && color_gradient_direction == "above") ||
          (direction == "high" && color_gradient_direction == "below")) {
        util_error(
          "Conflicting options for %s and %s (%s is deprecated).",
          dQuote("color_gradient_direction"),
          dQuote("direction"), dQuote("direction"),
          applicability_problem = TRUE
        )
      } else {
        util_warning("%s is deprecated.", dQuote("direction"),
                     applicability_problem = FALSE)
      }
    } else {
      util_warning("%s is deprecated.", dQuote("direction"),
                   applicability_problem = FALSE)
    }
  }


  ####################
  # PREPS AND CHECKS #
  ####################

  # map meta to study
  prep_prepare_dataframes()

  if (expected_observations != "ALL" && !(PART_VAR %in%
                                          colnames(meta_data))) {
    util_warning(c("For %s = %s, a column %s is needed in %s. Falling",
                   "back to %s = %s."),
                 sQuote("expected_observations"),
                 dQuote(expected_observations),
                 dQuote(PART_VAR),
                 sQuote("meta_data"),
                 sQuote("expected_observations"),
                 dQuote("ALL"),
                 applicability_problem = TRUE
    )
    expected_observations <- "ALL"
  }

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
        ), applicability_problem = TRUE)

        exclude_roles <- exclude_roles[exclude_roles %in%
                                         meta_data[[VARIABLE_ROLE]]]

        which_vars_not <-
          meta_data[[label_col]][meta_data[[VARIABLE_ROLE]] %in%
                                   c(exclude_roles, VARIABLE_ROLES$SUPPRESS)]
        if (missing(label_col)) {
          which_vars_not <-
            meta_data[[VAR_NAMES]][meta_data[[VARIABLE_ROLE]] %in%
                                     c(exclude_roles, VARIABLE_ROLES$SUPPRESS)]
        }
        which_vars_not <- setdiff(which_vars_not, strata_vars)
        which_vars_not <- setdiff(which_vars_not, group_vars)
        if (length(intersect(names(ds1), which_vars_not)) > 0) {
          util_message(paste0(
            "Study variables: ",
            paste(dQuote(intersect(names(ds1), which_vars_not)),
                  collapse = ", "),
            " are not considered due to their VARIABLE_ROLE."
          ), applicability_problem = TRUE,
          intrinsic_applicability_problem = TRUE)
        }
        ds1 <- ds1[, !(names(ds1) %in% which_vars_not)]
      } else {
        exclude_roles <- FALSE
        util_warning(
          c("Specified VARIABLE_ROLE(s) were not found in metadata.",
            "All variables are included here."),
          applicability_problem = TRUE)
      }


      # b: all roles are found in metadata
    } else {
      if (missing(exclude_roles)) {
        if (!.called_in_pipeline) util_message(
          c("Formal exclude_roles is used with default: all process variables",
            "are not included here."), applicability_problem = TRUE)
      }

      which_vars_not <- meta_data[[label_col]][meta_data[[VARIABLE_ROLE]] %in%
                                                 c(exclude_roles,
                                                   VARIABLE_ROLES$SUPPRESS)]
      if (missing(label_col)) {
        which_vars_not <- meta_data[[VAR_NAMES]][meta_data[[VARIABLE_ROLE]] %in%
                                                   c(exclude_roles,
                                                     VARIABLE_ROLES$SUPPRESS)]
      }
      which_vars_not <- setdiff(which_vars_not, strata_vars)
      which_vars_not <- setdiff(which_vars_not, group_vars)
      if (length(intersect(names(ds1), which_vars_not)) > 0) {
        util_message(paste0(
          "Study variables: ", paste(dQuote(intersect(names(ds1),
                                                      which_vars_not)),
                                     collapse = ", "),
          " are not considered due to their VARIABLE_ROLE."
        ),
        applicability_problem = TRUE,
        intrinsic_applicability_problem = TRUE)
      }
      ds1 <- ds1[, !(names(ds1) %in% which_vars_not)]
    }
  } else {
    # since there are no roles defined exclusion is set to false
    exclude_roles <- FALSE
    util_message(
      c("VARIABLE_ROLE has not been defined in the metadata,",
        "therefore all variables within segments are used."),
      applicability_problem = TRUE,
      intrinsic_applicability_problem = TRUE) # TODO: PART_VAR or STUDY_SEGMENT
  }

  # Which segments?
  if (!(STUDY_SEGMENT %in% names(meta_data))) {
    util_error("Metadata do not contain the column STUDY_SEGMENT",
               applicability_problem = TRUE)
  }

  seg_names <- meta_data[
    util_empty(meta_data[[VARIABLE_ROLE]]) |
    meta_data[[VARIABLE_ROLE]] !=
      VARIABLE_ROLES$SUPPRESS, # omit segments added on-the-fly as dependencies
    STUDY_SEGMENT,
    drop = TRUE] # meta_data[[LONG_LABEL]][meta_data$VAR_NAMES %in% part_vars]

  if (!(PART_VAR %in% names(meta_data))) {
    util_warning("Metadata do not contain the column PART_VAR",
                 applicability_problem = TRUE)
    pv <- seg_names
    pv[!startsWith(pv, "PART_")] <-
      paste0("PART_", pv[!startsWith(pv, "PART_")])
    while (any(pv %in% c(meta_data[[VAR_NAMES]],
                         meta_data[[LABEL]],
                         meta_data[[label_col]])
    )) {
      pv[pv %in% c(meta_data[[VAR_NAMES]],
                   meta_data[[LABEL]],
                   meta_data[[label_col]])] <-
        paste0("_", pv[pv %in% c(meta_data[[VAR_NAMES]],
                            meta_data[[LABEL]],
                            meta_data[[label_col]])], "_")
    }
    meta_data[[PART_VAR]] <- pv
  }

  part_vars <- meta_data[[PART_VAR]]
  names(part_vars) <- seg_names

  keep <- !is.na(part_vars) & !is.na(seg_names)

  part_vars <- part_vars[keep]

  part_vars <- part_vars[!duplicated(part_vars)]

  sn <- seg_names[!is.na(seg_names)]
  sn <- setNames(nm = unique(sn))
  .sn <- sn

  # do not compute segment plots, if not explicitly requested, if only one
  # segment in data
  keep_in_segmiss <- character(0)
  if (all(c(STUDY_SEGMENT, SEGMENT_MISS) %in% colnames(meta_data_segment))) {
    nas <- util_empty(meta_data_segment[[SEGMENT_MISS]])
    not_in_output <-
      util_is_na_0_empty_or_false(meta_data_segment[[SEGMENT_MISS]]) &
      !nas
    must_in_output <-
      !util_is_na_0_empty_or_false(meta_data_segment[[SEGMENT_MISS]])
    auto <- meta_data_segment[nas, STUDY_SEGMENT, drop = TRUE]
    remove <- meta_data_segment[not_in_output, STUDY_SEGMENT, drop = TRUE]
    keep_in_segmiss <- meta_data_segment[must_in_output,
                                         STUDY_SEGMENT, drop = TRUE]
    sn <- union(keep_in_segmiss, setdiff(sn, remove))
  }

  sn <- intersect(.sn, sn)

  sn <- sn[!is.na(sn)]
  sn <- setNames(nm = unique(sn))

  if (length(sn) < 2 && length(keep_in_segmiss) == 0) {
    util_error("No segment missingness plot for less than two segments",
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }

  # determine which vars per segment
  var_sets <- lapply(sn, util_get_vars_in_segment,
               meta_data = meta_data,
               label_col = label_col)

  # remove all part_vars
  if (TRUE) {
    remove_part_vars <- function(vars, meta_data, label_col) {
      v <- util_map_labels(vars, meta_data, to = VAR_NAMES, from = label_col)
      vars[!(v %in% part_vars)]
    }
    var_sets <- lapply( var_sets, remove_part_vars,
                        meta_data = meta_data,
                        label_col = label_col)
  }

  # remove variables excluded by roles
  if (!isFALSE(exclude_roles)) {
    remove_roles <- function(vars, meta_data, label_col) {
      roles <- util_map_labels(vars,
                               meta_data = meta_data,
                               to = VARIABLE_ROLE,
                               from = label_col)
      vars[!(roles %in% exclude_roles)]
    }
    var_sets <- lapply( var_sets, remove_roles,
                        meta_data = meta_data,
                        label_col = label_col)
  }

  # Which groups?
  if (length(group_vars) > 0) {
    is_grouped <- TRUE
    # No. of group levels and labels
    ds1[[group_vars]] <- util_assign_levlabs(
      variable = ds1[[group_vars]],
      string_of_levlabs = meta_data[[VALUE_LABELS]][meta_data[[label_col]] ==
                                                   group_vars],
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
    is_grouped <- FALSE
    gr <- 1
    group_vars <- "Group"
    ds1$Group <- 1
    cvs <- "Examinations"
  }

  if (length(strata_vars) > 0) {
    # No. of strata levels and labels
    if (dim(ds1)[1] != dim(ds1[!is.na(ds1[[strata_vars]]), ])[1]) {
      ds1 <- ds1[!is.na(ds1[[strata_vars]]), ]
      util_message(paste0("Some observations in ", strata_vars,
                          " are NA and were removed."),
                   applicability_problem = FALSE)
    }

    ds1[[strata_vars]] <- util_assign_levlabs(
      variable = ds1[[strata_vars]],
      string_of_levlabs = meta_data[[VALUE_LABELS]][meta_data[[label_col]] ==
                                                   strata_vars],
      splitchar = SPLIT_CHAR,
      assignchar = "="
    )
    strata <- unique(ds1[[strata_vars]])[!is.na(unique(ds1[[strata_vars]]))]
    # covariables for plot
    cvs <- c(strata_vars, group_vars, "Examinations")
  }

  # create result dataframe by factor combinations
  if (length(strata_vars) == 0) {
    res_df <- expand.grid(Group = gr, Examinations = names(var_sets))
    colnames(res_df) <- c(group_vars, "Examinations")
  } else {
    res_df <- expand.grid(
      Strata = strata, Group = gr, Examinations = names(var_sets),
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

  if (length(strata_vars) == 0) {
    for (j in seq_along(var_sets)) {
      for (i in seq_along(gr)) {
        one_var_from_seg_w_r_o_g <- var_sets[[j]][[1]] # empty var set is impossible, since segments are read from item-level-metadata here. so, the first one could represent for expected observations the whole segment.
        check_df <- ds1[
          util_observation_expected(rv = one_var_from_seg_w_r_o_g,
                                    study_data = ds1,
                                    meta_data = meta_data,
                                    label_col = label_col,
                                    expected_observations =
                                      expected_observations) &
            ds1[[group_vars]] == gr[i],
          c(as.character(unlist(var_sets[j]))),
          drop = FALSE]
        Ns <- c(Ns, nrow(check_df))
        Ms <- c(Ms, sum(apply(check_df, 1, myfun)))
      }
    }
  } else {
    for (i in seq_along(strata)) {
      for (j in seq_along(gr)) {
        for (k in seq_along(var_sets)) {
          one_var_from_seg_w_r_o_g <- var_sets[[k]][[1]] # empty var set is impossible, since segments are read from item-level-metadata here. so, the first one could represent for expected observations the whole segment.
          check_df <-
            ds1[util_observation_expected(rv = one_var_from_seg_w_r_o_g,
                                      study_data = ds1,
                                      meta_data = meta_data,
                                      label_col = label_col,
                                      expected_observations =
                                        expected_observations) &
            ds1[[strata_vars]] == strata[i] & ds1[[group_vars]] == gr[j],
          c(as.character(unlist(var_sets[k])))
          ]
          Ns <- c(Ns, nrow(check_df))
          Ms <- c(Ms, sum(apply(check_df, 1, myfun)))
        }
      }
    }
  }

  res_df$"No. of Participants" <- Ns # TODO: Write an indicator for segments expected to be empty according to some intro variable
  res_df$"No. of missing segments" <- Ms
  res_df$"(%) of missing segments" <- round(res_df$`No. of missing segments` /
                                              res_df$`No. of Participants` *
                                              100, digits = 2)
  res_df$"(%) of missing segments" <-
    as.numeric(res_df$"(%) of missing segments")
  res_df$threshold <- threshold_value
  res_df$direction <- color_gradient_direction

  res_df$"(%) of missing segments"[
    !is.finite(res_df$"(%) of missing segments")] <- NA_real_ # to avoid Inf %

  if (color_gradient_direction == "above") {
    res_df$GRADING <- ifelse(res_df$"(%) of missing segments" >
                               threshold_value, 1, 0)
  } else {
    res_df$GRADING <- ifelse(res_df$"(%) of missing segments" <
                               threshold_value, 1, 0)
  }

  # plot
  inversion <- ifelse(color_gradient_direction == "below", 1, 0)

  # order result data frame by grouping variable
  if (length(strata_vars) == 0) {
    # order result data frame by grouping variable
    res_df <- res_df[order(res_df[[group_vars]]), ]
    if (!is_grouped) {
      repsumtab <- res_df[, c("Examinations",
                              "No. of missing segments",
                              "No. of Participants")]
      colnames(repsumtab)[c(1,3)] <- c("Variables", "N")
      attr(repsumtab, "higher_means") <- ifelse(
        color_gradient_direction == "above",
        "worse",
        "")
      class(repsumtab) <- union("ReportSummaryTable", class(repsumtab))
      p <- print(repsumtab, view = FALSE)
    } else { # TODO: Implemen t something useful in print.ReportSummaryTable
      p <- util_heatmap_1th(
       df = res_df, cat_vars = cvs, values = "(%) of missing segments",
       right_intv = TRUE, threshold = threshold_value,
       invert = inversion
      )$SummaryPlot
      repsumtab <- NULL
    }
  } else { # TODO: Implemen t something useful in print.ReportSummaryTable
    # order result data frame by grouping variable
    res_df <- res_df[order(res_df[[strata_vars]], res_df[[group_vars]]), ]
    # repsumtab <- res_df[, c("Examinations",
    #                         "No. of missing segments",
    #                         "No. of Participants")]
    # colnames(repsumtab)[c(1,3)] <- c("Variables", "N")
    # attr(repsumtab, "higher_means") <- ifelse(
    #   color_gradient_direction == "above",
    #   "worse",
    #   "")
    # class(repsumtab) <- union("ReportSummaryTable", class(repsumtab))
    # p <- print(repsumtab, view = FALSE))
    p <- util_heatmap_1th( # TODO: Implemen t something useful in print.ReportSummaryTable
      df = res_df, cat_vars = cvs[-1], values = "(%) of missing segments",
      right_intv = TRUE, threshold = threshold_value,
      invert = inversion, strata = strata_vars
    )$SummaryPlot
    repsumtab <- NULL
  }
#
#   suppressWarnings({
#     # suppress wrong warnings: https://github.com/tidyverse/ggplot2/pull/4439/commits
#     # find out size of the plot https://stackoverflow.com/a/51795017
#     bp <- ggplot_build(p)
#     w <- 2 * length(bp$layout$panel_params[[1]]$x$get_labels())
#     if (w == 0) {
#       w <- 10
#     }
#     w <- w + 2 +
#       max(nchar(bp$layout$panel_params[[1]]$y$get_labels()),
#           na.rm = TRUE)
#     h <- 2 * length(bp$layout$panel_params[[1]]$y$get_labels())
#     if (h == 0) {
#       h <- 10
#     }
#     h <- h + 15
#
#     p <- util_set_size(p, width_em = w, height_em = h)
#   })

  return(list(SummaryData = res_df,
              ReportSummaryTable = repsumtab,
              SummaryPlot = p))
}
