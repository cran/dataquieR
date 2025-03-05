# RECORD_MISSMATCH_CHECKTYPE

dataquieR.ELEMENT_MISSMATCH_CHECKTYPE_default <- "subset_u"

#' @name dataquieR.ELEMENT_MISSMATCH_CHECKTYPE
#' @title Metadata describes more than the current study data
#' @description
#' - `none`: no check will be provided about the match of variables and
#'          records available in the study data and described in the metadata
#' - `exact`: There must be a 1:1 match between the study data and metadata
#'            regarding data frames and segments variables and records
#' - `subset_u`: study data are a subset of metadata. All variables from the study
#'               data are expected to be present in the metadata, but one or
#'               more variables in the metadata are not expected to be
#'               present in the study data.
#'               In this case a variable present in
#'               the study data but not in the metadata would produce an issue.
#' - `subset_m`: metadata are a subset of study data. All variables in the metadata
#'               are expected to be present in the study data, but one or more
#'               variables in the study data are not expected to be
#'               present in the metadata.
#' @family options
#' @docType data
options("dataquieR.ELEMENT_MISSMATCH_CHECKTYPE" =
          dataquieR.ELEMENT_MISSMATCH_CHECKTYPE_default)

dataquieR.scale_level_heuristics_control_metriclevels_default <- 25
dataquieR.scale_level_heuristics_control_binaryrecodelimit_default <- 8

#' @name dataquieR.scale_level_heuristics_control_metriclevels
#' @title Number of levels to consider a variable metric in absence of [SCALE_LEVEL]
#' @description
#' TODO
#' @family options
#' @docType data
options("dataquieR.scale_level_heuristics_control_metriclevels" =
          dataquieR.scale_level_heuristics_control_metriclevels_default)

#' @name dataquieR.scale_level_heuristics_control_binaryrecodelimit
#' @title Number of levels to consider a variable ordinal in absence of [SCALE_LEVEL]
#' @description
#' TODO
#' @family options
#' @docType data
options("dataquieR.scale_level_heuristics_control_binaryrecodelimit" =
          dataquieR.scale_level_heuristics_control_binaryrecodelimit_default)

# TODO: Deprecate this block? until including dataquieR.CONDITIONS_WITH_STACKTRACE
dataquieR.ERRORS_WITH_CALLER_default <- TRUE
#' @name dataquieR.ERRORS_WITH_CALLER
#' @title Set caller for error conditions (to be deprecated)
#' @description
#' TODO
#' @family options
#' @docType data
options("dataquieR.ERRORS_WITH_CALLER" =
          dataquieR.ERRORS_WITH_CALLER_default)
dataquieR.WARNINGS_WITH_CALLER_default <- TRUE
#' @name dataquieR.WARNINGS_WITH_CALLER
#' @title Set caller for warning conditions (to be deprecated)
#' @description
#' TODO
#' @family options
#' @docType data
options("dataquieR.WARNINGS_WITH_CALLER" =
          dataquieR.WARNINGS_WITH_CALLER_default)
dataquieR.MESSAGES_WITH_CALLER_default <- FALSE
#' @name dataquieR.MESSAGES_WITH_CALLER
#' @title Set caller for message conditions (to be deprecated)
#' @description
#' TODO
#' @family options
#' @docType data
options("dataquieR.MESSAGES_WITH_CALLER" =
          dataquieR.MESSAGES_WITH_CALLER_default)

#' @name dataquieR.CONDITIONS_WITH_STACKTRACE
#' @title Add stack-trace in condition messages (to be deprecated)
#' @description
#' TODO
#' @family options
#' @docType data
options("dataquieR.CONDITIONS_WITH_STACKTRACE" = FALSE)

dataquieR.CONDITIONS_LEVEL_TRHESHOLD_default <- 0
#' @name dataquieR.CONDITIONS_LEVEL_TRHESHOLD
#' @title Log Level
#' @description
#' TODO
#' @family options
#' @docType data
options("dataquieR.CONDITIONS_LEVEL_TRHESHOLD" =
          dataquieR.CONDITIONS_LEVEL_TRHESHOLD_default)


#' @name dataquieR.debug
#' @title Call [browser()] on errors
#' @description
#' TODO
#' @family options
#' @docType data
options("dataquieR.debug" = FALSE)

dataquieR.flip_mode_default <- "default" # flip, noflip, auto
#' @name dataquieR.flip_mode
#' @title Flip-Mode to Use for figures
#' @description
#' TODO
#' @family options
#' @docType data
options("dataquieR.flip_mode" = dataquieR.flip_mode_default)

dataquieR.force_item_specific_missing_codes_default <- FALSE
#' @name dataquieR.force_item_specific_missing_codes
#' @title Converting [MISSING_LIST]/[JUMP_LIST] to a [MISSING_LIST_TABLE] create on list per item
#' @description
#' TODO
#' @family options
#' @docType data
options("dataquieR.force_item_specific_missing_codes" =
          dataquieR.force_item_specific_missing_codes_default)

dataquieR.progress_fkt <- NULL
#' @name dataquieR.progress_fkt
#' @title [function] to call on progress increase
#' @description
#' TODO
#' @family options
#' @docType data
options("dataquieR.progress_fkt" =
          dataquieR.progress_fkt)

dataquieR.progress_msg_fkt <- NULL
#' @name dataquieR.progress_msg_fkt
#' @title [function] to call on progress message update
#' @description
#' TODO
#' @family options
#' @docType data
options("dataquieR.progress_msg_fkt" =
          dataquieR.progress_msg_fkt)

dataquieR.grading_rulesets_default <- "grading_rulesets"
#' @name dataquieR.grading_rulesets
#' @title Name of the [data.frame] featuring [GRADING_RULESET]
#' @description
#' TODO
#' @family options
#' @docType data
options("dataquieR.grading_rulesets" =
          dataquieR.grading_rulesets_default)

dataquieR.grading_formats_default <- "grading_formats"
#' @name dataquieR.grading_formats
#' @title Name of the [data.frame] featuring a format for grading-values
#' @description
#' TODO
#' @family options
#' @docType data
options("dataquieR.grading_formats" =
          dataquieR.grading_formats_default)

dataquieR.VALUE_LABELS_htmlescaped_default <- FALSE
#' @name dataquieR.VALUE_LABELS_htmlescaped
#' @title Assume, all [VALUE_LABELS] are [HTML escaped](https://www.w3.org/International/questions/qa-escapes)
#' @description
#' TODO
#' @family options
#' @docType data
options(dataquieR.VALUE_LABELS_htmlescaped =
          dataquieR.VALUE_LABELS_htmlescaped_default)

dataquieR.acc_loess.mark_time_points_default <- FALSE
#' @name dataquieR.acc_loess.mark_time_points
#' @title Display time-points in LOESS plots
#' @description
#' TODO
#' @family options
#' @docType data
options("dataquieR.acc_loess.mark_time_points" =
          dataquieR.acc_loess.mark_time_points_default)

dataquieR.acc_loess.plot_observations_default <- FALSE
#' @name dataquieR.acc_loess.plot_observations
#' @title Display observations in LOESS plots
#' @description
#' TODO
#' @family options
#' @docType data
options("dataquieR.acc_loess.plot_observations" =
          dataquieR.acc_loess.plot_observations_default)

dataquieR.acc_loess.plot_format_default <- "COMBINED"
#' @name dataquieR.acc_loess.plot_format
#' @title default for Plot-Format in [acc_loess()]
#' @description
#' TODO
#' @family options
#' @docType data
options("dataquieR.acc_loess.plot_format" =
          dataquieR.acc_loess.plot_format_default)

dataquieR.acc_multivariate_outlier.scale_default <- TRUE
#' @name dataquieR.acc_multivariate_outlier.scale
#' @title Apply min-max scaling in parallel coordinates figure to inspect multivariate outliers
#' @description
#' boolean, TRUE or FALSE
#' @family options
#' @docType data
options("dataquieR.acc_multivariate_outlier.scale" =
          dataquieR.acc_multivariate_outlier.scale_default)

dataquieR.non_disclosure_default <- FALSE
#' @name dataquieR.non_disclosure
#' @title Remove all observation-level-real-data from reports
#' @description
#' TODO
#' @family options
#' @docType data
options("dataquieR.non_disclosure" =
          dataquieR.non_disclosure_default)

dataquieR.lang_default <- ""
#' @name dataquieR.lang
#' @title Language-Suffix for metadata Label-Columns
#' @description
#' TODO
#' @family options
#' @docType data
options("dataquieR.lang" =
          dataquieR.lang_default)

dataquieR.force_label_col_default = "auto"
#' @name dataquieR.force_label_col
#' @title Control, how the `label_col` argument is used.
#' @description
#' TODO
#' @family options
#' @docType data
options("dataquieR.force_label_col" =
          dataquieR.force_label_col_default)

#' @name dataquieR.testdebug
#' @title Disable all interactively used metadata-based function argument provision
#' @description
#' TODO
#' @family options
#' @docType data
options("dataquieR.testdebug" = FALSE)

#' @name dataquieR.dontwrapresults
#' @title Disable automatic post-processing of `dataquieR` function results
#' @description
#' TODO
#' @family options
#' @docType data
options("dataquieR.dontwrapresults" = FALSE)

dataquieR.guess_missing_codes_default <- FALSE
#' @name dataquieR.guess_missing_codes
#' @title Control, if `dataquieR` tries to guess missing-codes from the study data in absence of metadata
#' @description
#' TODO
#' @family options
#' @docType data
options("dataquieR.guess_missing_codes" =
          dataquieR.guess_missing_codes_default)

dataquieR.col_con_con_logical_default <- "#B35451"
#' @name dataquieR.col_con_con_logical
#' @title Color for logical contradictions
#' @description
#' TODO
#' @family options
#' @docType data
options("dataquieR.col_con_con_logical" =
          dataquieR.col_con_con_logical_default)

dataquieR.col_con_con_empirical_default <- "#B39651"
#' @name dataquieR.col_con_con_empirical
#' @title Color for empirical contradictions
#' @description
#' TODO
#' @family options
#' @docType data
options("dataquieR.col_con_con_empirical" =
          dataquieR.col_con_con_empirical_default)

dataquieR.fix_column_type_on_read_default <- FALSE
#' @name dataquieR.fix_column_type_on_read
#' @title Try to avoid fallback to string columns when reading files
#' @description
#' If a file does not feature column data types ore features data types
#' cell-based, choose that type which matches the majority of the
#' sampled cells of a column for the column's data type.
#' @details
#' This may make you miss data type problems but it could fix them, so
#' [prep_get_data_frame()] works better.
#' @family options
#' @docType data
options("dataquieR.fix_column_type_on_read" =
          dataquieR.fix_column_type_on_read_default)

dataquieR.MAX_VALUE_LABEL_LEN_default <- 60
#' @name dataquieR.MAX_VALUE_LABEL_LEN
#' @title Maximum length for value labels
#' @description
#' value labels are restricted to this length
#' @family options
#' @docType data
options("dataquieR.MAX_VALUE_LABEL_LEN" =
          dataquieR.MAX_VALUE_LABEL_LEN_default)

# Maximum length for labels according to
# file names https://stackoverflow.com/a/265782/4242747
# and label handling in plot.ly
.MAX_LABEL_LEN <- 200L

dataquieR.MAX_LABEL_LEN_default <- 30
#' @name dataquieR.MAX_LABEL_LEN
#' @title Maximum length for variable labels
#' @description
#' All variable labels will be shortened to fit this maximum length.
#' Cannot be larger than 200 for technical reasons.
#' @family options
#' @docType data
options("dataquieR.MAX_LABEL_LEN" =
          dataquieR.MAX_LABEL_LEN_default)

dataquieR.max_group_var_levels_in_plot_default <- 20
#' @name dataquieR.max_group_var_levels_in_plot
#' @title Maximum number of levels of the grouping variable shown
#' individually in figures
#' @description
#' If there are more examiners or devices than can be shown individually,
#' they will be collapsed into "other".
#' @family options
#' @docType data
options("dataquieR.max_group_var_levels_in_plot" =
          dataquieR.max_group_var_levels_in_plot_default)

dataquieR.min_obs_per_group_var_in_plot_default <- 10
#' @name dataquieR.min_obs_per_group_var_in_plot
#' @title Minimum number of observations per grouping variable that is required
#' to include an individual level of the grouping variable in a figure
#' @description
#' Levels of the grouping variable with fewer observations than specified here
#' will be excluded from the figure.
#' @family options
#' @docType data
options("dataquieR.min_obs_per_group_var_in_plot" =
          dataquieR.min_obs_per_group_var_in_plot_default)

dataquieR.max_group_var_levels_with_violins_default <- 10
#' @name dataquieR.max_group_var_levels_with_violins
#' @title Maximum number of levels of the grouping variable shown
#' with individual histograms ('violins') in 'margins' figures
#' @description
#' If there are more examiners or devices, the figure will be reduced to
#' box-plots to save space.
#' @family options
#' @docType data
options("dataquieR.max_group_var_levels_with_violins" =
          dataquieR.max_group_var_levels_with_violins_default)

dataquieR.MULTIVARIATE_OUTLIER_CHECK_default <- "auto"
#' @name dataquieR.MULTIVARIATE_OUTLIER_CHECK
#' @title Default availability of multivariate outlier checks in reports
#' @description
#' can be
#'   - `TRUE`: for `cross-item_level`-groups with `MULTIVARIATE_OUTLIER_CHECK`
#'             empty, do a multivariate outlier check
#'   - `FALSE`: for `cross-item_level`-groups with `MULTIVARIATE_OUTLIER_CHECK`
#'             empty, don't do a multivariate outlier check
#'   - `"auto"`: for `cross-item_level`-groups with `MULTIVARIATE_OUTLIER_CHECK`
#'             empty, do multivariate outlier checks, if there is no entry in
#'             the column [CONTRADICTION_TERM].
#' @family options
#' @docType data
options("dataquieR.MULTIVARIATE_OUTLIER_CHECK" =
          dataquieR.MULTIVARIATE_OUTLIER_CHECK_default)

dataquieR.acc_margins_sort_default <- TRUE
#' @name dataquieR.acc_margins_sort
#' @title Sort levels of the grouping variable in the 'margins' figures
#' @description
#' If this option is set to `TRUE`, the levels of the grouping variable in the
#' figure are sorted in descending order according to the number of
#' observations so that levels with more observations are easier to identify.
#' Otherwise, the original order of the levels is retained.
#' @family options
#' @docType data
options("dataquieR.acc_margins_sort" =
          dataquieR.acc_margins_sort_default)

dataquieR.acc_margins_num_default <- TRUE
#' @name dataquieR.acc_margins_num
#' @title Include number of observations for each level of the grouping variable
#' in the 'margins' figure
#' @description
#' If this option is set to `FALSE`, the figures created by `acc_margins` will
#' not include the number of observations for each level of the grouping
#' variable. This can be used to obtain clean static plots.
#' @family options
#' @docType data
options("dataquieR.acc_margins_num" =
          dataquieR.acc_margins_num_default)

dataquieR.des_summary_hard_lim_remove_default <- FALSE
#' @name dataquieR.des_summary_hard_lim_remove
#' @title Removal of hard limits from data before calculating
#'        descriptive statistics.
#' @description
#' can be
#'   - `TRUE`: values outside hard limits will be removed from the data
#'             before calculating descriptive statistics
#'   - `FALSE`: values outside hard limits will not be removed from the original
#'              data
#' @family options
#' @docType data
options("dataquieR.des_summary_hard_lim_remove" =
            dataquieR.des_summary_hard_lim_remove_default)

dataquieR.GAM_for_LOESS.default <- FALSE
#' @name dataquieR.GAM_for_LOESS
#' @title Enable to switch to a general additive model instead of LOESS
#' @description
#' If this option is set to `TRUE`, time course plots will use general additive
#' models (GAM) instead of LOESS when the number of observations exceeds a
#' specified threshold. LOESS computations for large datasets have a high
#' memory consumption.
#' @family options
#' @docType data
options("dataquieR.GAM_for_LOESS" =
          dataquieR.GAM_for_LOESS.default)

dataquieR.acc_loess.exclude_constant_subgroups.default <- FALSE
#' @name dataquieR.acc_loess.exclude_constant_subgroups
#' @title Exclude subgroups with constant values from LOESS figure
#' @description
#' If this option is set to `TRUE`, time course plots will only show subgroups
#' with more than one distinct value. This might improve the readability of
#' the figure.
#' @family options
#' @docType data
options("dataquieR.acc_loess.exclude_constant_subgroups" =
          dataquieR.acc_loess.exclude_constant_subgroups.default)

dataquieR.acc_loess.min_bw.default <- 0.2
#' @name dataquieR.acc_loess.min_bw
#' @title Lower limit for the LOESS bandwidth
#' @description
#' The value should be greater than 0 and less than or equal to 1. In general,
#' increasing the bandwidth leads to a smoother trend line.
#' @family options
#' @docType data
options("dataquieR.acc_loess.min_bw" =
          dataquieR.acc_loess.min_bw.default)

dataquieR.acc_loess.min_proportion.default <- 0.1
#' @name dataquieR.acc_loess.min_proportion
#' @title Lower limit for the proportion of cases or controls to create a
#' smoothed time trend figure
#' @description
#' The value should be greater than 0 and less than 0.4. If the proportion of
#' cases or controls is lower than the specified value, the LOESS figure will
#' not be created for the specified binary outcome.
#' @family options
#' @docType data
options("dataquieR.acc_loess.min_proportion" =
          dataquieR.acc_loess.min_proportion.default)
