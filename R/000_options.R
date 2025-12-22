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
NULL

dataquieR.scale_level_heuristics_control_metriclevels_default <- 25
dataquieR.scale_level_heuristics_control_binaryrecodelimit_default <- 8

#' @name dataquieR.scale_level_heuristics_control_metriclevels
#' @title Number of levels to consider a variable metric in absence
#'        of [SCALE_LEVEL]
#' @description
#' If `SCALE_LEVEL` is not specified in the `meta_data`, it will be inferred
#' using a heuristic. This option defines, for numeric variables, the maximum
#' number of distinct data values for a variable to be considered categorical,
#' not ordinal.
#' @family options
#' @docType data
NULL

#' @name dataquieR.scale_level_heuristics_control_binaryrecodelimit
#' @title Number of levels to consider a variable ordinal in absence
#'        of [SCALE_LEVEL]
#' @description
#' If `SCALE_LEVEL` is not specified in the `meta_data`, it will be inferred
#' using a heuristic. This option defines, for numeric variables, the maximum
#' number of distinct data values for a variable to be considered ordinal.
#' @family options
#' @docType data
NULL

# TODO: Deprecate this block? until including dataquieR.CONDITIONS_WITH_STACKTRACE
dataquieR.ERRORS_WITH_CALLER_default <- TRUE
#' @name dataquieR.ERRORS_WITH_CALLER
#' @title Set caller for error conditions (to be deprecated)
#' @description
#' to be deprecated
#' @family options
#' @docType data
NULL

dataquieR.WARNINGS_WITH_CALLER_default <- TRUE
#' @name dataquieR.WARNINGS_WITH_CALLER
#' @title Set caller for warning conditions (to be deprecated)
#' @description
#' to be deprecated
#' @family options
#' @docType data
NULL

dataquieR.MESSAGES_WITH_CALLER_default <- FALSE
#' @name dataquieR.MESSAGES_WITH_CALLER
#' @title Set caller for message conditions (to be deprecated)
#' @description
#' to be deprecated
#' @family options
#' @docType data
NULL

dataquieR.CONDITIONS_WITH_STACKTRACE_default <- FALSE
#' @name dataquieR.CONDITIONS_WITH_STACKTRACE
#' @title Add stack-trace in condition messages (to be deprecated)
#' @description
#' to be deprecated
#' @family options
#' @docType data
NULL

dataquieR.CONDITIONS_LEVEL_TRHESHOLD_default <- 0
#' @name dataquieR.CONDITIONS_LEVEL_TRHESHOLD
#' @title Log Level
#' @description
#' TODO
#' @family options
#' @docType data
NULL

dataquieR.traceback_default <- FALSE
#' @name dataquieR.traceback
#' @title Include full trace-back in captured conditions
#' @description
#' Caveat: Needs really much memory
#' @family options
#' @docType data
NULL

dataquieR.debug_default <- FALSE
#' @name dataquieR.debug
#' @title Call [browser()] on errors
#' @description
#' TODO
#' @family options
#' @docType data
NULL

dataquieR.flip_mode_default <- "default" # flip, noflip, auto
#' @name dataquieR.flip_mode
#' @title Flip-Mode to Use for figures
#' @description
#' TODO
#' @family options
#' @docType data
NULL

dataquieR.force_item_specific_missing_codes_default <- FALSE
#' @name dataquieR.force_item_specific_missing_codes
#' @title Converting [MISSING_LIST]/[JUMP_LIST] to a [MISSING_LIST_TABLE] create on list per item
#' @description
#' TODO
#' @family options
#' @docType data
NULL

dataquieR.progress_init_fkt_default <- NULL
#' @name progress_init_fkt
#' @title [function] to call on progress initialization
#' @description
#' has one argument, `n`, reporting the number of steps in the current
#' job. needed, e.g., by packages, such as `progressr`.
#' TODO
#' @family options
#' @docType data
NULL

dataquieR.progress_fkt_default <- NULL
#' @name dataquieR.progress_fkt_default
#' @title [function] to call on progress increase
#' @description
#' TODO
#' @family options
#' @docType data
NULL

dataquieR.progress_msg_fkt_default <- NULL
#' @name dataquieR.progress_msg_fkt_default
#' @title [function] to call on progress message update
#' @description
#' TODO
#' @family options
#' @docType data
NULL

dataquieR.grading_rulesets_default <- "grading_rulesets"
#' @name dataquieR.grading_rulesets
#' @title Name of the [data.frame] featuring [GRADING_RULESET]
#' @description
#' TODO
#' @family options
#' @docType data
NULL

dataquieR.grading_formats_default <- "grading_formats"
#' @name dataquieR.grading_formats
#' @title Name of the [data.frame] featuring a format for grading-values
#' @description
#' TODO
#' @family options
#' @docType data
NULL

dataquieR.VALUE_LABELS_htmlescaped_default <- FALSE
#' @name dataquieR.VALUE_LABELS_htmlescaped
#' @title Assume, all [VALUE_LABELS] are [HTML escaped](https://www.w3.org/International/questions/qa-escapes)
#' @description
#' TODO
#' @family options
#' @docType data
NULL

dataquieR.acc_loess.mark_time_points_default <- FALSE
#' @name dataquieR.acc_loess.mark_time_points
#' @title Display time-points in LOESS plots
#' @description
#' TODO
#' @family options
#' @docType data
NULL

dataquieR.acc_loess.plot_observations_default <- FALSE
#' @name dataquieR.acc_loess.plot_observations
#' @title Display observations in LOESS plots
#' @description
#' TODO
#' @family options
#' @docType data
NULL

dataquieR.acc_loess.plot_format_default <- "COMBINED"
#' @name dataquieR.acc_loess.plot_format
#' @title default for Plot-Format in [acc_loess()]
#' @description
#' TODO
#' @family options
#' @docType data
NULL

dataquieR.acc_multivariate_outlier.scale_default <- TRUE
#' @name dataquieR.acc_multivariate_outlier.scale
#' @title Apply min-max scaling in parallel coordinates figure to inspect multivariate outliers
#' @description
#' boolean, TRUE or FALSE
#' @family options
#' @docType data
NULL

dataquieR.non_disclosure_default <- FALSE
#' @name dataquieR.non_disclosure
#' @title Remove all observation-level-real-data from reports
#' @description
#' TODO
#' @family options
#' @docType data
NULL

dataquieR.lang_default <- ""
#' @name dataquieR.lang
#' @title Language-Suffix for metadata Label-Columns
#' @description
#' TODO
#' @family options
#' @docType data
NULL

dataquieR.force_label_col_default <- "auto"
#' @name dataquieR.force_label_col
#' @title Control, how the `label_col` argument is used.
#' @description
#' TODO
#' @family options
#' @docType data
NULL

dataquieR.testdebug_default <- FALSE
#' @name dataquieR.testdebug
#' @title Disable all interactively used metadata-based function argument provision
#' @description
#' TODO
#' @family options
#' @docType data
NULL

dataquieR.dontwrapresults_default <- FALSE
#' @name dataquieR.dontwrapresults
#' @title Disable automatic post-processing of `dataquieR` function results
#' @description
#' TODO
#' @family options
#' @docType data
NULL

dataquieR.guess_missing_codes_default <- FALSE
#' @name dataquieR.guess_missing_codes
#' @title Control, if `dataquieR` tries to guess missing-codes from the study data in absence of metadata
#' @description
#' TODO
#' @family options
#' @docType data
NULL

dataquieR.col_con_con_logical_default <- "#B35451"
#' @name dataquieR.col_con_con_logical
#' @title Color for logical contradictions
#' @description
#' TODO
#' @family options
#' @docType data
NULL

dataquieR.col_con_con_empirical_default <- "#B39651"
#' @name dataquieR.col_con_con_empirical
#' @title Color for empirical contradictions
#' @description
#' TODO
#' @family options
#' @docType data
NULL

dataquieR.fix_column_type_on_read_default <- FALSE
#' @name dataquieR.fix_column_type_on_read
#' @title Try to avoid fallback to string columns when reading files
#' @description
#' If a file does not feature column data types or features data types
#' cell-based, choose that type which matches the majority of the
#' sampled cells of a column for the column's data type.
#' @details
#' This may make you miss data type problems but it could fix them, so
#' [prep_get_data_frame()] works better.
#' @family options
#' @docType data
NULL

dataquieR.MAX_VALUE_LABEL_LEN_default <- 60
#' @name dataquieR.MAX_VALUE_LABEL_LEN
#' @title Maximum length for value labels
#' @description
#' value labels are restricted to this length
#' @family options
#' @docType data
NULL

# Maximum length for labels according to
# file names https://stackoverflow.com/a/265782/4242747
# and label handling in plot.ly
.MAX_LABEL_LEN <- 200L

dataquieR.MAX_LABEL_LEN_default <- 60
#' @name dataquieR.MAX_LABEL_LEN
#' @title Maximum length for variable labels [LABEL]
#' @description
#' All variable labels will be shortened to fit this maximum length.
#' Cannot be larger than 200 for technical reasons.
#' @family options
#' @docType data
NULL

dataquieR.MAX_LONG_LABEL_LEN_default <- 200
#' @name dataquieR.MAX_LONG_LABEL_LEN
#' @title Maximum length for long variable labels [LONG_LABEL]
#' @description
#' All long variable labels will be shortened to fit this maximum length.
#' Cannot be larger than 200 for technical reasons.
#' @family options
#' @docType data
NULL

dataquieR.max_group_var_levels_in_plot_default <- 20
#' @name dataquieR.max_group_var_levels_in_plot
#' @title Maximum number of levels of the grouping variable shown
#' individually in figures
#' @description
#' If there are more examiners or devices than can be shown individually,
#' they will be collapsed into "other".
#' @family options
#' @docType data
NULL

dataquieR.max_cat_resp_var_levels_in_plot_default <- 6
#' @name dataquieR.max_cat_resp_var_levels_in_plot
#' @title Maximum number of levels of the categorical response variable shown
#' individually in figures
#' @description
#' If there are more levels of a categorical response variable than can be
#' shown individually, they will be collapsed into "other".
#' @family options
#' @docType data
NULL

dataquieR.min_time_points_for_cat_resp_var_default <- 20
#' @name dataquieR.min_time_points_for_cat_resp_var
#' @title Minimum number of data points to create a time course plot for an
#' individual level of a categorical response variable
#' @description
#' If there are less observations for an individual level of a categorical
#' variable, it will not be shown in the time course plot.
#' @family options
#' @docType data
NULL

dataquieR.min_obs_per_group_var_in_plot_default <- 10
#' @name dataquieR.min_obs_per_group_var_in_plot
#' @title Minimum number of observations per grouping variable that is required
#' to include an individual level of the grouping variable in a figure
#' @description
#' Levels of the grouping variable with fewer observations than specified here
#' will be excluded from the figure.
#' @family options
#' @docType data
NULL

dataquieR.max_group_var_levels_with_violins_default <- 10
#' @name dataquieR.max_group_var_levels_with_violins
#' @title Maximum number of levels of the grouping variable shown
#' with individual histograms ('violins') in 'margins' figures
#' @description
#' If there are more examiners or devices, the figure will be reduced to
#' box-plots to save space.
#' @family options
#' @docType data
NULL

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
NULL

dataquieR.MAHALANOBIS_THRESHOLD_default <- 0.975
#' @name dataquieR.MAHALANOBIS_THRESHOLD
#' @title Default availability of Mahalanobis based multivariate outlier checks in reports
#' @description
#' a number, see corresponding argument in [acc_mahalanobis()]
#' @family options
#' @docType data
NULL

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
NULL

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
NULL

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
NULL

dataquieR.GAM_for_LOESS_default <- FALSE
#' @name dataquieR.GAM_for_LOESS
#' @title Enable to switch to a general additive model instead of LOESS
#' @description
#' If this option is set to `TRUE`, time course plots will use general additive
#' models (GAM) instead of LOESS when the number of observations exceeds a
#' specified threshold. LOESS computations for large datasets have a high
#' memory consumption.
#' @family options
#' @docType data
NULL

dataquieR.acc_loess.exclude_constant_subgroups_default <- FALSE
#' @name dataquieR.acc_loess.exclude_constant_subgroups
#' @title Exclude subgroups with constant values from LOESS figure
#' @description
#' If this option is set to `TRUE`, time course plots will only show subgroups
#' with more than one distinct value. This might improve the readability of
#' the figure.
#' @family options
#' @docType data
NULL

dataquieR.acc_loess.min_bw_default <- 0.2
#' @name dataquieR.acc_loess.min_bw
#' @title Lower limit for the LOESS bandwidth
#' @description
#' The value should be greater than 0 and less than or equal to 1. In general,
#' increasing the bandwidth leads to a smoother trend line.
#' @family options
#' @docType data
NULL

dataquieR.acc_loess.min_proportion_default <- 0.1
#' @name dataquieR.acc_loess.min_proportion
#' @title Lower limit for the proportion of cases or controls to create a
#' smoothed time trend figure
#' @description
#' The value should be greater than 0 and less than 0.4. If the proportion of
#' cases or controls is lower than the specified value, the LOESS figure will
#' not be created for the specified binary outcome.
#' @family options
#' @docType data
NULL

dataquieR.droplevels_ReportSummaryTable_default <- FALSE
#' @name dataquieR.droplevels_ReportSummaryTable
#' @title Show also unused levels in heatmaps
#' @description
#' if `TRUE`, levels not taken will not be displayed when printing/plotting
#' heatmap tables
#' @family options
#' @docType data
NULL

dataquieR.resume_checkpoint_default <- FALSE
#' @name dataquieR.resume_checkpoint
#' @title If result already exists in a `storr` back-end, re-use it
#' @description
#' if `TRUE`, computation won't be repeated, if a result already exist in the
#' output `storr`
#' @family options
#' @docType data
NULL

dataquieR.convert_to_list_for_lapply_default <- FALSE
#' @name dataquieR.convert_to_list_for_lapply
#' @title If report uses a `storr` back-end, do not convert to base-list
#' @description
#' if `TRUE` and a report uses a `storr`-back-end, convert it to a base list,
#' i.e., copy to the RAM, even if this would likely not be really needed for
#' apply-calls
#' @family options
#' @docType data
NULL

dataquieR.resume_print_default <- FALSE
#' @name dataquieR.resume_print
#' @title If output folder is not empty, try to resume stopped `print()`
#' @description
#' if `TRUE` and a report was already partially printed with also this option
#' `TRUE`, then, a second call to `print()` will resume the printing.
#' @family options
#' @docType data
NULL

dataquieR.study_data_cache_max_default <- Inf
#' @name dataquieR.study_data_cache_max
#' @title Maximum size of cache for curated study data
#' @description
#' `dataquieR` caches all used flavors of curated study data, e.g., having
#' missing codes replaced by `NA`s, having hard limits replaced by `NA`, ...
#' For larger sets of study data this can be very RAM consuming, so you can
#' control here the maximum size for this cache. Also, this cache is distributed
#' to all compute nodes in case of parallel computation, which may be very time-
#' consuming, and, on single-node-parallelization, also, it may be even more
#' RAM-consuming then.
#' @family options
#' @family study_data_cache
#' @docType data
NULL

dataquieR.precomputeStudyData_default <- FALSE
#' @name dataquieR.precomputeStudyData
#' @title Pre-compute different curation levels of study data
#' @description
#' as described in [dataquieR.study_data_cache_max], different flavors of
#' the study data are cached. With this option, you control, if before a report
#' is computed, a frequently needed bunch of such flavors are pre-computed and
#' distributed to the compute nodes. However, this may be time- and RAM-
#' consuming, so, you can turn the pre-computation off, which will still allow
#' the individual compute nodes to maintain such a cache but then growing on
#' demand on individual nodes separately, only. If
#' [dataquieR.study_data_cache_max] cannot handle all flavors, they may still be
#' pre-computed but immediately discarded.
#' @family options
#' @family study_data_cache
#' @docType data
NULL

dataquieR.study_data_cache_quick_fill_default <- TRUE
#' @name dataquieR.study_data_cache_quick_fill
#' @title Control the pre-computation of curation levels of study data
#' @description
#' as described in [`dataquieR.precomputeStudyData`], different flavors of
#' the study data are cached. With this option, you control, if before a report
#' is computed, only frequently needed bunch of such flavors are pre-computed,
#' or simply all possible flavors. Won't have any effect, if pre-computation
#' has been turned off.
#' @family options
#' @family study_data_cache
#' @docType data
NULL

dataquieR.study_data_cache_metrics_default <- FALSE
#' @name dataquieR.study_data_cache_metrics
#' @title Collect metrics on cache usage of study data cache
#' @description
#' if `TRUE`, collect metrics on the usage of the study data cache
#' described here: [dataquieR.study_data_cache_max]. Won't work, fully,
#' if running in parallel.
#' `r lifecycle::badge("experimental")`
#' @family options
#' @family study_data_cache
#' @docType data
NULL

#' @export
#' @title Default space for some metrics during report computation
#' @description
#' `r lifecycle::badge("experimental")`
#' @family study_data_cache
dataquieR.study_data_cache_metrics_env_default <- new.env(parent = emptyenv())

#' @name dataquieR.study_data_cache_metrics_env
#' @title [environment] for storing metrics on the study data cache
#' @description
#' this is the environment, where metrics will be stored, if
#' [dataquieR.study_data_cache_metrics]-`option()` has been set `TRUE`.
#' `r lifecycle::badge("experimental")`
#' @family options
#' @family study_data_cache
#' @docType data
NULL

dataquieR.ignore_empty_vars_default <- "auto"
#' @name dataquieR.ignore_empty_vars
#' @title [character] remove variables with only empty values
#' @description
#' remove variables with only empty values (`NA`, `".  "`,
#' `""` or similar) from reports. `auto` means, such variables are removed, if
#' we have more than 20% of the variables empty.
#' @family options
#' @family study_data_cache
#' @docType data
NULL # 20% are implemented at dq_report2, currently, search for ignore_empty_vars20 in this file.


if (rlang::is_installed("stringi")) {
  locale_string <- stringi::stri_locale_get()
} else {
  if (is.null(getOption("dataquieR.locale"))) {
    packageStartupMessage(
      paste("stringi is not installed to detect system default locale.",
            "Falling back to en_US. You can set the option dataquieR.locale to",
            "your desired ISO Code for the locale to avoid this check."))
  }
  locale_string <- "en_US"
}
language_code <- sub("_.*", "", locale_string)

dataquieR.locale_default <- language_code
#' @name dataquieR.locale
#' @title [character] default language for type conversion
#' @description
#' the language to use for type conversions (`en`, `de`, `fr`, `cn`, `ca`, ...)
#' only used by `util_adjust_data_type2()`, currently
#' @family options
#' @docType data
NULL # TODO: Overlaps in parts with dataquieR.lang

dataquieR.type_adjust_parallel_default <- "TRUE"
#' @name dataquieR.type_adjust_parallel
#' @title [character] try to do type adjustments in parallel
#' only, if [dq_report2()] was called with `cores = 2` or higher.
#' @family options
#' @docType data
NULL

dataquieR.old_type_adjust_default <- "FALSE"
#' @name dataquieR.old_type_adjust
#' @title [character] use the old type conversion code (slower)
#' @family options
#' @docType data
NULL
# For a benchmark, set the option to "TRUE", then run
# microbenchmark::microbenchmark(
#   util_adjust_data_type(s, meta_data = m),
#   util_adjust_data_type2(s, meta_data = m)
# )

dataquieR.old_factor_handling_default <- "FALSE"
#' @name dataquieR.old_factor_handling
#' @title [character] use the old handling of study data already featuring
#'                    factors
#' @description
#' if `study_data` comes as a data frame, it may already feature factors. if
#' a column has the `DATA_TYPE` `integer` in the meta data, the factor was
#' converted to integer using [as.integer()], which caused unexpected behavior.
#' if this option is set to `"FALSE"` (the new default), the conversion will now
#' try to apply `as.character(column_data)`, first.
#'
#' @family options
#' @docType data
NULL

dataquieR.print_block_load_factor_default <-
  Sys.getenv("DATAQUIER_PRINT_BLOCK_LOAD_FACTOR", unset = "1")
#' @name dataquieR.print_block_load_factor
#' @title [numeric]
#' @description
#' multiply size of parallel compute blocks
#' by this factor. the higher it is set,
#' the less smooth progress bar grows, but
#' setting it to a huge number can really
#' speed up the rendering process by
#' approx. 10%. Either set to 1 for full
#' progress control or large (e.g., 1000000)
#' for maximum speed.
#' @family options
#' @family study_data_cache
#' @docType data
NULL

dataquieR.guess_character_default <- FALSE
#' @name dataquieR.guess_character
#' @title For metadata guessing, try to guess [DATA_TYPE] from the data values
#' @description
#' By default, the [DATA_TYPE] is derived from the R data type of the study
#' data. However, when data are imported from plain text files, it can be more
#' appropriate to examine the actual values and infer the data type based on
#' their content. This option enables that behavior: set
#' `dataquieR.guess_character` to `TRUE` to infer data types from the observed
#' values rather than relying solely on the column’s `class` in the data frame.
#' @family options
#' @docType data
NULL

dataquieR.dt_adjust_default <- "TRUE"
#' @name dataquieR.dt_adjust
#' @title [character] Adjust data types according to metadata
#' @description
#' also reports inadmissible data types. can be turned off for performance
#' reasons, if the data source is already type-safe (e.g., a database)
#' ***use with care, may cause pipelines breaking (maybe only in the final
#' rendering step), if the data type is incorrectly set for some columns.***
#'
#' @family options
#' @docType data
NULL

dataquieR.study_data_colnames_case_sensitive_default <- "TRUE"
#' @name dataquieR.study_data_colnames_case_sensitive
#' @title [character] Are column names in study data considered
#'                    case-sensitive for mapping
#' @description
#' if `TRUE`, `colnames(study_data)` replaced by the capitalization used in the
#' metadata using a case-insensitive matching, first.
#'
#' @family options
#' @docType data
NULL

dataquieR.lazy_plots_default <- "TRUE"
#' @name dataquieR.lazy_plots
#' @title [character] plots realized lazy
#' @description
#' if `TRUE`, plots are not realized until needed in side reports to save
#' memory.
#'
#' @family options
#' @docType data
NULL

dataquieR.lazy_plots_cache_default <- "FALSE"
#' @name dataquieR.lazy_plots_cache
#' @title [character] cache realizations
#' @description
#' if `TRUE`, realized plots are cached, may need more memory.
#'
#' @family options
#' @docType data
NULL

dataquieR.lazy_plots_gg_compatibility_default <- "TRUE"
#' @name dataquieR.lazy_plots_gg_compatibility
#' @title [character] be as compatible with `ggplot2` objects as possible
#' @description
#' if `TRUE`, plot promises are blessed in an `S7` class so they behave almost
#' like "real" `ggplot2` objects, so you normally do not need to call
#' [prep_realize_ggplot()] on them. However, this comes with a small memory
#' overhead, so, you can disable this.
#'
#' @family options
#' @docType data
NULL
