#' Check applicability of DQ functions on study data
#'
#' @description
#' Checks applicability of DQ functions based on study data and metadata
#' characteristics
#'
#' @details
#' This is a preparatory support function that compares study data with
#' associated metadata. A prerequisite of this function is that the no. of
#' columns in the study data complies with the no. of rows in the metadata.
#'
#' For each existing R-implementation, the function searches for necessary
#' static metadata and returns a heatmap like matrix indicating the
#' applicability of each data quality implementation.
#'
#' In addition, the data type defined in the metadata is compared with the
#' observed data type in the study data.
#'
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param item_level [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param split_segments [logical] return one matrix per study segment
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param max_vars_per_plot [integer] from=0. The maximum number of variables
#'                                            per single plot.
#' @param meta_data_segment [data.frame] -- optional: Segment level metadata
#' @param meta_data_dataframe [data.frame] -- optional: Data frame level
#'                                                                 metadata
#' @param meta_data [data.frame] old name for `item_level`
#' @param meta_data_v2 [character] path to workbook like metadata file, see
#'                                 [`prep_load_workbook_like_file`] for details.
#'                                 **ALL LOADED DATAFRAMES WILL BE PURGED**,
#'                                 using [`prep_purge_data_frame_cache`],
#'                                 if you specify `meta_data_v2`.
#' @param segment_level [data.frame] alias for `meta_data_segment`
#' @param dataframe_level [data.frame] alias for `meta_data_dataframe`
#'
#' @inheritParams acc_distributions
#'
#' @return a list with:
#'   - `SummaryTable`: data frame about the applicability of each indicator
#'                   function (each function in a column).
#'                   its [integer] values can be one of the following four
#'                   categories:
#'                               0. Non-matching datatype + Incomplete metadata,
#'                               1. Non-matching datatype + complete metadata,
#'                               2. Matching datatype + Incomplete metadata,
#'                               3. Matching datatype + complete metadata,
#'                               4. Not applicable according to data type
#'   - `ApplicabilityPlot`: [ggplot2::ggplot2] heatmap plot, graphical representation of
#'                       `SummaryTable`
#'   - `ApplicabilityPlotList`: [list] of plots per (maybe artificial) segment
#'   - `ReportSummaryTable`: data frame underlying `ApplicabilityPlot`
#' @export
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_manual facet_wrap
#'                     theme_minimal scale_x_discrete xlab guides
#'                     guide_legend theme element_text
pro_applicability_matrix <- function(study_data,
                                     item_level = "item_level",
                                     split_segments =
                                      FALSE,
                                     label_col,
                                     max_vars_per_plot = 20,
                                     meta_data_segment,
                                     meta_data_dataframe,
                                     flip_mode = "noflip",
                                     meta_data_v2,
                                     meta_data = item_level,
                                     segment_level,
                                     dataframe_level) { # TODO: add meta_data_cross_item
  util_maybe_load_meta_data_v2()
  util_ck_arg_aliases()
  if (!missing(meta_data_segment) && !is.null(meta_data_segment)) {
    meta_data_segment <-
      prep_check_meta_data_segment(meta_data_segment) # TODO: Which columns are mandatory
    # TODO: If an alternative argument dq_control has been set, this may point to either something composed (e.g., excel workbook or rdata file) featuring all other standard data frames as sheets, or to a sheet naming all other standard data frames
  } else {
    meta_data_segment <- NULL # as.list(formals((function(x){})))[[1]] # after overwriting x, x is not missing(x) any more
  }
  if (!missing(meta_data_dataframe) && !is.null(meta_data_dataframe)) {
    meta_data_dataframe <-
      prep_check_meta_data_dataframe(meta_data_dataframe) # TODO: Which columns are mandatory # TODO: study data may be merged here
  } else {
    meta_data_dataframe <- NULL
  }

  if (length(max_vars_per_plot) != 1 || !util_is_integer(max_vars_per_plot) ||
      is.na(max_vars_per_plot) || is.nan(max_vars_per_plot) ||
      max_vars_per_plot < 1) {
    util_error(c(
      "max_vars_per_plot must be one strictly positive non-complex integer",
      "value, may be Inf."
    ), applicability_problem = TRUE)
  }

  prep_prepare_dataframes(.replace_missings = FALSE)

  Variables <- names(ds1)
  # this matrix will be shown to users
  app_matrix <- data.frame(Variables = Variables)
  # this matrix can be used to trigger computations
  # tri_matrix <- data.frame(Variables = Variables)
  # defined for merge of strata (optional)
  by_y <- label_col

  # DATA_TYPE defined in metadata?
  if (!("DATA_TYPE" %in% names(meta_data))) {
    util_error(
      c("The attribute DATA_TYPE is not contained in the metadata but is",
        "required for this function."), applicability_problem = TRUE)
  }

  # variables with missing DATA_TYPE?
  if (any(is.na(meta_data$DATA_TYPE))) {
    whichnot <- as.character(meta_data[[label_col]][is.na(meta_data$DATA_TYPE)])
    util_error(paste0("The DATA_TYPE for variable(s) <<", whichnot,
                      ">> is not defined in the metadata."),
               applicability_problem = TRUE)
  }

  # check whether data types adhere to conventions
  if (!all(unique(meta_data[[DATA_TYPE]]) %in% DATA_TYPES)) {
    whichnot <- dplyr::setdiff(unique(meta_data$DATA_TYPE), DATA_TYPES)
    util_warning(paste0("The data type(s): <<", whichnot,
                        ">> is not eligible in the metadata concept."),
                 applicability_problem = TRUE)
    util_error("Please map data types to: %s.", paste0(dQuote(DATA_TYPES),
                                                       collapse = ", "),
               applicability_problem = TRUE)
  }

  # DATA TYPE CONSISTENCY ------------------------------------------------------
  # This step compares the datatype as represented in study data with expected
  # datatypes in metadata and creates a binary vector indicating whether data
  # type fits or not
  dt_appl <- as.numeric(util_compare_meta_with_study(sdf = ds1, mdf = meta_data,
                                                     label_col = label_col,
                                                     check_conversion_stable =
                                                       TRUE))
  # combine convertible w/ and convertible w/o issues
  dt_appl[!is.na(dt_appl) & dt_appl == 3] <- 2


  # HINT: For each added function that returns MoifiedStudyData,
  # check, if this should be used downstream the dq_report-pipeline. If not
  # it must be added to the default values of the dont_modify_study_data_by
  # argument of dq_report.

  # INTEGRITY ---------------------------------------------------------------
  # crude unit missingness always applicable
  app_matrix$"int_all_datastructure_dataframe" <-
    ifelse(is.null(meta_data_dataframe), 4, 3)
  app_matrix$"int_all_datastructure_dataframe" <-
    as.factor(app_matrix$"int_all_datastructure_dataframe")

  app_matrix$"int_all_datastructure_segment" <-
    ifelse(is.null(meta_data_segment), 4, 3)
  app_matrix$"int_all_datastructure_segment" <-
    as.factor(app_matrix$"int_all_datastructure_segment")

  app_matrix$"int_datatype_matrix" <- 3
  app_matrix$"int_datatype_matrix" <-
    as.factor(app_matrix$"int_datatype_matrix")

  # COMPLETENESS ---------------------------------------------------------------
  # crude unit missingness always applicable
  app_matrix$"com_unit_missingness" <- 3
  app_matrix$"com_unit_missingness" <-
    as.factor(app_matrix$"com_unit_missingness")
  app_matrix$"com_segment_missingness" <- util_app_sm(meta_data, dt_appl) # TODO: Rename all app functions to contain the full indicator name as for util_app_con_contradictions_redcap below
  app_matrix$"com_item_missingness" <- util_app_im(meta_data, dt_appl) # TODO: Add com_qualified_item_missingness and com_qualified_segment_missingness

  # CONSISTENCY ----------------------------------------------------------------
  app_matrix$"con_hard_limits" <- util_app_hl(meta_data, dt_appl)
  app_matrix$"con_soft_limits" <- util_app_sl(meta_data, dt_appl)
  app_matrix$"con_detection_limits" <- util_app_dl(meta_data, dt_appl)
  app_matrix$"con_inadmissible_categorical" <- util_app_iac(meta_data, dt_appl)
  app_matrix$"con_contradictions" <- util_app_cd(meta_data, dt_appl)
  app_matrix$"con_contradictions_redcap" <-
    util_app_con_contradictions_redcap(meta_data, dt_appl)

  # ACCURACY -------------------------------------------------------------------
  app_matrix$"acc_univariate_outlier" <- util_app_ol(meta_data, dt_appl)
  app_matrix$"acc_distributions" <- util_app_dc(meta_data, dt_appl)
  app_matrix$"acc_margins" <- util_app_mar(meta_data, dt_appl)
  app_matrix$"acc_varcomp" <- util_app_vc(meta_data, dt_appl)
  app_matrix$"acc_loess" <- util_app_loess(meta_data, dt_appl)
  app_matrix$"acc_shape_or_scale" <- util_app_sos(meta_data, dt_appl)
  app_matrix$"acc_end_digits" <- util_app_ed(meta_data, dt_appl)
  app_matrix$"acc_multivariate_outlier" <- util_app_mol(meta_data, dt_appl)

  # SHOULD STRATIFICATION OF SEGMENTS BE USED? ---------------------------------

  # is STUDY_SEGMENT in metadata and none of the entries has NA?
  strata_defined <- STUDY_SEGMENT %in% names(meta_data) &&
    !all(util_empty(meta_data[[STUDY_SEGMENT]]))

  if (!strata_defined && split_segments) {
    util_message(c(
      "Stratification for STUDY_SEGMENT is not possible due to missing",
      "metadata. Will split arbitrarily avoiding too large figures"
    ), applicability_problem = TRUE)
    nvars <- nrow(meta_data)
    meta_data$STUDY_SEGMENT <- paste0("Block #", ceiling(1:nvars /
                                                             max_vars_per_plot))
  } else if (strata_defined && split_segments) {
    if (any(is.na(meta_data$STUDY_SEGMENT))) { # HINT: This is now dead code
      util_message(c(
        "Some STUDY_SEGMENT are NA. Will assign those to an artificial",
        "segment %s"), dQuote("Other"),
        applicability_problem = TRUE
      )
      meta_data$STUDY_SEGMENT[is.na(meta_data$STUDY_SEGMENT)] <- "Other"
    }
    too_big_blocks <- table(meta_data$STUDY_SEGMENT) > max_vars_per_plot
    too_big_blocks <- names(too_big_blocks)[too_big_blocks]
    for (too_big_block in too_big_blocks) {
      util_message(
        "Will split segemnt %s arbitrarily avoiding too large figures",
        dQuote(too_big_block),
        applicability_problem = FALSE
      )
      nvars <- sum(meta_data$STUDY_SEGMENT == too_big_block, na.rm = TRUE)
      meta_data$STUDY_SEGMENT[
        meta_data$STUDY_SEGMENT == too_big_block] <-
        paste0(
          meta_data$STUDY_SEGMENT[meta_data$STUDY_SEGMENT ==
                                        too_big_block],
          "#",
          ceiling(1:nvars / max_vars_per_plot)
        )
    }
  }

  if (!(STUDY_SEGMENT %in% names(meta_data))) {
    meta_data[[STUDY_SEGMENT]] <- "Study"
  }

  # merge relation to segments to app_matrix
  app_matrix <- merge(app_matrix, meta_data[, intersect(c(VAR_NAMES, LABEL,
                                                          STUDY_SEGMENT,
                                                          label_col),
                                                        colnames(meta_data)),
                                            FALSE],
    by.x = "Variables",
    by.y = by_y
  )
  app_matrix <- app_matrix[, !(names(app_matrix) %in% c(VAR_NAMES, LABEL,
                                                        label_col))]

  # PREPARE PLOT ---------------------------------------------------------------
  # reorder according VARIABLE_ORDER (optional)
  if (VARIABLE_ORDER %in% colnames(meta_data)) {
    meta_data <- meta_data[order(meta_data$VARIABLE_ORDER), ]
    meta_data[[VARIABLE_ORDER]][is.na(meta_data[[VARIABLE_ORDER]])] <-
      max(meta_data[[VARIABLE_ORDER]], na.rm = TRUE)
    meta_data[[VARIABLE_ORDER]][is.na(meta_data[[VARIABLE_ORDER]])] <-
      1
    app_matrix <- app_matrix[na.omit(match(meta_data[[label_col]],
                                     app_matrix$Variables)), ]
  }

  # assign factor levels
  app_matrix$Variables <- factor(app_matrix$Variables, levels =
                                   app_matrix$Variables)


  # reshape wide to long
#  app_matrix_long <- melt(app_matrix, id.vars = c("Variables",
#                                                  STUDY_SEGMENT))
  app_matrix_long <-
    stats::reshape(data = app_matrix,
                   idvar = c("Variables", STUDY_SEGMENT),
                   varying = colnames(app_matrix)[2:(ncol(app_matrix)-1)],
                   v.names = "value",
                   times = colnames(app_matrix)[2:(ncol(app_matrix)-1)],
                   direction = "long")

  levs <- unique(app_matrix_long$time)
  #sort levels
  #levs <- sort(levs)
  levs_int <- levs[grep("int_", levs)]
  levs_com <- levs[grep("com_", levs)]
  levs_con <- levs[grep("con_", levs)]
  levs_acc <- levs[grep("acc_", levs)]

  #order the factor levels
  app_matrix_long$time <- factor(app_matrix_long$time,
                                  levels = c(levs_int,levs_com,
                                             levs_con, levs_acc))
  rownames(app_matrix_long) <- NULL
  colnames(app_matrix_long) <- c("VARIABLES", "STUDY_SEGMENT", "IMPLEMENTATION",
                                 "APP_SCORE")

  #Order rows by int, com, con, and acc
  app_matrix_long_int <- app_matrix_long[grep("int_",
                                             app_matrix_long$IMPLEMENTATION), ,
                                        drop = FALSE]
  app_matrix_long_int <-
    app_matrix_long_int[order(app_matrix_long_int$IMPLEMENTATION), , drop = FALSE]


  app_matrix_long_com <- app_matrix_long[grep("com_",
                                             app_matrix_long$IMPLEMENTATION), ,
                                        drop = FALSE]
  app_matrix_long_com <-
    app_matrix_long_com[order(app_matrix_long_com$IMPLEMENTATION), , drop = FALSE]

  app_matrix_long_con<- app_matrix_long[grep("con_",
                                             app_matrix_long$IMPLEMENTATION), ,
                                        drop = FALSE]
  app_matrix_long_con <-
    app_matrix_long_con[order(app_matrix_long_con$IMPLEMENTATION), , drop = FALSE]

  app_matrix_long_acc <- app_matrix_long[grep("acc_",
                                             app_matrix_long$IMPLEMENTATION), ,
                                        drop = FALSE]
  app_matrix_long_acc <-
    app_matrix_long_acc[order(app_matrix_long_acc$IMPLEMENTATION), , drop = FALSE]

  app_matrix_long<- rbind(app_matrix_long_int, app_matrix_long_com,
                          app_matrix_long_con, app_matrix_long_acc)

  lbs <- c(
    "Non-matching datatype + Incomplete metadata",
    "Non-matching datatype + complete metadata",
    "Matching datatype + Incomplete metadata",
    "Matching datatype + complete metadata",
    "Not applicable according to data type"
  )
  lvls <- c(
    0,
    1,
    2,
    3,
    4
  )
  # assign factor labels
  app_matrix_long$APP_SCORE <- factor(app_matrix_long$APP_SCORE,
    levels = lvls,
    labels = lbs
  )
  # PLOT -----------------------------------------------------------------------
  dred  <- "#B2182B"
  lred  <- "#EF6548"
  lblue <- "#92C5DE"
  dblue <- "#2166AC"
  grey  <- "#B0B0B0"

  colcode <- c(dred, lred, lblue, dblue, grey)
  names(colcode) <- lbs

  ratio <- dim(app_matrix)[1] / dim(app_matrix)[2]

  ref_env <- environment()

  plot_me <- function(m) {
    fli <- util_coord_flip(ref_env = ref_env)
    util_create_lean_ggplot(
      ggplot(m, aes(
        x = IMPLEMENTATION, y = VARIABLES,
        fill = APP_SCORE
      )) +
        geom_tile(colour = "white", linewidth = 0.8) + # https://github.com/tidyverse/ggplot2/issues/5051
        scale_fill_manual(values = colcode, name = " ") +
        {
          if (split_segments) facet_wrap(~STUDY_SEGMENT,
                                         scales = "free_y")# TODO: check ~
        } +
        theme_minimal() +
        scale_x_discrete(position = "top") +
        xlab("") +
        guides(fill = guide_legend(
          ncol = 1, nrow = length(colcode),
          byrow = TRUE
        )) +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 90, hjust = 0),
          axis.text.y = element_text(size = 10)#,
          #aspect.ratio = ratio
        ) + fli,
      m = m,
      colcode = colcode,
      split_segments = split_segments,
      fli = fli)
  }

  p <- plot_me(app_matrix_long)

  pl <- lapply(
    split(app_matrix_long, app_matrix_long$STUDY_SEGMENT),
    plot_me
  )


  ReportSummaryTable <- app_matrix
  ReportSummaryTable$STUDY_SEGMENT <- NULL

  ReportSummaryTable[2:ncol(ReportSummaryTable)] <-
    lapply(ReportSummaryTable[2:ncol(ReportSummaryTable)], util_as_numeric)

#  ReportSummaryTable[2:ncol(ReportSummaryTable)] <-
#    4 - ReportSummaryTable[2:ncol(ReportSummaryTable)]

  ReportSummaryTable$N <- 4

  attr(ReportSummaryTable, "higher_means") <- "better"
  attr(ReportSummaryTable, "continuous") <- FALSE
  attr(ReportSummaryTable, "colcode") <- colcode
  attr(ReportSummaryTable, "level_names") <-
    setNames(nm = 0:4, levels(app_matrix_long$APP_SCORE))


  ReportSummaryTable <- util_validate_report_summary_table(ReportSummaryTable,
                                                           meta_data = meta_data,
                                                           label_col = label_col)

  return(list(
    ApplicabilityPlot = p,
    ApplicabilityPlotList = pl,
    SummaryTable = app_matrix,
    ReportSummaryTable = ReportSummaryTable
  ))
}
