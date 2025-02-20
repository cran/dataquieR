#' Check declared data types of metadata in study data
#'
#' @description
#' Checks data types of the study data and for the data type
#' declared in the metadata
#'
#' [Indicator]
#'
#' @details
#' This is a preparatory support function that compares study data with
#' associated metadata. A prerequisite of this function is that the no. of
#' columns in the study data complies with the no. of rows in the metadata.
#'
#' For each study variable, the function searches for its data type declared in
#' static metadata and returns a heatmap like matrix indicating data type
#' mismatches in the study data.
#'
#' List function.
#'
#' @inheritParams .template_function_indicator
#'
#' @param resp_vars [variable] the names of the measurement variables, if
#'                             missing or `NULL`, all variables will be checked
#' @param split_segments [logical] return one matrix per study segment
#' @param max_vars_per_plot [integer] from=0. The maximum number of variables
#'                                            per single plot.
#' @param threshold_value [numeric] from=0 to=100. percentage failing
#'                                  conversions allowed to still classify a
#'                                  study variable convertible.
#' @return a list with:
#'   - `SummaryTable`: data frame containing data quality check for
#'                    "data type mismatch" (`CLS_int_vfe_type`,
#'                    `PCT_int_vfe_type`). The following categories are possible:
#'                    categories: "Non-matching datatype",
#'                    "Non-Matching datatype, convertible",
#'                    "Matching datatype"
#'   - `SummaryData`: data frame containing data quality check for
#'                    "data type mismatch" for a report
#'   - `SummaryPlot`: [ggplot2::ggplot2] heatmap plot, graphical representation of
#'                       `SummaryTable`
#'   - `DataTypePlotList`: [list] of plots per (maybe artificial) segment
#'   - `ReportSummaryTable`: data frame underlying `SummaryPlot`
#' @export
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_manual facet_wrap
#'                     theme_minimal scale_x_discrete xlab guides
#'                     guide_legend theme element_text
int_datatype_matrix <- function(resp_vars = NULL,
                                study_data,
                                label_col,
                                item_level = "item_level",
                                split_segments = FALSE,
                                max_vars_per_plot = 20,
                                threshold_value = 0,
                                # , flip_mode = "noflip" # TODO: Improve style
                                meta_data = item_level,
                                meta_data_v2) {

  if (.called_in_pipeline) {
    return(list(
      SummaryData =
        "Dummy result, should not be displayed, internal error, please report."))
  }

  util_maybe_load_meta_data_v2()

  if (length(max_vars_per_plot) != 1 || !util_is_integer(max_vars_per_plot) ||
      is.na(max_vars_per_plot) || is.nan(max_vars_per_plot) ||
      max_vars_per_plot < 1) {
    util_error(c(
      "max_vars_per_plot must be one strictly positive non-complex integer",
      "value, may be Inf."
    ), applicability_problem = TRUE)
  }

  if (missing(label_col)) {
    orig_label_col <- rlang::missing_arg()
  } else {
    orig_label_col <- force(label_col)
  }

  label_col <- attr(prep_get_labels("",
                                    item_level = meta_data,
                                    label_class = "SHORT",
                                    label_col = label_col),
                    "label_col")

  prep_prepare_dataframes(.replace_missings = FALSE, .adjust_data_type = FALSE)

  # correct variable use?
  util_correct_variable_use("resp_vars",
                            allow_more_than_one = TRUE,
                            allow_null          = TRUE,
                            allow_any_obs_na    = TRUE
  )

  # if no resp_vars specified use all columns of studydata
  if (length(resp_vars) == 0) {
    resp_vars <- names(ds1)
  } else {
    ds1 <- ds1[, resp_vars, drop = FALSE]
  }

  Variables <- resp_vars
  # this matrix will be shown to users
  app_matrix <- data.frame(Variables = Variables, stringsAsFactors = FALSE)
  # this matrix can be used to trigger computations
  # tri_matrix <- data.frame(Variables = Variables)
  # defined for merge of strata (optional)
  by_y <- label_col

  # DATA_TYPE defined in metadata?
  if (!(DATA_TYPE %in% names(meta_data))) {
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
  if (!all(unique(meta_data$DATA_TYPE) %in% DATA_TYPES)) {
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
  dt_appl <-
             as.numeric(util_compare_meta_with_study(sdf = ds1, mdf = meta_data,
                                                     label_col = label_col,
                                                     check_convertible = TRUE,
                                                     check_conversion_stable = TRUE,
                                                     threshold_value =
                                                       threshold_value))
  dt_appl <- recode(dt_appl,
                    `1` = 0,
                    `2` = 1,
                    `0` = 2,
                    `3` = 3) # 3, convetible with drawbacks

  # two columns in heatmap: percentage missmatch, percentage non-convertible
  # percentage convertible?

  app_matrix$MATCH = dt_appl

  # SHOULD STRATIFICATION OF SEGMENTS BE USED? ---------------------------------

  # is STUDY_SEGMENT in metadata and none of the entries has NA?
  strata_defined <- STUDY_SEGMENT %in% names(meta_data)

  if (!strata_defined && split_segments) {
    util_warning(c(
      "Stratification for STUDY_SEGMENT is not possible due to missing",
      "metadata. Will split arbitrarily avoiding too large figures"
    ), applicability_problem = TRUE)
    nvars <- nrow(meta_data)
    meta_data$STUDY_SEGMENT <- paste0("Block #", ceiling(1:nvars /
                                                             max_vars_per_plot))
  } else if (strata_defined && split_segments) {
    if (any(is.na(meta_data$STUDY_SEGMENT))) {
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
      nvars <- sum(meta_data$STUDY_SEGMENT == too_big_block, na.rm = TRUE) # TODO: Use [[STUDY_SEGMENT]]
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
  app_matrix_long <- stats::reshape(data = app_matrix,
                                     idvar = c("Variables",
                                               STUDY_SEGMENT),
                                     varying = colnames(app_matrix)[2],
                                     v.names = "value",
                                     times = colnames(app_matrix)[2],
                                     direction = "long")
  app_matrix_long$time <- as.factor(app_matrix_long$time)

  lev <- app_matrix$Variables
  app_matrix_long$Variables <- factor(app_matrix_long$Variables,
                                      levels = lev, ordered = FALSE)



  colnames(app_matrix_long) <- c("VARIABLES", "SEGMENT", "IMPLEMENTATION",
                                 "APP_SCORE")
  rownames(app_matrix_long) <- NULL


  # assign factor labels
  app_matrix_long$APP_SCORE <- factor(app_matrix_long$APP_SCORE,
    levels = c(2:0),
    ordered = TRUE,
    labels = c(
      "Non-matching datatype",
      "Non-Matching datatype, convertible",
      "Matching datatype"
    )
  )
  # PLOT -----------------------------------------------------------------------
  # colcode <- c("#B2182B", "#ef6548", "#92C5DE", "#2166AC", "#B0B0B0")
  colcode <- c("#B2182B", "#92C5DE", "#2166AC")
  names(colcode) <- levels(app_matrix_long$APP_SCORE)

  ratio <- dim(app_matrix)[1] / dim(app_matrix)[2]

  # ref_env <- environment() TODO

  plot_me <- function(m) {
    ggplot(m, aes(
      x = IMPLEMENTATION, y = VARIABLES,
      fill = APP_SCORE
    )) +
      geom_tile(colour = "white", linewidth = 0.8) + # https://github.com/tidyverse/ggplot2/issues/5051
      scale_fill_manual(values = colcode, name = " ") +
      {
        if (split_segments) facet_wrap(~SEGMENT, scales = "free_y")# TODO: check ~
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
       # aspect.ratio = ratio
      ) # + util_coord_flip(ref_env = ref_env) # TODO: estimate w and h, since p is not using discrete axes
  }

  p <- plot_me(app_matrix_long)

  pl <- lapply(
    split(app_matrix_long, app_matrix_long$SEGMENT),
    plot_me
  )

  # Figure size hint for plot p and list of plots pl
  attr(p, "sizing_hints") <- list(
    figure_type_id = "dot_mat",
    rotated = FALSE,
    number_of_vars = nrow(app_matrix_long) ,
    number_of_cat = 3) # this is always 3 types

  attr(pl, "sizing_hints") <- list(
    figure_type_id = "dot_mat",
    rotated = FALSE,
    number_of_vars = length(lapply(
      split(app_matrix_long, app_matrix_long$SEGMENT),
      nrow)), # a list with the no. variables per segment
    number_of_cat = 3) # this is always 3 types



  cmp_res <- util_compare_meta_with_study(sdf = ds1, mdf = meta_data,
                               label_col = label_col,
                               check_convertible = TRUE,
                               check_conversion_stable = TRUE,
                               return_percentages = TRUE,
                               threshold_value =
                                 threshold_value)
  ReportSummaryTable <- as.data.frame(t(cmp_res))
  wv <- attr(cmp_res, "which_vec")
  wv <- wv[vapply(wv, length, FUN.VALUE = integer(1)) > 0] # TODO: Why is this needed, example was from NAKO with a two-row table from /Users/struckmanns/tmp/NAKO_int_prob.RData
  FlaggedStudyData <- as.data.frame(wv,
                                    stringsAsFactors = FALSE)
  # FIXME: we do not need 4 bits, 2 would suffice
  # 0 -- match
  # 12 -- convertible mismatch, stable
  # 10 -- convertible mismatch, unstable
  # 9  -- nonconvertible mismatch
  FlaggedStudyData[] <- lapply(FlaggedStudyData,
                               factor,
                               ordered = TRUE,
                               levels = c(0, 12, 10, 9),
                               labels = c(
                                 "match",
                                 "convertible mismatch, stable",
                                 "convertible mismatch, unstable",
                                 "nonconvertible mismatch"
                                 ))

  #######
  match <- ReportSummaryTable$match
  ReportSummaryTable$match <- NULL
  ReportSummaryTable$convertible_mismatch_stable <-
    ReportSummaryTable$convertible_mismatch_stable / 100
  ReportSummaryTable$convertible_mismatch_unstable <-
    ReportSummaryTable$convertible_mismatch_unstable / 100
  ReportSummaryTable$nonconvertible_mismatch <-
    ReportSummaryTable$nonconvertible_mismatch / 100
  colnames(ReportSummaryTable)[colnames(ReportSummaryTable) ==
                                 "convertible_mismatch_stable"] <-
    "convertible mismatch, stable"
  colnames(ReportSummaryTable)[colnames(ReportSummaryTable) ==
                                 "convertible_mismatch_unstable"] <-
    "convertible mismatch, unstable"
  colnames(ReportSummaryTable)[colnames(ReportSummaryTable) ==
                                 "nonconvertible_mismatch"] <-
    "nonconvertible mismatch"

  ReportSummaryTable$N <- rep(1, nrow(ReportSummaryTable))
  ReportSummaryTable$Variables <- rownames(ReportSummaryTable)

  rownames(ReportSummaryTable) <- NULL

  attr(ReportSummaryTable, "continuous") <- TRUE
  attr(ReportSummaryTable, "colscale") <- c("#B2182B", "#92C5DE", "#2166AC")

  ReportSummaryTable <- util_validate_report_summary_table(ReportSummaryTable,
                                                    meta_data = meta_data,
                                                    label_col = label_col)

  app_matrix$PCT_int_vfe_type <-
    setNames(100 * ReportSummaryTable$`nonconvertible mismatch`,
             nm = ReportSummaryTable$Variables)[app_matrix$Variables]

  app_matrix$Variables <- as.character(app_matrix$Variables)

  # create SummaryData table
  SummaryData <- app_matrix[, c("Variables", "PCT_int_vfe_type")]

  SummaryData$PCT_int_vfe_type <-
    paste0(round(SummaryData$PCT_int_vfe_type * nrow(ds1) / 100, digits = 0),
           " (",
           base::format(SummaryData$PCT_int_vfe_type, digits = 2, nsmall = 2), "%)")

  names(SummaryData)[names(SummaryData) == "PCT_int_vfe_type"] <-
    "Data type mismatch N (%)"

#  names(SummaryData)[names(SummaryData) == "PCT_int_vfe_type"] <-
#   util_translate_indicator_metrics("PCT_int_vfe_type",
#                                    short = FALSE, long = FALSE)

  SummaryData$`Convertible mismatch, stable N (%)` <-
    paste0( round(ReportSummaryTable$`convertible mismatch, stable` * nrow(ds1),
                  digits = 0), " (",
            base::format(100 * ReportSummaryTable$`convertible mismatch, stable`, digits = 2, nsmall = 2),
            "%)")
  SummaryData$`Convertible mismatch, unstable N (%)`<-
    paste0(round(ReportSummaryTable$`convertible mismatch, unstable` * nrow(ds1),
                 digits = 0), " (",
           base::format(100 * ReportSummaryTable$`convertible mismatch, unstable`, digits = 2, nsmall = 2),
           "%)")

  SummaryData$`Data type match N (%)` <-
    paste0(round(match / 100 * nrow(ds1), digits = 0), " (",
           base::format(match, digits = 2, nsmall = 2), "%)" )

  SummaryData$`Expected DATA_TYPE` <- setNames(meta_data[[DATA_TYPE]],
                        nm = meta_data[[label_col]])[SummaryData$Variables]

  SummaryData$`Observed DATA_TYPE` <-
    setNames(prep_datatype_from_data(resp_vars = # TODO: Improve
                                       SummaryData$Variables,
                                     study_data = ds1),
             nm = colnames(ds1))[SummaryData$Variables]

  SummaryData <- cbind(SummaryData,
                             app_matrix[, c("MATCH", "STUDY_SEGMENT"),
                                        drop = FALSE])

  SummaryData$MATCH <- factor(
    SummaryData$MATCH,
    levels = c(3:0),
    ordered = TRUE,
    labels = c(
      "Non-matching datatype, convertible, unstable",
      "Non-matching datatype",
      "Non-Matching datatype, convertible, stable",
      "Matching datatype"
    )
  )

  colnames(SummaryData)[colnames(SummaryData) == "MATCH"] <-
    "State, given threshold"

  #Add hover text to SummaryData table
  text_to_display <- util_get_hovertext("[int_datatype_matrix_hover]")
  attr(SummaryData, "description") <- text_to_display



  SummaryTable <- app_matrix
  SummaryTable$GRADING <- SummaryTable$MATCH == 2
  names(SummaryTable)[names(SummaryTable) == "MATCH"] <- "CLS_int_vfe_type"

  ReportSummaryTable[,
                     c("convertible mismatch, stable",
                       "convertible mismatch, unstable",
                       "nonconvertible mismatch",
                       "N")] <-
    round(ReportSummaryTable[, c("convertible mismatch, stable",
                           "convertible mismatch, unstable",
                           "nonconvertible mismatch",
                           "N")] / ReportSummaryTable$N * nrow(ds1), digits = 0)
  attr(ReportSummaryTable, "relative") <- TRUE

  if (!missing(orig_label_col)) { # map back ReportSummaryTable to requested label_col and tables to suitable table labels
    SummaryTable[["Variables"]] <-
      prep_map_labels(x = SummaryTable[["Variables"]],
                      item_level = meta_data,
                      to  = orig_label_col,
                      from = label_col,
                      ifnotfound = SummaryTable[["Variables"]],
                      warn_ambiguous = FALSE)
    ReportSummaryTable[["Variables"]] <-
      prep_map_labels(x = ReportSummaryTable[["Variables"]],
                      item_level = meta_data,
                      to  = orig_label_col,
                      from = label_col,
                      ifnotfound = SummaryTable[["Variables"]],
                      warn_ambiguous = FALSE)
    SummaryData[["Variables"]] <-
      prep_map_labels(x = SummaryData[["Variables"]],
                      item_level = meta_data,
                      to  = orig_label_col,
                      from = label_col,
                      ifnotfound = SummaryTable[["Variables"]],
                      warn_ambiguous = FALSE)
  }

  attr(ReportSummaryTable, "flip_mode") <- "noflip";

  return(list( # FIXME: Add Flagged- and Modified-StudyData
    SummaryPlot = p,
    DataTypePlotList = pl,
    SummaryTable = SummaryTable,
    SummaryData = SummaryData,
    ReportSummaryTable = ReportSummaryTable,
    FlaggedStudyData = FlaggedStudyData
  ))
}
