#' Check declared data types of metadata in study data
#'
#' @description
#' Checks data types of the study data and for the data type
#' declared in the metadata
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
#' @param resp_vars [variable] the names of the measurement variables, if
#'                             missing or `NULL`, all variables will be checked
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param split_segments [logical] return one matrix per study segment
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param max_vars_per_plot [integer] from=0. The maximum number of variables
#'                                            per single plot.
#' @param threshold_value [numeric] from=0 to=100. percentage failing
#'                                  conversions allowed to still classify a
#'                                  study variable convertible.
#' `inheritParams` `acc_distributions`
#' @return a list with:
#'   - `SummaryTable`: data frame about the applicability of each indicator
#'                   function (each function in a column).
#'                   its [integer] values can be one of the following four
#'                   categories:
#'                               0. Non-matching datatype,
#'                               1. Matching datatype,
#'   - `SummaryPlot`: [ggplot2] heatmap plot, graphical representation of
#'                       `SummaryTable`
#'   - `DataTypePlotList`: [list] of plots per (maybe artificial) segment
#'   - `ReportSummaryTable`: data frame underlying `SummaryPlot`
#' @export
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_manual facet_wrap
#'                     theme_minimal scale_x_discrete xlab guides
#'                     guide_legend theme element_text
#' @examples
#' \dontrun{
#' load(system.file("extdata/meta_data.RData", package = "dataquieR"), envir =
#'   environment())
#' load(system.file("extdata/study_data.RData", package = "dataquieR"), envir =
#'   environment())
#' appmatrix <- int_datatype_matrix(study_data = study_data,
#'                                  meta_data = meta_data,
#'                                  label_col = LABEL)
#' }
int_datatype_matrix <- function(resp_vars = NULL,
                                study_data, meta_data, split_segments =
                                     FALSE, label_col,
                                     max_vars_per_plot = 20,
                                threshold_value = 0
                                # , flip_mode = "noflip" # TODO: Improve style
                                ) {

  if (length(max_vars_per_plot) != 1 || !util_is_integer(max_vars_per_plot) ||
      is.na(max_vars_per_plot) || is.nan(max_vars_per_plot) ||
      max_vars_per_plot < 1) {
    util_error(c(
      "max_vars_per_plot must be one strictly positive non-complex integer",
      "value, may be Inf."
    ), applicability_problem = TRUE)
  }

  util_prepare_dataframes(.replace_missings = FALSE)

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
                                                     threshold_value =
                                                       threshold_value))
  dt_appl <- recode(dt_appl, `1` = 0, `2` = 1, `0` = 2)
#TODO: return_counts
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
  app_matrix_long <- melt(app_matrix, id.vars = c("Variables",
                                                  STUDY_SEGMENT))
  colnames(app_matrix_long) <- c("VARIABLES", "SEGMENT", "IMPLEMENTATION",
                                 "APP_SCORE")

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
        axis.text.y = element_text(size = 10),
        aspect.ratio = ratio
      ) # + util_coord_flip(ref_env = ref_env) # TODO: estimate w and h, since p is not using discrete axes
  }

  p <- plot_me(app_matrix_long)

  pl <- lapply(
    split(app_matrix_long, app_matrix_long$SEGMENT),
    plot_me
  )

  ReportSummaryTable <- app_matrix;

  ReportSummaryTable$N <- 1;

  ReportSummaryTable$STUDY_SEGMENT <- NULL

  attr(ReportSummaryTable, "colcode") <- setNames(
    c("#B2182B", "#92C5DE", "#2166AC"),
    nm = as.character(3:1))

  attr(ReportSummaryTable, "level_names") <-
    setNames(nm = 2:0, levels(app_matrix_long$APP_SCORE))

  attr(ReportSummaryTable, "continuous") <- FALSE

  class(ReportSummaryTable) <- union("ReportSummaryTable",
                                     class(ReportSummaryTable))

  SummaryData <- app_matrix

  SummaryData$MATCH <- factor(SummaryData$MATCH,
                                      levels = c(2:0),
                                      ordered = TRUE,
                                      labels = c(
                                        "Non-matching datatype",
                                        "Non-Matching datatype, convertible",
                                        "Matching datatype"
                                      )
  )


  SummaryTable <- app_matrix
  SummaryTable$GRADING <- SummaryTable$MATCH == 2

  return(list(
    SummaryPlot = p,
    DataTypePlotList = pl,
    SummaryTable = SummaryTable,
    SummaryData = SummaryData,
    ReportSummaryTable = ReportSummaryTable
  ))
}
