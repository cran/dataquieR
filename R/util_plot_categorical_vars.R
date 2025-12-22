#' Utility function to create plots for categorical variables
#'
#' Depending on the required level of complexity, this helper function creates
#' various plots for categorical variables. Next to basic bar plots, it also
#' enables group comparisons (for example for device/examiner effects) and
#' longitudinal views.
#'
#' @param resp_vars name of the categorical variable
#' @param group_vars name of the grouping variable
#' @param time_vars name of the time variable
#' @param study_data the data frame that contains the measurements
#' @param meta_data the data frame that contains metadata
#'                  attributes of study data
#' @param n_cat_max maximum number of categories to be displayed individually
#'                  for the categorical variable (`resp_vars`)
#' @param n_group_max maximum number of categories to be displayed individually
#'                  for the grouping variable (`group_vars`, devices / examiners)
#' @param n_data_min minimum number of data points to create a time course plot
#'                   for an individual category of the `resp_vars` variable
#'
#' @return a figure
#'
#' @noRd
util_plot_categorical_vars <- function(resp_vars,
                                   group_vars = NULL,
                                   time_vars = NULL,
                                   study_data,
                                   meta_data,
                                   n_cat_max =
                                     getOption("dataquieR.max_cat_resp_var_levels_in_plot",
                                               dataquieR.max_cat_resp_var_levels_in_plot_default),
                                   n_group_max =
                                     getOption("dataquieR.max_group_var_levels_in_plot",
                                               dataquieR.max_group_var_levels_in_plot_default),
                                   n_data_min =
                                     getOption("dataquieR.min_time_points_for_cat_resp_var",
                                               dataquieR.min_time_points_for_cat_resp_var_default)) {
  # preps and checks -----------------------------------------------------------
  util_expect_data_frame(study_data)
  util_expect_data_frame(meta_data)

  prep_prepare_dataframes(.replace_hard_limits = TRUE,
                          .apply_factor_metadata = TRUE)

  util_correct_variable_use(resp_vars,
                            allow_all_obs_na = FALSE,
                            need_scale = "nominal | ordinal")
  util_correct_variable_use(group_vars,
                            allow_null = TRUE,
                            allow_all_obs_na = FALSE,
                            need_type = "!float",
                            need_scale = "nominal | ordinal")
  util_correct_variable_use(time_vars,
                            allow_null = TRUE,
                            allow_all_obs_na = FALSE,
                            need_type = "datetime")

  util_expect_scalar(n_cat_max,
                     check_type = util_is_numeric_in(min = 2,
                                                     whole_num = TRUE,
                                                     finite = TRUE))
  util_expect_scalar(n_group_max,
                     check_type = util_is_numeric_in(min = 2,
                                                     whole_num = TRUE,
                                                     finite = TRUE))
  util_expect_scalar(n_data_min,
                     check_type = util_is_numeric_in(min = 0,
                                                     whole_num = TRUE,
                                                     finite = TRUE))
  N <- NULL # prevent warning 'no visible binding for global variable'

  plot_data <- ds1[, c(resp_vars, group_vars, time_vars), drop = FALSE]
  plot_data <- plot_data[complete.cases(plot_data), , drop = FALSE]
  tab_cat <- table(plot_data[[resp_vars]])
  # collapse 'rare' categories to reduce the number of categories, if needed
  if (length(tab_cat) > n_cat_max) {
    tab_cat <- tab_cat[order(tab_cat, decreasing = TRUE)]
    keep_cats <- names(tab_cat)[1:n_cat_max]
    levels(plot_data[[resp_vars]])[which(!levels(plot_data[[resp_vars]]) %in%
                                           keep_cats)] <- "other"
    # new category 'other' should always be the last one
    lvl_cat <-
      c(levels(plot_data[[resp_vars]])[which(levels(plot_data[[resp_vars]])
                                              %in% keep_cats)],
        "other")
    plot_data[[resp_vars]] <- as.character(plot_data[[resp_vars]])
    plot_data[[resp_vars]] <- factor(plot_data[[resp_vars]],
                                      levels = lvl_cat)
  }
  if (!is.null(group_vars)) {
    tab_gr <- table(plot_data[[group_vars]])
    # collapse 'rare' groups to reduce the number of levels, if needed
    if (length(tab_gr) > n_group_max) {
      tab_gr <- tab_gr[order(tab_gr, decreasing = TRUE)]
      keep_gr <- names(tab_gr)[1:n_group_max]
      levels(plot_data[[group_vars]])[which(!levels(plot_data[[group_vars]])
                                            %in% keep_gr)] <- "other"
      # new category 'other' should always be the last one
      lvl_gr <-
        c(levels(plot_data[[group_vars]])[which(levels(plot_data[[group_vars]])
                                                %in% keep_gr)],
          "other")
      plot_data[[group_vars]] <- as.character(plot_data[[group_vars]])
      plot_data[[group_vars]] <- factor(plot_data[[group_vars]],
                                        levels = lvl_gr)
    }
  }
  # omit categories with too few data points from the time course plot
  if (!is.null(time_vars)) {
    tab_cat <- table(plot_data[[resp_vars]])
    if (any(tab_cat) < n_data_min) {
      keep_cats <- names(tab_cat)[which(tab_cat >= n_data_min)]
      levels(plot_data[[resp_vars]])[which(!levels(plot_data[[resp_vars]]) %in%
                                             keep_cats)] <- NA
      plot_data <- plot_data[complete.cases(plot_data), , drop = FALSE]
    }
  }

  if (nrow(plot_data) == 0) {
    util_error("Not enough data to create a plot.",
               applicability_problem = FALSE)
  }

  # create figures -------------------------------------------------------------
  if (!is.null(group_vars) & !is.null(time_vars)) {

    # colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
    #             "#D55E00", "#CC79A7", "#8C510A")
    # n_gr <- length(levels(plot_data[[group_vars]]))
    # if (n_gr <= 8) {
    #   colors <- colors[1:n_gr]
    # } else {
    #   colors <- colorRampPalette(colors)(n_gr)
    # }
    pp <- util_histogram(plot_data = plot_data,
                         is_datetime = TRUE,
                         num_var = time_vars,
                         facet_var = resp_vars,
                         fill_var = group_vars)#,
                         #colors = colors)
  } else if (!is.null(time_vars)) {

    pp <- util_histogram(plot_data = plot_data,
                         is_datetime = TRUE,
                         num_var = time_vars,
                         facet_var = resp_vars)
  } else if (!is.null(group_vars)) {
    data_RepSumTab <- plot_data %>%
      dplyr::mutate(N = dplyr::n()) %>%
      dplyr::group_by(.data[[resp_vars]], .data[[group_vars]], N) %>%
      dplyr::summarise(freq = dplyr::n()) %>%
      dplyr::rename("Variables" = !!resp_vars) %>%
      as.data.frame(., check.names = FALSE)
    data_RepSumTab <- stats::reshape(data_RepSumTab, direction = "wide",
                                     idvar = c("Variables", "N"),
                                     timevar = group_vars)
    data_RepSumTab <- data_RepSumTab %>% replace(is.na(.), 0)
    colnames(data_RepSumTab) <- gsub("^freq\\.", "", colnames(data_RepSumTab))
    # TODO - PROBLEM: `print.ReportSummaryTable` drops the order of the levels
    # in 'Variables'. (I tried to reverse it, so that the first category is on
    # top, but because of the 'as.character' transformation I cannot change
    # the order of levels outside of `print.ReportSummaryTable`)
    # lvl_cat <- levels(data_RepSumTab$Variables)
    # data_RepSumTab$Variables <- as.character(data_RepSumTab$Variables)
    # data_RepSumTab$Variables <- factor(data_RepSumTab$Variables,
    #                                    levels = rev(lvl_cat))
    class(data_RepSumTab) <- c("ReportSummaryTable", "data.frame")
    pp <- print(data_RepSumTab, flip_mode = "flip", view = FALSE)
    # flipped because categories (here 'Variables') should be on the y-axis
    # here, only the figure is returned, so no chance for other visualizaions,
    # yet; i.e., we need to prevent the plot from being removed in
    # util_compress_ggplots_in_res()
    attr(pp, "from_ReportSummaryTable") <- NULL
    # attr(pp, "as_plotly") <- "util_as_plotly_util_plot_categorical_vars_gv"
  } else {
    plot_data <- plot_data %>%
      dplyr::group_by(.data[[resp_vars]]) %>%
      dplyr::summarise(freq = dplyr::n()) %>%
      as.data.frame()
    pp <- util_bar_plot(plot_data = plot_data,
                        cat_var = resp_vars,
                        num_var = "freq",
                        show_numbers = FALSE) #changed to FALSE to hide numbers
    attr(pp, "as_plotly") <- "util_as_plotly_util_plot_categorical_vars"
    attr(pp, "dont_util_adjust_geom_text_for_plotly") <- TRUE

    # Information for sizing
    obj1 <- ggplot2::ggplot_build(pp)
    obj1_data <- util_gg_get(obj1, "data")[[1]]
    rotated <- unique(obj1_data$flipped_aes)
    rotated <-  rotated[!is.na(rotated)]
    number_of_bars <- nrow(obj1_data)
    # fix number in case there are categories not listed in the obj1,
    # example OBS_BP_0 in ship example data
    if (is.factor(ds1[[resp_vars]])) {
      no_levels <- length(levels(ds1[[resp_vars]]))
      number_of_bars <- max(c(number_of_bars,no_levels))
    }
    min_bar_height <- min(obj1_data$ymax, na.rm = TRUE)
    max_bar_height <- max(obj1_data$ymax, na.rm = TRUE)
    range_bar <- max_bar_height - min_bar_height
    no_char_y = nchar(max_bar_height)
    no_char_x = max(nchar(as.character(plot_data[[resp_vars]])), na.rm = TRUE)


    attr(pp, "sizing_hints") <- list(
      figure_type_id = "bar_chart",
      rotated = rotated,
      number_of_bars = number_of_bars,
      range_bar = range_bar,
      no_char_y = no_char_y,
      no_char_x = no_char_x
    )



  }
  return(pp)
}

util_as_plotly_util_plot_categorical_vars <- function(res) {
  py <- util_ggplotly(res$SummaryPlot)
  pyb <- util_plotly_build(py)
  traces <- pyb$x$data
  # the color codes are defined in util_bar_plot
  black_bar_labs <-
     vapply(lapply(lapply(traces, `[[`, "textfont"), `[[`, "color"),
            identical, "rgba(0,0,0,1)", FUN.VALUE=logical(1))
  white_bar_labs <-
    vapply(lapply(lapply(traces, `[[`, "textfont"), `[[`, "color"),
            identical, "rgba(255,255,255,1)", FUN.VALUE=logical(1))
   py <- plotly::style(py, textposition = "bottom", traces = white_bar_labs)
  py <- plotly::style(py, textposition = "top", traces = black_bar_labs)
  py <- plotly::layout(py,
                       xaxis = list(tickangle = "auto"),
                       yaxis = list(tickangle = "auto"))
  py
}
