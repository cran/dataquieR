#' Estimate marginal means, see [emmeans::emmeans]
#'
#' @description
#' This function examines the impact of so-called process variables on a
#' measurement variable. This implementation combines a descriptive and a
#' model-based approach. Process variables that can be considered in this
#' implementation must be categorical. It is currently not possible to
#' consider more than one process variable within one function call.
#' The measurement variable can be adjusted for (multiple) covariables, such as
#' age or sex, for example.
#'
#' Marginal means rests on model-based results, i.e. a significantly different
#' marginal mean depends on sample size. Particularly in large studies, small
#' and irrelevant differences may become significant. The contrary holds if
#' sample size is low.
#'
#' [Indicator]
#'
#' @details
#' Limitations
#'
#' Selecting the appropriate distribution is complex. Dozens of continuous,
#' discrete or mixed distributions are conceivable in the context of
#' epidemiological data. Their exact exploration is beyond the scope of this
#' data quality approach. The present function uses the help function
#' \link{util_dist_selection}, the assigned `SCALE_LEVEL` and the `DATA_TYPE`
#' to discriminate the following cases:
#' \itemize{
#'   \item continuous data
#'   \item binary data
#'   \item count data with <= 20 distinct values
#'   \item count data with > 20 distinct values (treated as continuous)
#'   \item nominal data
#'   \item ordinal data
#'  }
#' Continuous data and count data with more than 20 distinct values are analyzed
#' by linear models. Count data with up to 20 distinct values are modeled by a
#' Poisson regression. For binary data, the implementation uses logistic
#' regression.
#' Nominal response variables will either be transformed to binary variables or
#' analyzed by multinomial logistic regression models. The latter option is only
#' available if the argument `dichotomize_categorical_resp` is set to `FALSE`
#' and if the package `nnet` is installed. The transformation to a binary
#' variable can be user-specified using the metadata columns `RECODE_CASES`
#' and/or `RECODE_CONTROL`. Otherwise, the most frequent category will be
#' assigned to cases and the remaining categories to control.
#' For ordinal response variables, the argument `cut_off_linear_model_for_ord`
#' controls whether the data is analyzed in the same way as continuous data:
#' If every level of the variable has at least as many observations as specified
#' in the argument, the data will be analyzed by a linear model. Otherwise,
#' the data will be modeled by a ordered regression, if the package `ordinal`
#' is installed.
#'
#' @keywords accuracy
#'
#' @inheritParams .template_function_indicator
#'
#' @param resp_vars  [variable] the name of the measurement variable
#' @param group_vars [variable list] len=1-1. the name of the observer, device
#'                                   or reader variable
#' @param co_vars [variable list] a vector of covariables, e.g. age and sex for
#'                              adjustment
#' @param threshold_type [enum] empirical | user | none. In case `empirical` is
#'                       chosen, a multiplier of the scale measure is used.
#'                       In case of `user`, a value of the mean or probability
#'                       (binary data) has to be defined
#'                       see [`Implementation and use of thresholds`](https://dataquality.qihs.uni-greifswald.de/VIN_acc_impl_margins.html#Implementation_and_use_of_thresholds) in the  online documentation).
#'                       In case of `none`, no thresholds are displayed and no
#'                       flagging of unusual group levels is applied.
#' @param threshold_value [numeric] a multiplier or absolute value (see
#'                       [`Implementation and use of thresholds`](https://dataquality.qihs.uni-greifswald.de/VIN_acc_impl_margins.html#Implementation_and_use_of_thresholds) in the
#'                       online documentation).
#' @param min_obs_in_subgroup [integer] from=0. This optional argument specifies
#'                       the minimum number of observations that is required to
#'                       include a subgroup (level) of the `group_var` in the
#'                       analysis. Subgroups with less observations are
#'                       excluded.
#' @param min_obs_in_cat [integer] This optional argument specifies the minimum
#'                        number of observations that is required to include
#'                        a category (level) of the outcome (`resp_vars`) in
#'                        the analysis. Categories with less observations are
#'                        combined into one group. If the collapsed category
#'                        contains less observations than required, it will be
#'                        excluded from the analysis.
#' @param dichotomize_categorical_resp [logical] Should nominal response
#'                        variables always be transformed to binary variables?
#' @param cut_off_linear_model_for_ord [integer] from=0. This optional argument
#'                        specifies the minimum number of observations for
#'                        individual levels of an ordinal outcome (`resp_var`)
#'                        that is required to run a linear model instead of an
#'                        ordered regression (i.e., a cut-off value above which
#'                        linear models are considered a good approximation).
#'                        The argument can be set to `NULL` if ordered
#'                        regression models are preferred for ordinal data in
#'                        any case.
#' @param sort_group_var_levels [logical] Should the levels of the grouping
#'                        variable be sorted descending by the number of
#'                        observations? Note that ordinal grouping variables
#'                        will not be reordered.
#' @param include_numbers_in_figures [logical] Should the figure report the
#'                        number of observations for each level of the grouping
#'                        variable?
#' @param n_violin_max [integer] from=0. This optional argument specifies
#'                       the maximum number of levels of the `group_var` for
#'                       which violin plots will be shown in the figure.
#'
#' @return a list with:
#'   - `SummaryTable`: [data.frame] underlying the plot
#'   - `ResultData`: [data.frame]
#'   - `SummaryPlot`: [ggplot2::ggplot()] margins plot
#'
#' @export

#' @importFrom utils tail head
#'
#' @seealso
#' [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_acc_impl_margins.html
#' )
acc_margins <- function(resp_vars = NULL,
                        group_vars = NULL,
                        co_vars = NULL,
                        study_data,
                        label_col,
                        item_level = "item_level",
                        threshold_type = "empirical",
                        threshold_value,
                        min_obs_in_subgroup = 5,
                        min_obs_in_cat = 5,
                        dichotomize_categorical_resp = TRUE,
                        cut_off_linear_model_for_ord = 10,
                        meta_data = item_level,
                        meta_data_v2,
                        sort_group_var_levels =
                          getOption("dataquieR.acc_margins_sort",
                                    dataquieR.acc_margins_sort_default),
                        include_numbers_in_figures =
                          getOption("dataquieR.acc_margins_num",
                                    dataquieR.acc_margins_num_default),
                        n_violin_max =
                          getOption("dataquieR.max_group_var_levels_with_violins",
                                    dataquieR.max_group_var_levels_with_violins_default)) { # TODO: flip_mode =
  #prep for meta_data_v2
  util_maybe_load_meta_data_v2()

  # to avoid "no visible binding for global variable ‘sample_size’"
  sample_size <- NULL
  # preps ----------------------------------------------------------------------
  # map metadata to study data
  prep_prepare_dataframes(.replace_hard_limits = TRUE,
                          .apply_factor_metadata = TRUE)

  util_correct_variable_use("resp_vars",
                            need_scale = "!na",
                            min_distinct_values = 2)

  util_correct_variable_use("group_vars",
                            allow_any_obs_na = TRUE,
                            need_type = "!float",
                            need_scale = "nominal | ordinal")

  util_correct_variable_use("co_vars",
                            allow_more_than_one = TRUE,
                            allow_all_obs_na = FALSE,
                            allow_null = TRUE,
                            allow_na = TRUE,
                            allow_any_obs_na = TRUE)

  co_vars <- na.omit(co_vars)
  if (is.null(co_vars)) {
    co_vars <- character(0)
  }

  # replace dollar signs for emmeans
  dollar <- "\uFE69"
  colnames(ds1) <- gsub("$", dollar, fixed = TRUE, colnames(ds1))
  if (any(grepl('$', fixed = TRUE, c(resp_vars, group_vars, co_vars)))) {
    util_message(c("emmeans used by acc_margins does not support variable",
                   "names containing %s, replacing this symbol by an",
                   "equivalent unicode character %s"),
                 dQuote("$"),
                 dQuote(dollar), applicability_problem = TRUE,
                 intrinsic_applicability_problem = FALSE)
  }
  original_resp_vars <- resp_vars
  original_group_vars <- group_vars
  original_co_vars <- co_vars
  resp_vars <- gsub("$", dollar, fixed = TRUE, resp_vars)
  group_vars <- gsub("$", dollar, fixed = TRUE, group_vars)
  co_vars <- gsub("$", dollar, fixed = TRUE, co_vars)

  if (length(setdiff(co_vars, "1")) > 0) {
    lb <- prep_get_labels(original_co_vars,
                          item_level = meta_data,
                          label_col = label_col,
                          label_class = "LONG",
                          resp_vars_match_label_col_only = TRUE)
    if (length(lb) < 4) {
      adjusted_hint <- sprintf("adjusted for %s", paste0(lb, collapse = ", "))
    } else {
      adjusted_hint <- sprintf("adjusted for %d variables", length(lb))
    }
  } else {
    adjusted_hint <- ""
  }

  title <- paste(prep_get_labels(original_group_vars,
                                 item_level = meta_data,
                                 label_col = label_col,
                                 label_class = "LONG",
                                 resp_vars_match_label_col_only = TRUE),
                 "margins in",
                 prep_get_labels(original_resp_vars,
                                 item_level = meta_data,
                                 label_col = label_col,
                                 label_class = "LONG",
                                 resp_vars_match_label_col_only = TRUE))

  util_expect_scalar(min_obs_in_subgroup,
                     check_type = util_is_numeric_in(min = 5,
                                                     whole_num = TRUE,
                                                     finite = TRUE),
                     convert_if_possible = function(x) {
                       x1 <- suppressWarnings(as.integer(x))
                       if (is.na(x1) ||
                           !util_is_numeric_in(min = 5, whole_num = TRUE,
                                               finite = TRUE)(x1)) {
                         x1 <- as.integer(5)
                         util_message(
                           paste("min_obs_in_subgroup is not specified",
                                 "correctly and is set to 5 instead."),
                           applicability_problem = TRUE)
                       }
                       x1
                     })

  util_expect_scalar(min_obs_in_cat,
                     check_type = util_is_numeric_in(min = 1,
                                                     whole_num = TRUE,
                                                     finite = TRUE))

  util_expect_scalar(sort_group_var_levels, check_type = is.logical)
  if (meta_data[[SCALE_LEVEL]][
    meta_data[[label_col]] == original_group_vars] %in% SCALE_LEVELS$ORDINAL) {
    sort_group_var_levels <- FALSE
  }
  util_expect_scalar(include_numbers_in_figures, check_type = is.logical)
  util_expect_scalar(n_violin_max,
                     check_type = util_is_numeric_in(min = 0))

  # omit missing values and unnecessary variables
  n_prior <- nrow(ds1)
  ds1 <- ds1[, c(resp_vars, group_vars, co_vars), drop = FALSE]
  ds1 <- ds1[complete.cases(ds1[, c(group_vars, co_vars)]), ]
  n_post <- nrow(ds1)
  msg <- NULL
  if (n_post < n_prior) {
    msg <- paste0(
      "Due to missing values in ",
      ifelse(length(co_vars) > 0,
             paste(paste0(co_vars, collapse = ", "), "or "),
             ""),
      group_vars, ", N = ", n_prior - n_post,
      " observations were excluded. "
    )
  }
  n_prior <- n_post
  ds1 <- ds1[complete.cases(ds1), ]
  n_post <- nrow(ds1)
  if (n_post < n_prior) {
    msg <- paste0(
      msg, "Due to missing values in ", resp_vars, ", N = ",
      n_prior - n_post, " observations were excluded",
      ifelse(nchar(msg) > 0, " additionally.", "."))
  }

  if (length(msg) > 0 && nchar(msg) > 0) {
    util_message(trimws(msg),
                 applicability_problem = FALSE)
  }

  if (!(prod(dim(ds1)))) {
    util_error("No data left after data preparation.")
  }

  # ensure that included levels of the grouping variable have the specified
  # minimum number of observations
  check_df <- util_table_of_vct(ds1[[group_vars]])
  critical_levels <- levels(check_df$Var1)[check_df$Freq <
                                             min_obs_in_subgroup]
  if (length(critical_levels) > 0) {
    util_message("Levels %s were excluded due to less than %d observations.",
                 paste0(c(vapply(head(critical_levels, 10), dQuote, ""),
                          if (length(critical_levels) <= 10)
                            character(0) else "..."),
                        collapse = ", "),
                 min_obs_in_subgroup,
                 applicability_problem = FALSE)
    # exclude levels with too few observations
    ds1 <- ds1[!(ds1[[group_vars]] %in% critical_levels), ]
    levels(ds1[[group_vars]])[
      which(levels(ds1[[group_vars]]) %in% critical_levels)] <- NA
  }
  ds1 <- ds1[complete.cases(ds1), , drop = FALSE]
  if (nrow(ds1) == 0) {
    util_error("No data left after data preparation.")
  }

  if (!missing(threshold_value)) {
    if (is.vector(threshold_value)) {
      .threshold_value <- as.numeric(threshold_value)
    } else {
      .threshold_value <- NA
    }
    if (length(threshold_value) != 1 || is.na(.threshold_value)) {
      util_message(
        "threshold_value is not numeric(1): %s, setting it to default value 1.",
        dQuote(head(try(as.character(threshold_value)), 1)),
        applicability_problem = TRUE)
      threshold_value <- 1
    } else {
      threshold_value <-
        .threshold_value
    }
  }

  if (is.null(threshold_type) || (!is.list(threshold_type)
                                  && length(threshold_type) != 1)) {
    if (
      !is.null(threshold_type)
      ||
      !.called_in_pipeline
    ) util_message("No or many threshold type specified and set to empirical.",
                   applicability_problem = TRUE)
    threshold_type <- "empirical"
  }

  threshold_type <- match.arg(threshold_type, c("empirical", "user", "none"))

  # no relative distance (based on SD) to mean defined?
  if (threshold_type %in% c("empirical", "none") & missing(threshold_value)) {
    threshold_value <- 1
  }

  # threshold is user but no value defined -> switch to empirical
  if (threshold_type == "user" & missing(threshold_value)) {
    util_message(
      c(
        "Threshold was set to user but no value for the unit of measurements",
        "was defined.\n",
        "The function switches to one SD as default."),
      applicability_problem = TRUE)
    threshold_type == "empirical"
    threshold_value <- 1
  }

  # check first if there is a user-specified recoding
  if (("RECODE_CASES" %in% colnames(meta_data) &&
       !util_empty(meta_data[meta_data[[label_col]] == resp_vars, RECODE_CASES])) ||
      ("RECODE_CONTROL" %in% colnames(meta_data) &&
       !util_empty(meta_data[meta_data[[label_col]] == resp_vars, RECODE_CONTROL]))) {
    rvs_bin <- util_dichotomize(
      study_data = ds1[, resp_vars, drop = FALSE],
      meta_data = meta_data,
      label_col = label_col)
    rvs_bin_note <- attr(rvs_bin, "Dichotomization")[[resp_vars]]
    ds1[[resp_vars]] <- unlist(rvs_bin)
    ds1 <- ds1[complete.cases(ds1), , drop = FALSE]
  }

  var_prop <- util_dist_selection(ds1[, resp_vars, drop = FALSE])
  if (var_prop$NDistinct < 2) {
    util_error("The response variable is constant after data preparation.",
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }

  # call utility functions based on scale and data properties ------------------
  var_scale <- meta_data[[SCALE_LEVEL]][meta_data[[label_col]] == resp_vars]
  var_dtype <- meta_data[[DATA_TYPE]][meta_data[[label_col]] == resp_vars]

  ###1st CASE: there are only 2 distinct values
  if (var_prop$NDistinct == 2) {
    if (nrow(ds1) < 2 * min_obs_in_cat) {
      util_error("Not enough data (after data preparation).",
                 applicability_problem = TRUE,
                 intrinsic_applicability_problem = TRUE)
    }
    # recode binary variable to 0/1, if needed
    if (!all(unique(ds1[[resp_vars]]) %in% c(0, 1))) {
      bin_codes <- names(sort(table(ds1[[resp_vars]])))
      mf <- tail(bin_codes, 1)
      lf <- head(bin_codes, 1)
      # https://stackoverflow.com/questions/12187187/
      #     how-to-retrieve-the-most-repeated-value-in-a-
      #                                         column-present-in-a-data-frame
      col <- NA
      col[ds1[[resp_vars]] == mf] <- 0
      col[ds1[[resp_vars]] == lf] <- 1
      ds1[[resp_vars]] <- col
      rvs_bin_note <- paste(
        paste("Cases (1):", lf),
        paste("Control (0):", mf),
        sep = ". ")
    } else {
      rvs_bin_note <- "Cases (1): 1. Control (0): 0"
    }
    # run margins function for binary response
    mar_out <- util_margins_bin(resp_vars = resp_vars,
                                group_vars = group_vars,
                                co_vars = co_vars,
                                threshold_type = threshold_type,
                                threshold_value = threshold_value,
                                min_obs_in_subgroup = min_obs_in_subgroup,
                                min_obs_in_cat = min_obs_in_cat,
                                caption = rvs_bin_note,
                                ds1 = ds1,
                                label_col = label_col,
                                adjusted_hint = adjusted_hint,
                                title = title,
                                sort_group_var_levels = sort_group_var_levels,
                                include_numbers_in_figures =
                                  include_numbers_in_figures)
    obj1<- ggplot2::ggplot_build(mar_out$plot)
    obj1_data <- util_rbind(data_frames_list = obj1$data)
    min_value <- min(c(obj1_data$x,obj1_data$xintercept),  na.rm = TRUE)
    max_value <- max(c(obj1_data$x,obj1_data$xintercept),  na.rm = TRUE)
    range_values <- max_value - min_value

 #   if (exists("mf")) {
#      no_char_y <- max(nchar(c(mf, lf)))
#    } else {
 #     no_char_y <- 1
#    }
    no_char_y <- nchar(range_values)
    rm(obj1, obj1_data)
    type_plot <- "count_plot"


    ###2nd CASE: NOMINAL (2 possible results)
  } else if (var_scale == SCALE_LEVELS$NOMINAL) {
    if (nrow(ds1) < 2 * min_obs_in_cat) {
      util_error("Not enough data (after data preparation).",
                 applicability_problem = TRUE,
                 intrinsic_applicability_problem = TRUE)
    }
    # Nominal response variables will either be transformed to binary variables
    # or analyzed by multinomial logistic regression models.
    count_nom <- util_table_of_vct(ds1[[resp_vars]])
    count_nom <- count_nom[which(count_nom[, 2] > 0), ]
    count_nom <- count_nom[order(count_nom[, 2], decreasing = TRUE), ]
    count_nom$below_thresh <- count_nom[, 2] < min_obs_in_cat
    # catch cases were the nominal response variable has to be analyzed as a
    # binary variable:
    if (nrow(count_nom) == 2 | # i.e., there are only two categories
        length(which(count_nom$below_thresh)) >= nrow(count_nom) - 1 |
        # i.e., (almost) all categories have too few observations to be
        # analyzed individually
        (sum(count_nom[which(count_nom$below_thresh), 2]) < min_obs_in_cat &
         length(which(!count_nom$below_thresh)) == 2)
        # If we would collapse all rare categories, they would still have too
        # few observations to form a third category. Thus, we can only analyse
        # two categories individually.
    ) {
      dichotomize_categorical_resp <- TRUE
    }

    if (dichotomize_categorical_resp) {
      # Dichotomized nominal variables will be analyzed by logistic models.
      # The dichotomization can be user-defined in the metadata (RECODE_CASES
      # and/or RECODE_CONTROL). If not, dichotomization will be performed
      # automatically (if 'dichotomize_categorical_resp' is TRUE).
      if (!("RECODE_CASES" %in% colnames(meta_data))) {
        meta_data[[RECODE_CASES]] <- ""
      }
      if (!("RECODE_CONTROL" %in% colnames(meta_data))) {
        meta_data[[RECODE_CONTROL]] <- ""
      }
      if (util_empty(meta_data[[RECODE_CASES]][meta_data[[label_col]] == resp_vars]) &
          util_empty(meta_data[[RECODE_CONTROL]][meta_data[[label_col]] == resp_vars])) {
        # If the recoding is not defined in the metadata, dataquieR will use the
        # most frequent category as 'cases', the remaining categories as
        # 'control'. If there were too few observations in the most frequent
        # category for the 'cases', we will include further categories until we
        # reach the lower limit specified by 'min_obs_in_cat' (but we stop it
        # before all categories are being combined into one group).
        ind_cases <- 1
        n_cases <- count_nom[1, 2]
        while (n_cases < min_obs_in_cat & max(ind_cases) < nrow(count_nom) - 1) {
          ind_cases <- seq_len(max(ind_cases) + 1)
          n_cases <- sum(count_nom[ind_cases, 2])
        }
        meta_data[[RECODE_CASES]][meta_data[[label_col]] == resp_vars] <-
          paste(count_nom[ind_cases, 1], collapse = " | ")
      }
      rvs_bin <- util_dichotomize(
        study_data = ds1[, resp_vars, drop = FALSE],
        meta_data = meta_data,
        label_col = label_col)
      rvs_bin_note <- attr(rvs_bin, "Dichotomization")[[resp_vars]]
      ds1[[resp_vars]] <- unlist(rvs_bin)
      ds1 <- ds1[complete.cases(ds1), ]
      # run margins function for binary response
      mar_out <- util_margins_bin(resp_vars = resp_vars,
                                  group_vars = group_vars,
                                  co_vars = co_vars,
                                  threshold_type = threshold_type,
                                  threshold_value = threshold_value,
                                  min_obs_in_subgroup = min_obs_in_subgroup,
                                  min_obs_in_cat = min_obs_in_cat,
                                  caption = rvs_bin_note,
                                  ds1 = ds1,
                                  label_col = label_col,
                                  adjusted_hint = adjusted_hint,
                                  title = title,
                                  sort_group_var_levels = sort_group_var_levels,
                                  include_numbers_in_figures =
                                    include_numbers_in_figures)
      obj1<- ggplot2::ggplot_build(mar_out$plot)
      obj1_data <- util_rbind(data_frames_list = obj1$data)
      min_value <- min(c(obj1_data$x,obj1_data$xintercept),  na.rm = TRUE)
      max_value <- max(c(obj1_data$x,obj1_data$xintercept),  na.rm = TRUE)
      range_values <- max_value - min_value
      no_char_y <- nchar(range_values)
      rm(obj1, obj1_data)
      type_plot <- "count_plot"

    } else { # more than two categories to be considered in the analysis
      # Rare categories will either be collapsed or discarded.
      if (any(count_nom$below_thresh)) {
        crit_lev_ind <- which(count_nom[, 2] < min_obs_in_cat)
        critical_levels <- count_nom[crit_lev_ind, 1]
        if (sum(count_nom[crit_lev_ind, 2]) < min_obs_in_cat) {
          util_message(paste0(c(
            "The following levels:", head(critical_levels, 100),
            if (length(critical_levels) > 100)  {", ..." }, "have <",
            min_obs_in_cat, " observations and will be discarded."
          ),
          collapse = " "
          ), applicability_problem = FALSE)
          levels(ds1[[resp_vars]])[
            which(levels(ds1[[resp_vars]]) %in% critical_levels)] <- NA
          ds1 <- ds1[!is.na(ds1[[resp_vars]]), ]
        } else {
          util_message(paste0(c(
            "The following levels:", head(critical_levels, 100),
            if (length(critical_levels) > 100)  {", ..." }, "have <",
            min_obs_in_cat, " observations and will be collapsed."
          ),
          collapse = " "
          ), applicability_problem = FALSE)
          new_lev <- "other"
          # ensure that the new category is not yet present
          if (new_lev %in% levels(ds1[[resp_vars]])) {
            new_lev <- "other (collapsed)"
          }
          while (new_lev %in% levels(ds1[[resp_vars]])) {
            new_lev <- paste0(
              "other collapsed_",
              paste0(sample(c(letters, LETTERS), replace = TRUE,
                            size = length(levels(ds1[[resp_vars]])) + 1),
                     collapse = ""))
          }
          levels(ds1[[resp_vars]])[
            which(levels(ds1[[resp_vars]]) %in% critical_levels)] <- new_lev
        }
      }
      # run margins function for response with more than two categories
      mar_out <- util_margins_nom(resp_vars = resp_vars,
                                  group_vars = group_vars,
                                  co_vars = co_vars,
                                  min_obs_in_subgroup = min_obs_in_subgroup,
                                  min_obs_in_cat = min_obs_in_cat,
                                  ds1 = ds1,
                                  label_col = label_col,
                                  adjusted_hint = adjusted_hint,
                                  title = title,
                                  sort_group_var_levels = sort_group_var_levels)
      obj1<- ggplot2::ggplot_build(mar_out$plot)
      n_groups <- max(obj1$data[[2]]$group) * nrow(count_nom)
      min_value <- min(c(obj1$data[[1]]$xmin, obj1$data[[1]]$xmax),  na.rm = TRUE)
      max_value <- max(c(obj1$data[[1]]$xmin, obj1$data[[1]]$xmax),  na.rm = TRUE)
      range_values <- max_value - min_value
      no_char_y <- nchar(round(range_values, digits = 2 ))
      rm(obj1)
      type_plot <- "rotated_plot"
    }
    ###3rd CASE: ORDINAL (2 possible results)
  } else if (var_scale == SCALE_LEVELS$ORDINAL) {
    # Ordinal response variables will either be analyzed by a linear model
    # or by mixed effects ordered logistic models.
    count_ord <- util_table_of_vct(ds1[[resp_vars]])
    if (!is.null(cut_off_linear_model_for_ord) &&
        all(count_ord[, 2] >= cut_off_linear_model_for_ord)) {
      orig_levels <- levels(ds1[[resp_vars]])
      names(orig_levels) <- seq_along(orig_levels) - 1
      ds1[[resp_vars]] <- as.numeric(ds1[[resp_vars]]) - 1

      mar_out <- util_margins_lm(resp_vars = resp_vars,
                                 group_vars = group_vars,
                                 co_vars = co_vars,
                                 threshold_type = threshold_type,
                                 threshold_value = threshold_value,
                                 min_obs_in_subgroup = min_obs_in_subgroup,
                                 ds1 = ds1,
                                 label_col = label_col,
                                 levels = orig_levels,
                                 adjusted_hint = adjusted_hint,
                                 title = title,
                                 sort_group_var_levels = sort_group_var_levels,
                                 include_numbers_in_figures =
                                   include_numbers_in_figures,
                                 n_violin_max = n_violin_max)

      obj1<- ggplot2::ggplot_build(mar_out$plot)
      obj1_data <- util_rbind(data_frames_list = obj1$data)
      min_value <- min(c(obj1_data$x,obj1_data$xintercept),  na.rm = TRUE)
      max_value <- max(c(obj1_data$x,obj1_data$xintercept),  na.rm = TRUE)
      range_values <- max_value - min_value
      a<- orig_levels
      names(a) <- NULL
      no_char_y <- max(nchar(a))
      rm(a)
      rm(obj1, obj1_data)
      type_plot <- "violin_plot"

    } else {
      title <- paste("Conditional modes of",
                     prep_get_labels(original_group_vars,
                                     item_level = meta_data,
                                     label_col = label_col,
                                     label_class = "LONG",
                                     resp_vars_match_label_col_only = TRUE),
                     "levels for",
                     prep_get_labels(original_resp_vars,
                                     item_level = meta_data,
                                     label_col = label_col,
                                     label_class = "LONG",
                                     resp_vars_match_label_col_only = TRUE))

      mar_out <- util_margins_ord(resp_vars = resp_vars,
                                  group_vars = group_vars,
                                  co_vars = co_vars,
                                  min_obs_in_subgroup = min_obs_in_subgroup,
                                  min_subgroups = 4, # specific requirement from 'ordinal'
                                  ds1 = ds1,
                                  label_col = label_col,
                                  adjusted_hint = adjusted_hint,
                                  title = title,
                                  sort_group_var_levels = sort_group_var_levels)

      n_groups <- length(mar_out$plot_data$group)
      no_char_x <- max(nchar(as.character(mar_out$plot_data$group)))
      min_value <- min(mar_out$plot_data$LCL,  na.rm = TRUE)
      max_value <- max(mar_out$plot_data$UCL,  na.rm = TRUE)
      range_values <- max_value - min_value
      no_char_y <- nchar(round(range_values, digits = 3))
      type_plot <- "rotated_plot"
    }
    ###4th CASE: INTEGER categories 2 to 20 -- count data
  } else if (var_dtype == DATA_TYPES$INTEGER &
             var_prop$NCategory > 2 & var_prop$NCategory <= 20 &
             # TODO: Count data can exceed 20, of course! How do we identify count
             # data here? Maybe include a pre-test to choose between poisson and
             # linear regression? Or also consider negative binomial regression here?
             !var_prop$AnyNegative) {
    # TODO: The website states 15 as cut-off, instead of 20. Which one is the
    # preferred value??
    mar_out <- util_margins_poi(resp_vars = resp_vars,
                                group_vars = group_vars,
                                co_vars = co_vars,
                                threshold_type = threshold_type,
                                threshold_value = threshold_value,
                                min_obs_in_subgroup = min_obs_in_subgroup,
                                ds1 = ds1,
                                label_col = label_col,
                                adjusted_hint = adjusted_hint,
                                title = title,
                                sort_group_var_levels = sort_group_var_levels,
                                include_numbers_in_figures = include_numbers_in_figures)
    obj1<- ggplot2::ggplot_build(mar_out$plot)
    obj1_data <- util_rbind(data_frames_list = obj1$data)
    min_value <- min(c(obj1_data$x,obj1_data$xintercept),  na.rm = TRUE)
    max_value <- max(c(obj1_data$x,obj1_data$xintercept),  na.rm = TRUE)
    range_values <- max_value - min_value
    no_char_y <- nchar(range_values)
    rm(obj1, obj1_data)
    type_plot <- "count_plot"

  } else if (var_scale %in% c(SCALE_LEVELS$RATIO, SCALE_LEVELS$INTERVAL)) {
    mar_out <- util_margins_lm(resp_vars = resp_vars,
                               group_vars = group_vars,
                               co_vars = co_vars,
                               threshold_type = threshold_type,
                               threshold_value = threshold_value,
                               min_obs_in_subgroup = min_obs_in_subgroup,
                               ds1 = ds1,
                               label_col = label_col,
                               adjusted_hint = adjusted_hint,
                               title = title,
                               sort_group_var_levels = sort_group_var_levels,
                               include_numbers_in_figures =
                                 include_numbers_in_figures,
                               n_violin_max = n_violin_max)
    obj1<- ggplot2::ggplot_build(mar_out$plot)
    obj1_data <- util_rbind(data_frames_list = obj1$data)
    min_value <- min(c(obj1_data$x,obj1_data$xintercept),  na.rm = TRUE)
    max_value <- max(c(obj1_data$x,obj1_data$xintercept),  na.rm = TRUE)
    range_values <- max_value - min_value
    no_char_y <- nchar(range_values)
    rm(obj1, obj1_data)
    type_plot <- "violin_plot"
  } else  {
    util_error("No suitable method implemented yet, sorry.")
  }


  # finalize output ------------------------------------------------------------
  res_df <- mar_out$plot_data
  res_plot <- mar_out$plot

  SummaryTable <- data.frame(
    Variables = resp_vars,
    FLG_acc_ud_loc = as.numeric(any(res_df$GRADING > 0)),
    PCT_acc_ud_loc = round(sum(res_df$GRADING == 1)/nrow(res_df)*100,
                           digits = 2))

  SummaryData <- cbind.data.frame(
    Variables = resp_vars,
    res_df
  )

  #modify number of decimal places

  if ("sample_size" %in% colnames(SummaryData)) {
    SummaryData$sample_size <-
      util_round_to_decimal_places(SummaryData$sample_size)
  }

  SummaryData$df <- NULL
  SummaryData$threshold <- NULL
  SummaryData$overall <- NULL
  SummaryData$GRADING <- NULL

  SummaryData$margins <- util_round_to_decimal_places(SummaryData$margins)
  SummaryData$LCL <- util_round_to_decimal_places(SummaryData$LCL)
  SummaryData$UCL <- util_round_to_decimal_places(SummaryData$UCL)
  SummaryData$CL <- paste0("[", format(SummaryData$LCL), "; ",
                           format(SummaryData$UCL), "]")
  SummaryData$LCL <- NULL
  SummaryData$UCL <- NULL
  if ("SE" %in% colnames(SummaryData)) {
    SummaryData$SE <- util_round_to_decimal_places(SummaryData$SE)
  }

  colnames(SummaryData)[which(colnames(SummaryData) == "sample_size")] <- "n"

  attr(SummaryData, "description") <- character(0)
  attr(SummaryData, "description")[[group_vars]] <- "Group: Observer/Device/..."
  if (resp_vars %in% colnames(SummaryData)) { # for nominal variables
    attr(SummaryData, "description")[[resp_vars]] <- "Categories of the Outcome"
    attr(SummaryData, "description")[["margins"]] <- "Probability for the Group"
  } else {
    attr(SummaryData, "description")[["margins"]] <- "Mean for the Group"
  }
  attr(SummaryData, "description")[["SE"]] <- "Standard error for the Group"
  attr(SummaryData, "description")[["n"]] <-
    "Number of Observations of the Group"
  attr(SummaryData, "description")[["CL"]] <-
    "Confidence Interval for the Group"


  SummaryPlot <- util_set_size(res_plot, width_em = 25 +
                  1.2 * length(unique(ds1[[group_vars]])),
                height_em = 25)

  #Information for sizing
#  obj1 <- ggplot2::ggplot_build(res_plot)
#  obj1_data <- util_rbind(data_frames_list = obj1$data)

  if(!exists("n_groups")) {
    n_groups <- nrow(SummaryData)
  }



#  min_value <- min(c(obj1_data$x,obj1_data$xintercept),  na.rm = TRUE)
#  max_value <- max(c(obj1_data$x,obj1_data$xintercept),  na.rm = TRUE)
#  range_values <- max_value - min_value
#  if ((max(obj1_data$x,na.rm = TRUE) - min(obj1_data$x,na.rm = TRUE)) < 2 ) {
#    type_plot <- "count_plot"
#  } else {
#    type_plot <- "violin_plot"
#  }
#  rm(obj1, obj1_data)

if(!exists("no_char_x")){
  no_char_x <- max(nchar(as.character(SummaryData[[group_vars]])))
}


  return(util_attach_attr(list(
    ResultData = SummaryData,
    SummaryTable = SummaryTable,
    SummaryPlot = SummaryPlot),
    sizing_hints = list(figure_type_id = "marg_plot",
                       n_groups = n_groups,
                       no_char_x = no_char_x,
                       no_char_y = no_char_y,
                       type_plot = type_plot
                       ),
    as_plotly = "util_as_plotly_acc_margins"))
}


#' @family plotly_shims
#' @concept plotly_shims
#' @keywords internal
util_as_plotly_acc_margins <- function(res, ...) {
  #remove classes for plotly to work properly
  res$SummaryPlot <- util_remove_dataquieR_result_class(res$SummaryPlot)
  # use res$SummaryPlot, not res_plot to avoid depending on the enclosure
  # of the result, that may contain study data.
  util_ensure_suggested("plotly")
  if (inherits(res$SummaryPlot, "patchwork")) {
    # extract estimates on size of the plot from the patchwork object
    # obtain relative width of the single elements of the plot
    rel_w <- res$SummaryPlot$patches$layout$widths /
      sum(res$SummaryPlot$patches$layout$widths, na.rm = TRUE)
    # extract the violin plots
    py1 <- try(plotly::ggplotly(res$SummaryPlot[[1]],
                                ...), silent = TRUE)
    if (identical(py1$x$data[[2]]$mode, "lines+markers")) { # no violins were created because of too many observers, see dataquieR.max_group_var_levels_with_violins
      py1$x$data[[1]]$mode <- NULL # suppress a warning on print (https://github.com/plotly/plotly.R/issues/2242)
    } else {
      py1$x$data[[2]]$mode <- NULL # suppress a warning on print (https://github.com/plotly/plotly.R/issues/2242)
    }

    # extract the overall distribution plot
    py2 <- try(plotly::ggplotly(res$SummaryPlot[[2]],
                                ...), silent = TRUE)
    # check if both are plotly objects
    util_stop_if_not(!inherits(py1, "try-error"))
    util_stop_if_not(!inherits(py2, "try-error"))
    # https://plotly.com/r/subplots/#subplots-with-shared-yaxes


    #  summary_ds<-as.data.frame(dplyr::summarize(dplyr::group_by_at(ds1[, c(resp_vars, group_vars), drop = FALSE],
    #                                                               group_vars), samplesize = dplyr::n()))


    # py1<- plotly::ggplotly(py1, tooltip = paste("Sample size:",
    #                                            as.data.frame(dplyr::summarize(dplyr::group_by_at(ds1[, c(resp_vars, group_vars), drop = FALSE],
    #                                                                                                             group_vars), samplesize = dplyr::n()))[,2]))
    #py2<- plotly::layout(py2)

    target_layers <-
      which(lapply(lapply(py1$x$data, `[[`, "marker"), `[[`, "symbol") == "diamond")

    hovertexts <- lapply(py1$x$data, `[[`, "hovertext")
    if (!all(vapply(hovertexts, is.null, logical(1)))) {
      hovertexts_matching_sample_size <- lapply(hovertexts, grepl, pattern = "sample_size: ", fixed = TRUE)
      hovertexts_matching_sample_size_with_length_nr_groups <-
        vapply(hovertexts_matching_sample_size, function(x) length(x) == length(unique(res$SummaryPlot[[1]]$data[[2]])) && all(x, na.rm = TRUE), FUN.VALUE = logical(1))
      layer_with_sample_size <- which(hovertexts_matching_sample_size_with_length_nr_groups)
      if (length(layer_with_sample_size) != 1) {
        util_warning(c("Internal error: unexpected number of",
                       "layer_with_sample_size. Sorry, please report to us"))
      } else {
        hovertexts_with_hovertexts <- py1$x$data[[layer_with_sample_size]]$hovertext

        xpositions_with_hovertexts <- py1$x$data[[layer_with_sample_size]]$x

        for (tl in target_layers) {
          # amend texts for hover-texts, matching by x position.

          py1$x$data[[tl]]$text <- paste0(
            py1$x$data[[tl]]$text,
            "<br />",
            hovertexts_with_hovertexts[match(py1$x$data[[tl]]$x,
                                             xpositions_with_hovertexts)]
          )
        }
      }
    }

    target_layer_outliers <- which(!(unlist(lapply(lapply(lapply(py1$x$data, `[[`, "marker"), `[[`, "outliercolor"), is.null))))
    if (length(target_layer_outliers) == 1) {
      # https://github.com/plotly/plotly.R/issues/1114
      py1$x$data[[target_layer_outliers]]$marker$outliercolor <-
        py1$x$data[[target_layer_outliers]]$line$color
      py1$x$data[[target_layer_outliers]]$marker$line$color <-
        py1$x$data[[target_layer_outliers]]$line$color
      py1$x$data[[target_layer_outliers]]$marker$opacity <- 0.5
    }

    note <- ""
    try(note <- res$SummaryPlot$patches$annotation$caption, silent = TRUE)
    if (length(note) != 1) {
      note <- ""
    }

    # recombine plots
    suppressMessages(force(plotly::layout(plotly::subplot(py1,
                                                          py2,
                                                          nrows =
                                                            res$SummaryPlot$patches$layout$nrow,
                                                          shareY = TRUE, #all plots use the same y axes
                                                          widths = #define relative width
                                                            rel_w),
                                          title = list(text = #get the overall title from patch
                                                         paste0(
                                                           res$SummaryPlot$patches$annotation$title,
                                                           "<br /><sub>",
                                                           res$SummaryPlot$patches$annotation$subtitle,
                                                           "</sub>"
                                                         )),
                                          font = list(size = 12),
                                          margin = 0.01,
                                          annotations = list(
                                            list(
                                              x = 1,
                                              y = 1,
                                              yshift = 24, # px
                                              text = note,
                                              align = 'right',
                                              valign = 'top',
                                              xref = 'paper',
                                              yref = 'paper',
                                              xanchor = 'right',
                                              yanchor = 'top',
                                              showarrow = FALSE,
                                              font = list(size = 8)
                                            )
                                          ))))

  } else {
    util_plot_figure_plotly(res$SummaryPlot, attr(res, "sizing_hints"))
  }
}
