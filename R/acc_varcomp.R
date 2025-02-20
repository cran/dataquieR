#' Utility function to compute model-based ICC depending on the (statistical) data type
#'
#' @description
#' This function is still under construction. It is designed to run for any
#' statistical data type as follows:
#'   - Variables with only two distinct values will be modeled by mixed effects
#'     logistic regression.
#'   - Nominal variables will be transformed to binary variables. This can be
#'     user-specified using the metadata columns `RECODE_CASES` and/or
#'     `RECODE_CONTROL`. Otherwise, the most frequent category will be assigned
#'     to cases and the remaining categories to control. As for other binary
#'     variables, the ICC will be computed using a mixed effects logistic
#'     regression.
#'   - Ordinal variables will be analyzed by linear mixed effects models, if
#'     every level of the variable has at least as many observations as
#'     specified in the argument `cut_off_linear_model_for_ord`. Otherwise, the
#'     data will be modeled by a mixed effects ordered regression, if the
#'     package `ordinal` is available.
#'   - Metric variables with integer values are analyzed by linear mixed
#'     effects models.
#'   - For variables with data type `float`, the existing implementation
#'     `acc_varcomp` is called, which also uses linear mixed effects models.
#'
#' [Indicator]
#'
#' @export
#'
#' @inheritParams .template_function_indicator
#'
#' @param resp_vars  [variable] the name of the measurement variable
#' @param group_vars [variable] the name of the examiner, device or
#'                              reader variable
#' @param co_vars    [variable list] a vector of covariables, e.g. age and sex,
#'                              for adjustment
#' @param min_obs_in_subgroup [integer] from=0. This optional argument specifies
#'                              the minimum number of observations that is
#'                              required to include a subgroup (level) of the
#'                              `group_var` in the analysis. Subgroups with less
#'                              observations are excluded.
#' @param min_subgroups [integer] from=0. This optional argument specifies
#'                              the minimum number of subgroups (level) of the
#'                              `group_var` that is required to run the
#'                              analysis. If there are less subgroups, the
#'                              analysis is not conducted.
#' @param cut_off_linear_model_for_ord [integer] from=0. This optional argument
#'                              specifies the minimum number of observations for
#'                              individual levels of an ordinal outcome
#'                              (`resp_var`) that is required to run a linear
#'                              mixed effects model instead of a mixed effects
#'                              ordered regression (i.e., a cut-off value above
#'                              which linear models are considered a good
#'                              approximation). The argument can be set to
#'                              `NULL` if ordered regression models are
#'                              preferred for ordinal data in any case.
#'
#' @return The function returns two data frames, 'SummaryTable' and
#'         'SummaryData', that differ only in the names of the columns.
#'
#' @details
#' Not yet described
#'
acc_varcomp <- function(resp_vars = NULL,
                        group_vars = NULL,
                        co_vars = NULL,
                        study_data,
                        label_col,
                        item_level = "item_level",
                        min_obs_in_subgroup = 10,
                        min_subgroups = 5,
                        cut_off_linear_model_for_ord = 10,
                        meta_data = item_level,
                        meta_data_v2) {
  # preps and checks -----------------------------------------------------------
  util_maybe_load_meta_data_v2()
  threshold_value <- 0.05

  prep_prepare_dataframes(.replace_hard_limits = TRUE,
                          .apply_factor_metadata = TRUE)

  util_correct_variable_use("resp_vars",
                            allow_all_obs_na = FALSE,
                            need_scale = "!na")

  util_correct_variable_use("group_vars",
                            allow_all_obs_na = FALSE,
                            need_type = "!float",
                            need_scale = "nominal | ordinal")

  util_correct_variable_use("co_vars",
                            allow_na = TRUE,
                            allow_more_than_one = TRUE,
                            allow_null = TRUE)
  co_vars <- co_vars[which(!util_empty(co_vars))]
  if (is.null(co_vars)) {
    co_vars <- character(0)
  }

  util_expect_scalar(min_obs_in_subgroup,
                     error_message = "min_obs_in_subgroup needs to be integer > 0",
                     check_type = util_is_numeric_in(min = 0,
                                                     whole_num = TRUE,
                                                     finite = TRUE))
  util_expect_scalar(min_subgroups,
                     error_message = "min_subgroups needs to be integer > 0",
                     check_type = util_is_numeric_in(min = 0,
                                                     whole_num = TRUE,
                                                     finite = TRUE))

  util_expect_scalar(cut_off_linear_model_for_ord,
                     error_message =
                       "cut_off_linear_model_for_ord needs to be integer > 0",
                     allow_null = TRUE,
                     check_type = util_is_numeric_in(min = 0,
                                                     whole_num = TRUE,
                                                     finite = TRUE))

  ds1 <- ds1[, c(resp_vars, group_vars, co_vars), drop = FALSE]
  ds1 <- ds1[complete.cases(ds1), , drop = FALSE]

  # ensure that the grouping variable has the specified minimum number of levels
  check_df <- util_table_of_vct(ds1[[group_vars]])
  if (length(check_df[, 1]) < min_subgroups) {
    util_error("%d < %d levels in %s. Will not compute ICCs for %s.",
               length(check_df[, 1]),
               min_subgroups,
               dQuote(group_vars),
               dQuote(resp_vars),
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }

  # ensure that included levels of the grouping variable have the specified
  # minimum number of observations
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

  rvs_bin_note <- NULL

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

  # ensure that the response variable is not constant
  var_prop <- util_dist_selection(ds1[, resp_vars, drop = FALSE])
  if (var_prop$NDistinct < 2) {
    util_error("The response variable is constant after data preparation.",
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }

  # compute model-based ICC depending on the statistical data type -------------
  # prepare model formula
  if (length(co_vars) == 0) {
    .co_vars <- "1"
  } else {
    .co_vars <- util_bQuote(co_vars)
  }
  fmla <- as.formula(paste0(
    util_bQuote(resp_vars), " ~ ",
    paste0(c(.co_vars, paste0("(1|", util_bQuote(group_vars), ")")),
           collapse = "+")))

  var_scale <- meta_data[[SCALE_LEVEL]][meta_data[[label_col]] == resp_vars]
  var_dtype <- meta_data[[DATA_TYPE]][meta_data[[label_col]] == resp_vars]


  ## if loop
  if (var_prop$NDistinct == 1) {
    util_error("The response variable is constant.",
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  } else if (var_prop$NDistinct == 2 && (
    var_scale %in% c("ordinal", "ratio", "interval"))) { ## binary response ----------------------------
    check_df <- util_table_of_vct(ds1[[resp_vars]])
    if (any(check_df$Freq < min_obs_in_subgroup)) {
      util_error("Too few observations with different values in %s.",
                 dQuote(resp_vars),
                 applicability_problem = TRUE)
    }
    # code the most frequent value as 1, the other as 0
    check_df <- check_df[order(check_df$Freq, decreasing = TRUE), ]
    ds1[[resp_vars]] <- ifelse(ds1[[resp_vars]] == check_df$Var1[1], 1, 0)
    rvs_bin_note <- paste(
      paste("Cases (1):", check_df[1, 1]),
      paste("Control (0):", check_df[2, 1]),
      sep = ". ")
    # mixed effects logistic regression
    fit1 <- try(lme4::glmer(formula = fmla, data = ds1,
                            family = binomial,
                            control = lme4::glmerControl(optimizer = "bobyqa"),
                            nAGQ = 10))
    icc <- NA
    if (!inherits(fit1, "try-error")) {
      # extract variance of random effects
      v_tab <- as.data.frame(lme4::VarCorr(fit1))
      # calculate icc
      icc <- round(v_tab$vcov / (v_tab$vcov + pi^2/3), 3)
    }
    check_gr <- util_table_of_vct(ds1[[group_vars]])
    res_icc <- data.frame(
      Variables = resp_vars,  #TODO: add the information somewhere else
      #   Category = check_df$Var1[1],
      Object = group_vars,
      Model.Call = Reduce(paste, deparse(fmla)),
      ICC = icc,
      Class.Number = nrow(check_gr),
      Mean.Class.Size = mean(check_gr$Freq),
      Median.Class.Size = median(check_gr$Freq),
      Min.Class.Size = min(check_gr$Freq),
      Max.Class.Size = max(check_gr$Freq),
      convergence.problem = inherits(fit1, "try-error")
    )
  } else if (var_scale == SCALE_LEVELS$NOMINAL) { ## nominal response ----------
    count_nom <- util_table_of_vct(ds1[[resp_vars]])
    count_nom <- count_nom[order(count_nom[, 2], decreasing = TRUE), ]
    min_obs_in_cat <- 2 * length(levels(ds1[[group_vars]]))
    count_nom$below_thresh <- count_nom[, 2] < min_obs_in_cat
    # Nominal response variables will be transformed to binary variables.
    if (!(RECODE_CASES %in% colnames(meta_data))) {
      meta_data[[RECODE_CASES]] <- ""
    }
    if (!(RECODE_CONTROL %in% colnames(meta_data))) {
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
    # ensure that the response variable is not constant
    var_prop <- util_dist_selection(ds1[, resp_vars, drop = FALSE])
    if (var_prop$NDistinct < 2) {
      util_error("The response variable is constant (after data preparation).",
                 applicability_problem = TRUE,
                 intrinsic_applicability_problem = TRUE)
    }
    # ensure that the response variable is binary
    if (var_prop$NDistinct != 2) {
      util_error("The response variable is not binary.",
                 applicability_problem = TRUE,
                 intrinsic_applicability_problem = TRUE)
    }
    # ensure that there are enough observations for the model
    # count_bin <- util_table_of_vct(ds1[[resp_vars]])
    # if (any(count_bin[, 2] < min_obs_in_cat)) {
    #   util_error("Not enough observations per category (after data preparation).")
    # }
    count_bin <- util_table_of_vct(ds1[[group_vars]])
    if (any(count_bin[, 2] < min_obs_in_subgroup)) {
      util_error("Not enough observations per group (after data preparation).")
    }
    fit1 <- try(lme4::glmer(formula = fmla, data = ds1,
                            family = "binomial",
                            control = lme4::glmerControl(optimizer = "bobyqa"),
                            nAGQ = 10))
    icc <- NA
    if (!inherits(fit1, "try-error")) {
      # extract variance of random effects
      v_tab <- as.data.frame(lme4::VarCorr(fit1))
      # calculate icc
      icc <- round(v_tab$vcov / (v_tab$vcov + pi^2/3), 3)
    }
    res_icc <- data.frame(
      Variables = resp_vars,
      Object = group_vars,
      Model.Call = Reduce(paste, deparse(fmla)),
      ICC = icc,
      Class.Number = nrow(count_bin),
      Mean.Class.Size = mean(count_bin$Freq),
      Median.Class.Size = median(count_bin$Freq),
      Min.Class.Size = min(count_bin$Freq),
      Max.Class.Size = max(count_bin$Freq),
      convergence.problem = inherits(fit1, "try-error")
    )
  } else if (var_scale == SCALE_LEVELS$ORDINAL && var_prop$NDistinct > 2) { ## ordinal response ----------
    # Ordinal response variables will either be analyzed as integer responses
    # or using the 'ordinal' package.
    count_ord <- util_table_of_vct(ds1[[resp_vars]])
    if (!is.null(cut_off_linear_model_for_ord) &&
        all(count_ord[, 2] >= cut_off_linear_model_for_ord)) {
      ds1[[resp_vars]] <- as.numeric(ds1[[resp_vars]]) - 1
      fit1 <- try(lme4::lmer(formula = fmla, data = ds1, REML = TRUE))
      icc <- NA
      if (!inherits(fit1, "try-error")) {
        # extract variance
        v_tab <- as.data.frame(lme4::VarCorr(fit1, comp = "Variance"))
        v_tab <- v_tab$vcov
        # calculate icc
        icc <- round(v_tab[1] / (v_tab[1] + v_tab[2]), 3)
      }
      check_gr <- util_table_of_vct(ds1[[group_vars]])
      res_icc <- data.frame(
        Variables = resp_vars,
        Object = group_vars,
        Model.Call = Reduce(paste, deparse(fmla)),
        ICC = icc,
        Class.Number = nrow(check_gr),
        Mean.Class.Size = mean(check_gr$Freq),
        Median.Class.Size = median(check_gr$Freq),
        Min.Class.Size = min(check_gr$Freq),
        Max.Class.Size = max(check_gr$Freq),
        convergence.problem = inherits(fit1, "try-error")
      )
    } else {
      util_ensure_suggested("ordinal")
      fit1 <- try(ordinal::clmm(formula = fmla, data = ds1,
                                Hess = TRUE, nAGQ = 10))
      icc <- NA
      if (!inherits(fit1, "try-error")) {
        # extract variance of random effects (intercept)
        re_var <- as.numeric(lme4::VarCorr(fit1, comp = "Variance"))
        # calculate icc
        icc <- round(re_var / (re_var + pi^2/3), 3)
      }
      check_gr <- util_table_of_vct(ds1[[group_vars]])
      res_icc <- data.frame(
        Variables = resp_vars,
        Object = group_vars,
        Model.Call = Reduce(paste, deparse(fmla)),
        ICC = icc,
        Class.Number = nrow(check_gr),
        Mean.Class.Size = mean(check_gr$Freq),
        Median.Class.Size = median(check_gr$Freq),
        Min.Class.Size = min(check_gr$Freq),
        Max.Class.Size = max(check_gr$Freq),
        convergence.problem = inherits(fit1, "try-error")
      )
    }
  } else if (var_scale == SCALE_LEVELS$RATIO && var_prop$NDistinct > 2) {
    if( var_dtype == DATA_TYPES$INTEGER) { #TODO: extract code from here
      ## integer values, variable on interval or ratio scale ---------------------
      # linear model with random effects
      fit1 <- try(lme4::lmer(formula = fmla, data = ds1, REML = TRUE))
      # other options:
      # - generalized linear model with random effects using the poisson
      # distribution
      # fit2 <- try(lme4::glmer(formula = fmla, data = ds1, family = "poisson",
      #                         nAGQ = 10))
      # - generalized linear model with random effects using the
      # negative binomial distribution
      # fit3 <- try(lme4::glmer.nb(formula = fmla, data = ds1, nAGQ = 10))
      icc <- NA
      if (!inherits(fit1, "try-error")) {
        # extract variance
        v_tab <- as.data.frame(lme4::VarCorr(fit1, comp = "Variance"))
        v_tab <- v_tab$vcov
        # calculate icc
        icc <- round(v_tab[1] / (v_tab[1] + v_tab[2]), 3)
      }
      check_gr <- util_table_of_vct(ds1[[group_vars]])
      res_icc <- data.frame(
        Variables = resp_vars,
        Object = group_vars,
        Model.Call = Reduce(paste, deparse(fmla)),
        ICC = icc,
        Class.Number = nrow(check_gr),
        Mean.Class.Size = mean(check_gr$Freq),
        Median.Class.Size = median(check_gr$Freq),
        Min.Class.Size = min(check_gr$Freq),
        Max.Class.Size = max(check_gr$Freq),
        convergence.problem = inherits(fit1, "try-error")
      )
    } else if(var_dtype == DATA_TYPES$FLOAT){
      if (length(co_vars) == 1 && co_vars == "1") {
        co_vars <- NULL
      }
      res_icc <- util_acc_varcomp(resp_vars = resp_vars, group_vars = group_vars,
                                  co_vars = co_vars,
                                  min_obs_in_subgroup = min_obs_in_subgroup,
                                  min_subgroups = min_subgroups,
                                  label_col = label_col, threshold_value = threshold_value,
                                  study_data = study_data, meta_data = meta_data)
    } else {
      util_error("No method implemented for scale level ratio and data types string or datetime, sorry.",
                 applicability_problem = TRUE)
    }
  } else if (var_scale == SCALE_LEVELS$INTERVAL && var_prop$NDistinct > 2){
    if( var_dtype == DATA_TYPES$INTEGER) {
      ## integer values, variable on interval or ratio scale ---------------------
      # linear model with random effects
      fit1 <- try(lme4::lmer(formula = fmla, data = ds1, REML = TRUE))
      # other options:
      # - generalized linear model with random effects using the poisson
      # distribution
      # fit2 <- try(lme4::glmer(formula = fmla, data = ds1, family = "poisson",
      #                         nAGQ = 10))
      # - generalized linear model with random effects using the
      # negative binomial distribution
      # fit3 <- try(lme4::glmer.nb(formula = fmla, data = ds1, nAGQ = 10))
      icc <- NA
      if (!inherits(fit1, "try-error")) {
        # extract variance
        v_tab <- as.data.frame(lme4::VarCorr(fit1, comp = "Variance"))
        v_tab <- v_tab$vcov
        # calculate icc
        icc <- round(v_tab[1] / (v_tab[1] + v_tab[2]), 3)
      }
      check_gr <- util_table_of_vct(ds1[[group_vars]])
      res_icc <- data.frame(
        Variables = resp_vars,
        Object = group_vars,
        Model.Call = Reduce(paste, deparse(fmla)),
        ICC = icc,
        Class.Number = nrow(check_gr),
        Mean.Class.Size = mean(check_gr$Freq),
        Median.Class.Size = median(check_gr$Freq),
        Min.Class.Size = min(check_gr$Freq),
        Max.Class.Size = max(check_gr$Freq),
        convergence.problem = inherits(fit1, "try-error")
      )
    } else if(var_dtype == DATA_TYPES$FLOAT){
      if (length(co_vars) == 1 && co_vars == "1") {
        co_vars <- NULL
      }
      res_icc <- util_acc_varcomp(resp_vars = resp_vars, group_vars = group_vars,
                                  co_vars = co_vars,
                                  min_obs_in_subgroup = min_obs_in_subgroup,
                                  min_subgroups = min_subgroups,
                                  label_col = label_col, threshold_value = threshold_value,
                                  study_data = study_data, meta_data = meta_data)
    } else {
      util_error("No method implemented for scale level interval and data types string or datetime, sorry.",
                 applicability_problem = TRUE)
    }
  }

  if (is.data.frame(res_icc)) {
    res_icc$ICC <- util_round_to_decimal_places(res_icc$ICC)
    res_icc$Mean.Class.Size <-
      util_round_to_decimal_places(res_icc$Mean.Class.Size)
    res_icc$Median.Class.Size <-
      util_round_to_decimal_places(res_icc$Median.Class.Size, 1)

    sumtab <- res_icc
    sumtab[["GRADING"]] <- as.numeric(threshold_value <= sumtab$ICC)
    names(sumtab)[names(sumtab) == "ICC"] <- "ICC_acc_ud_loc"

    SummaryData <- res_icc

    SummaryData <- SummaryData[, setdiff(colnames(SummaryData), c("Model.Call",
                                                                  "GRADING"))]

    # TODO: How to return rvs_bin_note?
    return(list("SummaryTable" = sumtab,
                "SummaryData" = SummaryData))
  } else { # output created by acc_varcomp
    return(res_icc)
  }
}
