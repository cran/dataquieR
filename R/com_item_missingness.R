#' Summarize missingness columnwise (in variable)
#'
#' @description
#' Item-Missingness (also referred to as item nonresponse (De Leeuw et al.
#' 2003)) describes the missingness of single values, e.g. blanks or empty data
#' cells in a data set. Item-Missingness occurs for example in case a respondent
#' does not provide information for a certain question, a question is overlooked
#' by accident, a programming failure occurs or a provided answer were missed
#' while entering the data.
#'
#' [Indicator]
#'
#' @details
#' # ALGORITHM OF THIS IMPLEMENTATION:
#'
#'  - Lists of missing codes and, if applicable, jump codes are selected from
#'    the metadata
#'  - The no. of system missings (NA) in each variable is calculated
#'  - The no. of used missing codes is calculated for each variable
#'  - The no. of used jump codes is calculated for each variable
#'  - Two result dataframes (1: on the level of observations, 2: a summary for
#'    each variable) are generated
#'  - *OPTIONAL:* if `show_causes` is selected, one summary plot for all
#'                `resp_vars` is provided
#'
#' @param resp_vars [variable list] the name of the measurement variables
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param show_causes [logical] if TRUE, then the distribution of missing codes
#'                              is shown
#' @param cause_label_df [data.frame] missing code table. If missing codes have
#'                                    labels the respective data frame can be
#'                                    specified here or in the metadata as
#'                                    assignments, see [cause_label_df]
#' @param include_sysmiss [logical] Optional, if TRUE system missingness (NAs)
#'                                  is evaluated in the summary plot
#' @param threshold_value [numeric] from=0 to=100. a numerical value ranging
#'                                                 from 0-100
#' @param suppressWarnings [logical] warn about consistency issues with missing
#'                                   and jump lists
#' @param assume_consistent_codes [logical] if TRUE and no labels are given and
#'                                          the same missing/jump code is used
#'                                          for more than one variable, the
#'                                          labels assigned for this code are
#'                                          treated as being be the same for
#'                                          all variables.
#' @param expand_codes [logical] if TRUE, code labels are copied from other
#'                               variables, if the code is the same and the
#'                               label is set somewhere
#'@param drop_levels [logical] if TRUE, do not display unused missing codes in
#'                             the figure legend.
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
#' @param pretty_print [logical] deprecated. If you want to have a human
#'                               readable output, use `SummaryData` instead
#'                               of `SummaryTable`
#'
#' @return a list with:
#'   - `SummaryTable`: data frame about item missingness per response variable
#'   - `SummaryData`: data frame about item missingness per response variable
#'                    formatted for user
#'   - `SummaryPlot`: ggplot2 heatmap plot, if show_causes was TRUE
#'   - `ReportSummaryTable`: data frame underlying `SummaryPlot`
#'
#' @export
#' @importFrom ggplot2 ggplot facet_wrap geom_bar theme_minimal theme annotate
#'                     scale_fill_gradientn theme element_blank
#' @seealso
#' [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_com_impl_item_missingness.html
#' )
com_item_missingness  <- function(study_data,
                                  meta_data,
                                  resp_vars = NULL,
                                  label_col,
                                  show_causes = TRUE,
                                  cause_label_df,
                                  include_sysmiss = TRUE,
                                  threshold_value,
                                  suppressWarnings = FALSE,
                                  assume_consistent_codes = TRUE,
                                  expand_codes = assume_consistent_codes,
                                  drop_levels = TRUE,
                                  expected_observations = c("HIERARCHY",
                                                            "ALL",
                                                            "SEGMENT"),
                                  pretty_print = lifecycle::deprecated()) {

  if (lifecycle::is_present(pretty_print)) {

    # Signal the deprecation to the user
    lifecycle::deprecate_stop("2.1.0",
                              "dataquieR::com_item_missingness(pretty_print = 'FALSE')",
                              "dataquieR::com_item_missingness()",
                              details = c("Replace as follows: dataquieR::com_item_missingness(pretty_print = FALSE)$SummaryTable --> dataquieR::com_item_missingness()$SummaryTable",
                                          "dataquieR::com_item_missingness(pretty_print = TRUE)$SummaryTable --> dataquieR::com_item_missingness()$SummaryData",
                                          "dataquieR::com_item_missingness()$SummaryTable --> dataquieR::com_item_missingness()$SummaryData"))

  }
  expected_observations_missing <- missing(expected_observations)
  util_expect_scalar(expected_observations, allow_more_than_one = TRUE)
  expected_observations <- match.arg(expected_observations)
  util_expect_scalar(expected_observations)

  util_expect_scalar(assume_consistent_codes, check_type = is.logical)
  util_expect_scalar(include_sysmiss, check_type = is.logical)
  util_expect_scalar(suppressWarnings, check_type = is.logical)
  util_expect_scalar(drop_levels, check_type = is.logical)

  study_data <- util_cast_off(study_data, "study_data", TRUE)
  .sd <- study_data
  prep_prepare_dataframes(.replace_missings = FALSE)
  study_data <- .sd

  if (missing(threshold_value)) {
    if (!.called_in_pipeline) util_message(
      c("The mandatory argument threshold_value was not",
        "defined and is set to the default of 90%%."),
      applicability_problem = TRUE)
    threshold_value <- 90
  }
  .threshold_value <- suppressWarnings(as.numeric(threshold_value))
  if (is.na(.threshold_value)) {
    util_message(
      c("Could not convert threshold_value %s to a number.",
        "Set to default value 90%%."),
      dQuote(as.character(threshold_value)),
      applicability_problem = TRUE
    )
    threshold_value <- 90
  } else {
    threshold_value <- .threshold_value
  }


  if (!missing(show_causes) &&
      length(show_causes) == 1 &&
      is.logical(show_causes) &&
      !show_causes) {
    util_warning(c("The argument %s has been deprecated. It will be ignored",
                   "and in a future version be removed."),
                 dQuote("show_causes"))
  }

  if (!missing(cause_label_df)) {
    util_expect_data_frame(cause_label_df, c("CODE_VALUE", "CODE_LABEL"))
    util_warning(c("The argument %s has been deprecated. It will be",
                   "in a future version be removed."),
                 dQuote("cause_label_df"))
    meta_data <-
      prep_add_cause_label_df(meta_data = meta_data,
                              cause_label_df = cause_label_df,
                              label_col = label_col,
                              assume_consistent_codes = assume_consistent_codes)
  }

  if (!suppressWarnings) {
    util_validate_missing_lists(meta_data = meta_data,
                                cause_label_df = cause_label_df,
                                assume_consistent_codes =
                                  assume_consistent_codes,
                                expand_codes = FALSE,
                                suppressWarnings = suppressWarnings,
                                label_col = label_col
    )
  }

  if (missing(resp_vars)) {
    resp_vars <- meta_data[[label_col]]
  }

  if (!expected_observations_missing && expected_observations != "ALL" &&
      !(PART_VAR %in% colnames(meta_data))) {
    util_message(c("For %s = %s, a column %s is needed in %s. Falling",
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

  if (expand_codes) {
    meta_data <- prep_expand_codes(meta_data,
                                   suppressWarnings,
                                   mix_jumps_and_missings = FALSE)
  }

  {
    r <- util_study_var2factor(study_data = study_data, meta_data = meta_data,
                               resp_vars = resp_vars, label_col = label_col,
                               assume_consistent_codes = assume_consistent_codes,
                               have_cause_label_df = !missing(cause_label_df),
                               code_name = MISSING_LIST,
                               include_sysmiss = FALSE)
    colnames(r) <-
      util_map_labels(colnames(r),
                      meta_data,
                      to = VAR_NAMES,
                      from = label_col)

    m <-
      vapply(lapply(util_seg_table(r, study_data,
                                   meta_data,
                                   expected_observations = expected_observations),
                    `[[`, "Freq"), sum, FUN.VALUE = integer(1))
  }
  {
    r <- util_study_var2factor(study_data = study_data, meta_data = meta_data,
                               resp_vars = resp_vars, label_col = label_col,
                               assume_consistent_codes = assume_consistent_codes,
                               have_cause_label_df = !missing(cause_label_df),
                               code_name = JUMP_LIST,
                               include_sysmiss = FALSE)
    colnames(r) <-
      util_map_labels(colnames(r),
                      meta_data,
                      to = VAR_NAMES,
                      from = label_col)
    j <-
      vapply(lapply(util_seg_table(r, study_data,
                                   meta_data,
                                   expected_observations = expected_observations),
                    `[[`, "Freq"), sum, FUN.VALUE = integer(1))
  }
  SysMiss <-
    vapply(prep_map_labels(resp_vars,
                           meta_data =  meta_data,
                           to = VAR_NAMES,
                           from = label_col),
           function(rv) {
             sum(is.na(study_data[[rv]][
               util_observation_expected(rv = rv,
                                         study_data = study_data,
                                         meta_data = meta_data,
                                         label_col = VAR_NAMES,
                                         expected_observations =
                                           expected_observations)]))
                    },
                    FUN.VALUE = integer(1))

  N_obs <-
    util_count_expected_observations(resp_vars,
                                     study_data = ds1,
                                     meta_data = meta_data,
                                     label_col = label_col,
                                     expected_observations = expected_observations)

  DV_N <- vapply(
    resp_vars,
    function(rv) {
      in_seg <-
        util_observation_expected(rv = rv,
                                  study_data = ds1,
                                  meta_data = meta_data,
                                  label_col = label_col,
                                  expected_observations = expected_observations)
      {
        vn <- util_map_labels(rv, meta_data,
                              VAR_NAMES, label_col)
        vals <- study_data[!in_seg, vn, drop = FALSE]
        vals <- util_replace_codes_by_NA(
          study_data = vals, meta_data = meta_data)[[vn]]
        have_data_not_expected <- !util_is_na_0_empty_or_false(vals)
        # also allow 0/FALSE, if no data is expected
        if (any(have_data_not_expected) && !suppressWarnings) {
          segvars <- util_all_intro_vars_for_rv(rv,
                                                ds1, meta_data,
                                                label_col,
                                                expected_observations =
                                                  expected_observations)
          util_warning(
            c("There are %d meassurements of %s for participants",
              "not being part of one of the segments %s"),
            sum(have_data_not_expected),
            dQuote(rv),
            paste(dQuote(util_map_labels(
              x = util_map_labels(segvars, meta_data, from = label_col,
                                  to = VAR_NAMES),
              meta_data = meta_data,
                                         from = PART_VAR, to = STUDY_SEGMENT)),
                  collapse = ", "),
            applicability_problem = TRUE)
        }
      }
      sum(!is.na(study_data[in_seg, util_map_labels(rv, meta_data,
                                                    VAR_NAMES, label_col)]))
    }, FUN.VALUE = integer(1))

  N_meas <- DV_N - m -  j

  SummaryTable <- data.frame(check.names = FALSE,
                             Variables = resp_vars,
                             `Expected observations N` = N_obs,
                             `Sysmiss N` = SysMiss,
                             `Datavalues N` = DV_N,
                             `Missing codes N` = m,
                             `Jumps N` = j,
                             `Measurements N` = N_meas
  )

  rownames(SummaryTable) <- NULL

  SysMissP <- round(SysMiss / N_obs * 100, digits = 2)
  DV_N_P <- round(DV_N / N_obs * 100, digits = 2)
  m_P <- round(m / N_obs * 100, digits = 2)
  j_P <- round(j / N_obs * 100, digits = 2)
  N_meas_P <- round(N_meas / (N_obs - j) * 100, digits = 2)
  Missing_expected_obs<- SysMiss+j+m

  SummaryTable$Missing_expected_obs<- Missing_expected_obs
  Missing_expected_obs_P <- round((SysMiss+j+m)/N_obs*100, digits = 2)
  PCT_com_crm_mv <- round((SysMiss+j+m)/ nrow(ds1)* 100, digits = 2)
  SummaryTable$PCT_com_crm_mv<- PCT_com_crm_mv
  SummaryTable$GRADING <- ifelse(N_meas_P < threshold_value, 1, 0)

  #Create SummaryData
  SummaryData<-SummaryTable

  names(SummaryData)[names(SummaryData) == "Observations N"] <-
    "Expected observations N"

  names(SummaryData)[names(SummaryData) == "Sysmiss N"] <-
    "Sysmiss N (%)"
  SummaryData$`Sysmiss N (%)`<- paste0(SysMiss, " (", SysMissP, ")")

  names(SummaryData)[names(SummaryData) == "Datavalues N"] <-
    "Datavalues N (%)"
  SummaryData$`Datavalues N (%)`<- paste0(DV_N, " (", DV_N_P, ")")

  names(SummaryData)[names(SummaryData) == "Missing codes N"] <-
    "Missing codes N (%)"
  SummaryData$`Missing codes N (%)`<- paste0(m, " (", m_P, ")")

  names(SummaryData)[names(SummaryData) == "Jumps N"] <- "Jumps N (%)"
  SummaryData$`Jumps N (%)`<- paste0(j, " (", j_P, ")")

  names(SummaryData)[names(SummaryData) == "Measurements N"] <-
    "Measurements N (%)"
  SummaryData$`Measurements N (%)`<- paste0(N_meas, " (", N_meas_P, ")")

  names(SummaryData)[names(SummaryData) == "Missing_expected_obs"] <-
    "Missing expected obs. N (%)"
  SummaryData$`Missing expected obs. N (%)`<- paste0(Missing_expected_obs,
                                                     " (",
                                                     Missing_expected_obs_P, ")")

  names(SummaryData)[names(SummaryData) == "PCT_com_crm_mv"] <-
    "Crude missingness N (%)"
  SummaryData$`Crude missingness N (%)`<- paste0(SysMiss+j+m, " (",
                                                 PCT_com_crm_mv, ")")


  # to add formulas in hover text:
  # create a named vector with names = column names of the SummaryData and
  # values = what you want to be displayed in the hover text
  text_to_display <-
    c(Variables = "",
      `Expected observations N`= paste0("Number of observational units after",
                                        " removing non-participants"),
      `Sysmiss N (%)` = "Percentage = (Sysmiss/Expected observations)*100",
      `Datavalues N (%)` = paste0("Number of datavalues = ",
                                  "Expected observations - Sysmiss. ",
                                  "Percentage = (Expected observations",
                                  " - Sysmiss)/Expected observations*100"),
      `Missing codes N (%)` = "Percentage = (Missing codes/Expected observations)*100",
      `Jumps N (%)` = "Percentage = (Jumps/Expected observations)*100",
      `Measurements N (%)` = paste0("Number of expected observations",
                                    " = Expected observations - (Sysmiss + ",
                                    "Missing codes + Jumps). ",
                                    "Percentage = (Expected observations - Sysmiss - Missing ",
                                    "codes - Jumps)/(Expected observations - Jumps) *100"),
      `Missing expected obs. N (%)` = paste0("Number of missing measurements ",
                                             "= Sysmiss + Missing codes + Jumps. ",
                                             "Percentage = (Sysmiss + Missing codes + Jumps)",
                                             "/Expected observations*100"),
      `Crude missingness N (%)` = paste0("Crude missingness = Sysmiss + ",
                                         "Missing codes + Jumps. ",
                                         "Percentage = (Sysmiss + Missing ",
                                         "codes + Jumps)/Number of observational units"),
      `GRADING` = "1 = missing values are exceeding the defined threshold")


  #  result_table$check_MC <- paste0(N_class_MC, " (", N_MC_avail, ")")
  #  result_table$check_JC <- paste0(N_class_JC, " (", N_JC_avail, ")")

  r <- util_study_var2factor(study_data = study_data, meta_data = meta_data,
                             resp_vars = resp_vars, label_col = label_col,
                             assume_consistent_codes = assume_consistent_codes,
                             have_cause_label_df = !missing(cause_label_df),
                             include_sysmiss = include_sysmiss)
  colnames(r) <-
    util_map_labels(colnames(r),
                    meta_data,
                    to = VAR_NAMES,
                    from = label_col)
  r <- util_seg_table(r, study_data, meta_data,
                      expected_observations = expected_observations)
  r <- r[vapply(r, ncol, FUN.VALUE = integer(1)) == 2]
  r <- lapply(names(r), function(nm) setNames(r[[nm]], c("CODES", nm)))
  mctab <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "CODES",
                                             all = TRUE), r)
  rownames(mctab) <- mctab$CODES
  mctab$CODES <- NULL
  mctab <- t(mctab)
  mctab[is.na(mctab)] <- 0
  mctab <- data.frame(mctab, check.names = FALSE)
  if (drop_levels) {
    mctab <- mctab[, colSums(mctab) != 0, drop = FALSE]
  }
  if (!ncol(mctab)) {
    mctab <- data.frame(check.names = FALSE,
                        Variables = resp_vars,
                        stringsAsFactors = FALSE)
  } else {
    mctab$Variables <- prep_map_labels(rownames(mctab),
                                       meta_data = meta_data,
                                       to = label_col,
                                       from = VAR_NAMES)
  }
  rownames(mctab) <- NULL
  mctab$N <-
    util_count_expected_observations(mctab$Variables,
                                     study_data = ds1,
                                     meta_data = meta_data,
                                     label_col = label_col,
                                     expected_observations = expected_observations)
  class(mctab) <- union("ReportSummaryTable", class(mctab))

  attr(SummaryData, "description") <- text_to_display


  list(SummaryTable = SummaryTable,
       SummaryData = SummaryData,
       SummaryPlot = print(mctab, view = FALSE, relative = FALSE),
       ReportSummaryTable = mctab)
}
