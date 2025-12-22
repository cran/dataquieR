#' Compute Descriptive Statistics - categorical variables
#'
#' @description
#'
#' generates a descriptive overview of the categorical variables (nominal and
#' ordinal) in `resp_vars`.
#'
#' [Descriptor]
#'
#' @details
#' TODO
#'
#' @inheritParams .template_function_indicator
#'
#' @param resp_vars [variable] the name of the categorical measurement variable
#' @param hard_limits_removal [logical] if TRUE values outside hard limits are
#'                                      removed from the data before calculating
#'                                      descriptive statistics. The default is FALSE
#' @param ... arguments to be passed to all called indicator functions if
#'            applicable.
#'
#' @return a [list] with:
#'   - `SummaryTable`: [data.frame]
#'   - `SummaryData`: [data.frame]
#' @export
#'
#' @examples
#' \dontrun{
#' prep_load_workbook_like_file("meta_data_v2")
#' xx <- des_summary_categorical(study_data = "study_data", meta_data =
#'                               prep_get_data_frame("item_level"))
#' util_html_table(xx$SummaryData)
#' util_html_table(des_summary_categorical(study_data = prep_get_data_frame("study_data"),
#'                    meta_data = prep_get_data_frame("item_level"))$SummaryData)
#' }
#' @seealso
#' [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_des_impl_summary.html
#' )
#' @importFrom graphics barplot hist
#' @importFrom stats mad
#'
des_summary_categorical <- function(resp_vars = NULL, # IDEA: group_vars = NULL, co_vars = NULL,
                                    study_data,
                                    label_col,
                                    item_level = "item_level",
                                    meta_data = item_level,
                                    meta_data_v2,
                                    hard_limits_removal =
                                      getOption("dataquieR.des_summary_hard_lim_remove",
                                                dataquieR.des_summary_hard_lim_remove_default),
                                    ...) { # TODO: Describe @return slots
  # TODO: add figures also to Excel Exports and prevent column type guessing
  # by datatables export correctly (Java Script)

  # TODO: add function util_first_arg_study_data_or_resp_vars

  ##### Preparation -----
  # Metadata and study data set up ----
  util_maybe_load_meta_data_v2()
  util_ck_arg_aliases()
  util_expect_data_frame(study_data)
  try(util_expect_data_frame(meta_data), silent = TRUE)
  item_level <- meta_data <-
    util_amend_missing_metadata(study_data = study_data,
                                meta_data = meta_data,
                                level = VARATT_REQUIRE_LEVELS$RECOMMENDED)
  if (util_is_try_error(
    erobj <- try(prep_prepare_dataframes(.replace_hard_limits = FALSE,
                                .replace_missings = FALSE,
                                .amend_scale_level = TRUE,
                                .apply_factor_metadata = FALSE,
                                .apply_factor_metadata_inadm = FALSE
    ), silent = TRUE))) {
    w <- "%s has problems: %s, estimating %s"
    if (suppressWarnings(util_ensure_suggested("cli", err = FALSE))) {
      w <- cli::bg_black(cli::col_yellow(w))
    }
    util_warning(w, sQuote("meta_data"),
                 dQuote(conditionMessage(util_condition_from_try_error(erobj))),
                 sQuote("meta_data"),
                 immediate = TRUE)
    meta_data <-
      prep_study2meta(study_data, level = VARATT_REQUIRE_LEVELS$RECOMMENDED)
    prep_prepare_dataframes(.meta_data = meta_data,
                            .replace_hard_limits = FALSE,
                            .replace_missings = FALSE,
                            .amend_scale_level = TRUE,
                            .apply_factor_metadata = FALSE,
                            .apply_factor_metadata_inadm = FALSE
    )
  }

  # Define resp_vars----
  if (length(resp_vars) == 0) {
    #in case of missing resp_vars use all variables in the study data
    suppress <- function(...) suppressWarnings(suppressMessages(...))
    resp_vars <- colnames(ds1)
  } else {
    # in case of resp_vars present, check them
    suppress <- eval
  }

  #Check and remove resp_vars not present in the study data
  suppress(util_correct_variable_use(resp_vars,
                                     allow_more_than_one = TRUE,
                                     do_not_stop = TRUE,
                                     need_scale = "nominal | ordinal"))

  #resp_vars <- intersect(colnames(ds1), resp_vars)


  # Select only resp_vars that are categorical
  df_category <- meta_data[meta_data[, label_col, drop = TRUE] %in% resp_vars, ]


  study_data_new <- ds1[, colnames(ds1) %in% resp_vars, drop = FALSE]

  colnames(study_data_new) <-
    util_map_labels(colnames(study_data_new),
                    meta_data = df_category,
                    to = VAR_NAMES,
                    from = label_col)

  # Create descriptive statistics
  result1 <- des_summary(study_data = study_data_new,
                         item_level = df_category,
                         label_col = label_col)

  # Select only needed columns of SummaryData
  names_vector <- c("Variables",
                    "Type",
                    "STUDY_SEGMENT",
                    "Median",
                    "Mode",
                    "IQR (Quartiles)",
                    "Range (Min - Max)",
                    "No. categories (incl.NAs)",
                    "Frequency table",
                    "Valid",
                    "Missing",
                    "Graph")

  summary_data <-
    result1$SummaryData[ ,
                         colnames(result1$SummaryData) %in% names_vector,
                         drop = FALSE]

  summary_table <-
    result1$SummaryTable

  summary_data[is.na(summary_data)] <- ""

  return(list(SummaryData = util_attach_attr(summary_data,
                                             is_html_escaped = TRUE),
              SummaryTable = util_attach_attr(summary_table,
                                              is_html_escaped = TRUE)))
}
