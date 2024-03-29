#' Extension of [acc_shape_or_scale] to examine uniform distributions of
#' end digits
#'
#' @description
#' This implementation contrasts the empirical distribution of a measurement
#' variables against assumed distributions. The approach is adapted from the
#' idea of rootograms (Tukey (1977)) which is also applicable for count data
#' (Kleiber and Zeileis (2016)).
#'
#' @details
#' # ALGORITHM OF THIS IMPLEMENTATION:
#' - This implementation is restricted to data of type float or integer.
#' - Missing codes are removed from resp_vars (if defined in the metadata)
#' - The user must specify the column of the metadata containing probability
#'   distribution (currently only: normal, uniform, gamma)
#' - Parameters of each distribution can be estimated from the data or are
#'   specified by the user
#' - A histogram-like plot contrasts the empirical vs. the technical
#'   distribution
#'
#' @param resp_vars [variable] the names of the measurement variables,
#'                             mandatory
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#'
#' @return a [list] with:
#'   - `SummaryData`: data frame underlying the plot
#'   - `SummaryPlot`: ggplot2 distribution plot comparing expected
#'                    with observed distribution
#'
#' @export
#' @importFrom rlang :=
#'
#' @seealso
#' [Online Documentation](
#' https://dataquality.qihs.uni-greifswald.de/VIN_acc_impl_end_digits.html
#' )
acc_end_digits <- function(resp_vars = NULL, study_data, meta_data,
                           label_col = VAR_NAMES) {

  # preps ----------------------------------------------------------------------
  # map metadata to study data
  prep_prepare_dataframes(.replace_hard_limits = TRUE)

  # correct variable use?
  util_correct_variable_use("resp_vars",
    allow_na = FALSE,
    need_type = "integer|float"
  )

  if (.called_in_pipeline && util_is_na_0_empty_or_false(
    meta_data[meta_data[[label_col]] == resp_vars, END_DIGIT_CHECK])) {
    util_error("No end digit check requested for %s",
               dQuote(resp_vars),
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE) # this is not really intrinsic, but, from a user's point of view, it is.
  }

  # checks
  if (is.null(resp_vars) || length(ds1[[resp_vars]]) == 0L ||
      mode(ds1[[resp_vars]]) != "numeric") {
    # likely dead code
    util_error("%s must be a non-empty vector of names of numeric variables",
               dQuote("resp_vars"), applicability_problem = TRUE) # nocov
  }

  if (any(is.infinite(ds1[[resp_vars]]))) {
    util_error("Values in 'resp_vars' must not contain infinite data",
               applicability_problem = TRUE)
  }

  vlabels <- meta_data[meta_data[[label_col]] == resp_vars, VALUE_LABELS]

  have_labels <- vapply(util_parse_assignments(vlabels,
         multi_variate_text = TRUE), length, FUN.VALUE = integer(1)) > 0

  if (have_labels) {
    util_error("%s are categorical, so end digits are not reasonable to check",
               sQuote("resp_vars"),
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
  }

  vtype <- meta_data[meta_data[[label_col]] == resp_vars, DATA_TYPE]

  decs <- meta_data[meta_data[[label_col]] == resp_vars, DECIMALS]

  if (is.null(decs)) {
    decs <- NA
  }

  decs <- as.integer(decs)

  if (vtype == DATA_TYPES$FLOAT && (is.na(decs))) {
    util_error(
      "The number of digits following the decimal point must be prespecified.",
      applicability_problem = FALSE
    )
  }

  if (vtype == DATA_TYPES$FLOAT && all(util_is_integer(ds1[[resp_vars]]))) {
    util_message("%s is of type integer.",
                 dQuote(resp_vars),
                 applicability_problem = TRUE)
    vtype <- "integer"
  }

  if (vtype == DATA_TYPES$INTEGER && !(all(util_is_integer(ds1[[resp_vars]])))) {
    util_error("%s is not of type integer.",
               dQuote(resp_vars),
               applicability_problem = TRUE)
  }

  # create new row of metadata attributes for transformed variable of last
  # digits
  meta_tmp <- data.frame(matrix(NA, ncol = length(colnames(meta_data))))
  colnames(meta_tmp) <- colnames(meta_data)
  # avoid overwriting existing variables
  i <- ""
  while ((new_v_nm <- sprintf("%s_x_last%s", resp_vars, as.character(i))) %in%
         meta_data[[VAR_NAMES]]) {
    if (i == "") {
      i <- 1
    } else {
      i <- i + 1
    }
  }
  meta_tmp[[VAR_NAMES]] <- new_v_nm
  meta_tmp[[label_col]] <- new_v_nm
  meta_tmp[[DISTRIBUTION]] <- "uniform"

  if (vtype == DATA_TYPES$FLOAT) {
    # use modulo ops and sprintf() to remove leading numbers
    x_lds <- sprintf(paste("%.", decs, "f", sep = ""), ds1[[resp_vars]] %% 1)
        # of mode character
    # last digit
    x_last <- substring(x_lds, first = decs + 2, last = decs + 2)
    ds2 <- data.frame(cbind(ds1, x_last))
    ds2$x_last <- util_as_numeric(ds2$x_last)
    ds2 <- dplyr::rename(ds2, !!new_v_nm := "x_last")
        # https://community.rstudio.com/t/
        #              pass-a-variable-to-dplyr-rename-to-change-columnname/6907
  } else {
    ds2 <- data.frame(cbind(ds1, x_last = ds1[[resp_vars]] %% 10))
    ds2 <- dplyr::rename(ds2, !!new_v_nm := "x_last")
        # https://community.rstudio.com/t/
        #              pass-a-variable-to-dplyr-rename-to-change-columnname/6907
  }

  if (!(DISTRIBUTION %in% names(meta_data))) {
    meta_data[[DISTRIBUTION]] <- NA
  }

  meta_tmp <- dplyr::bind_rows(meta_data, meta_tmp)

  # rename ds1 columns for call of unexp_prob_dist()
  colnames(ds2) <- meta_tmp[[VAR_NAMES]]

  rv <- new_v_nm

  res <- acc_shape_or_scale(
    resp_vars = rv, guess = FALSE, par1 = 0, par2 = 9, end_digits = TRUE,
    study_data = ds2, meta_data = meta_tmp, dist_col = DISTRIBUTION,
    label_col = label_col
  )

  # do not return simply res to make parsing out existing results easier
  return(list(SummaryData = res$SummaryTable, SummaryPlot = util_set_size(
    res$SummaryPlot,
    width_em = 15
  )))
}
