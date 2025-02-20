#' Extension of [acc_shape_or_scale] to examine uniform distributions of
#' end digits
#'
#' @description
#' This implementation contrasts the empirical distribution of a measurement
#' variables against assumed distributions. The approach is adapted from the
#' idea of rootograms (Tukey (1977)) which is also applicable for count data
#' (Kleiber and Zeileis (2016)).
#'
#' [Indicator]
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
#' @inheritParams .template_function_indicator
#'
#' @param resp_vars [variable] the names of the measurement variables,
#'                             mandatory
#'
#' @return a [list] with:
#'   - `SummaryTable`: [data.frame] with the columns `Variables` and `FLG_acc_ud_shape`
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
acc_end_digits <- function(resp_vars = NULL,
                           study_data,
                           label_col,
                           item_level = "item_level",
                           meta_data = item_level,
                           meta_data_v2) {

  # preps ----------------------------------------------------------------------
  # map metadata to study data
  util_maybe_load_meta_data_v2()
  prep_prepare_dataframes(.replace_hard_limits = TRUE)

  # correct variable use?
  util_correct_variable_use("resp_vars",
    allow_na = FALSE,
    need_type = "integer|float",
    need_scale = "interval|ratio"
  )

  if (.called_in_pipeline && util_is_na_0_empty_or_false(
    meta_data[meta_data[[label_col]] == resp_vars, END_DIGIT_CHECK])) {
    util_error("No end digit check requested for %s",
               dQuote(resp_vars),
               applicability_problem = TRUE,
               intrinsic_applicability_problem = TRUE)
    # this is not really intrinsic, but, from a user's point of view, it is.
  }

  # checks
  if (any(is.infinite(ds1[[resp_vars]]))) {
    util_error("Values in 'resp_vars' must not contain infinite data",
               applicability_problem = TRUE)
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

  if (vtype == DATA_TYPES$FLOAT) {
    # use modulo ops and sprintf() to remove leading numbers
    x_lds <- sprintf(paste("%.", decs, "f", sep = ""), ds1[[resp_vars]] %% 1)
    # store last digit in `resp_vars` column
    ds1[[resp_vars]] <- util_as_numeric(
      substring(x_lds, first = decs + 2, last = decs + 2)
    )
  } else {
    ds1[[resp_vars]] <- ds1[[resp_vars]] %% 10
  }

  if (!(DISTRIBUTION %in% names(meta_data))) {
    meta_data[[DISTRIBUTION]] <- NA
  }

  mrow <- which(meta_data[[label_col]] == resp_vars)
  meta_data[[DISTRIBUTION]][mrow] <- DISTRIBUTIONS$UNIFORM
  meta_data[[DATA_TYPE]][mrow] <- DATA_TYPES$INTEGER

  res <- acc_shape_or_scale(
    resp_vars = resp_vars, guess = FALSE, par1 = 0, par2 = 9, end_digits = TRUE,
    study_data = ds1, meta_data = meta_data, dist_col = DISTRIBUTION,
    label_col = label_col
  )

  if (length(attr(res, "error")) > 0) {
    util_error(attr(res, "error")[[1]])
  }

  st <- res$SummaryTable
  st$Variables <- gsub("_x_last[\\s\\d]*$", "", st$Variables)

  return(util_attach_attr(list(SummaryTable = st,
              SummaryPlot = util_set_size(
                res$SummaryPlot,
                width_em = 15
              )), sizing_hints = attr(res, "sizing_hints")
  ))
}
