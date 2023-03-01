#' Utility function to compute and optimize bin breaks for histograms
#'
#' @param x a vector of data values (numeric or datetime)
#' @param iqr_bw the interquartile range of values which should be included to
#'               calculate the Freedman-Diaconis bandwidth (e.g., for
#'               `con_limit_deviations` only values within limits)
#' @param n_bw the number of values which should be included to calculate the
#'             Freedman-Diaconis bandwidth (e.g., for `con_limit_deviations`
#'             the number of values within limits)
#' @param min_within the minimum value which is still within limits
#'                   (needed for `con_limit_deviations`)
#' @param max_within the maximum value which is still within limits
#'                   (needed for `con_limit_deviations`)
#' @param min_plot the minimum value which should be included in the plot
#' @param max_plot the maximum value which should be included in the plot
#' @param nbins_max the maximum number of bins for the histogram. Strong
#'                  outliers can cause too many narrow bins, which might be
#'                  even to narrow to be plotted. This also results in large
#'                  files and rendering problems. So it is sensible to limit
#'                  the number of bins. The function will produce a warning if
#'                  it reduces the number of bins in such a case. Reasons
#'                  could be unspecified missing value codes, or minimum or
#'                  maximum values far away from most of the data values, or
#'                  (for `con_limit_deviations`) no or few values within limits.
#'
#' @return a list with bin breaks below, within and above limits


util_optimize_histogram_bins <- function(x, iqr_bw, n_bw,
                                         min_within = NULL, max_within = NULL,
                                         min_plot = NULL, max_plot = NULL,
                                         nbins_max = NULL) {
  # check input ----------------------------------------------------------------
  util_expect_scalar(x, allow_more_than_one = TRUE, allow_na = TRUE,
                     check_type = function(x) {
    is.numeric(x) || "POSIXct" %in% class(x)
  })
  if (length(x) == 0 || all(util_empty(x))) {
    util_error("Nothing to plot!", applicability_problem = TRUE)
  }

  util_expect_scalar(iqr_bw, allow_na = TRUE, check_type = is.numeric)
  if (is.na(iqr_bw)) iqr_bw <- 0

  util_expect_scalar(n_bw, check_type = util_is_numeric_in(whole_num = TRUE))
  if (n_bw == 0) n_bw <- 1

  util_expect_scalar(min_plot, allow_null = TRUE, check_type = function(x) {
    is.numeric(x) || "POSIXct" %in% class(x)
  })
  if (is.null(min_plot)) min_plot <- min(x)

  util_expect_scalar(max_plot, allow_null = TRUE, check_type = function(x) {
    is.numeric(x) || "POSIXct" %in% class(x)
  })
  if (is.null(max_plot)) max_plot <- max(x)

  util_expect_scalar(min_within, allow_null = TRUE, check_type = function(x) {
    is.numeric(x) || "POSIXct" %in% class(x)
  })
  if (is.null(min_within)) min_within <- min_plot

  util_expect_scalar(max_within, allow_null = TRUE, check_type = function(x) {
    is.numeric(x) || "POSIXct" %in% class(x)
  })
  if (is.null(max_within)) max_within <- max_plot

  util_expect_scalar(nbins_max,
                     allow_null = TRUE,
                     check_type = util_is_numeric_in(min = 40, max = 500,
                                                     whole_num = TRUE))

  # step 1: Freedman-Diaconis bandwidth, adjusted to limits (if any) -----------
  dif <- as.numeric(max_within) - as.numeric(min_within)
  dif_below <- as.numeric(min_within) - as.numeric(min_plot)
  dif_above <- as.numeric(max_plot) - as.numeric(max_within)
  # If the distinction below / within / above limits is not needed, all values
  # will be considered to be "within" limits here. The values of dif_below and
  # dif_above will be 0.

  # - calculate bandwidth according to Freedman-Diaconis:
  #   (2 * IQR(data) / length(data)^(1/3))
  bw <- 2 * iqr_bw / n_bw^(1 / 3)
  if (bw == 0) bw <- 1
  # - calculate the number of bins between the lower and upper limit, or between
  #   the minimum and maximum value if there are no limits, based on the
  #   calculated bandwidth
  # - round the number of bins within limits to an integer value to obtain bin
  #   breaks at the limits
  # - get the new bandwidth for the rounded number of bins within limits
  nbins_within <- round(dif / bw)
  if (nbins_within == 0) nbins_within <- 1
  byX <- dif / nbins_within

  # step 2: adjust bandwidth for nbins_max if needed ---------------------------
  # - if a maximum number of bins has been specified, calculate the minimum
  #   possible bandwidth
  if (!is.null(nbins_max)) {
    bw_min <- (as.numeric(max_plot) - as.numeric(min_plot)) / nbins_max
  } else {
    bw_min <- NULL
  }
  # - check whether the bandwidth `byX` is not below the minimum bandwidth.
  #   If it is, use 'floor' with the minimum bandwidth to calculate and round
  #   the number of bins. This approach will fail in keeping the bandwidth
  #   at or above 'bw_min', if 'dif' < 'bw_min' (i.e., the minimum possible bin
  #   width is larger than the range within limits). Since we want to have at
  #   least one bin within limits, we have to switch to different bin sizes in
  #   this case to still be able to split the plot below, within and above
  #   limits. So in this case, we will allow to have a smaller bin within
  #   limits and otherwise use bandwidth 'bw_min'.
  if (!is.null(bw_min) && byX < bw_min) {
    nbins_within <- floor(dif / bw_min)
    if (nbins_within == 0) {
      nbins_within <- 1
      byX <- bw_min
    } else {
      byX <- dif / nbins_within
    }
    # throw an informative warning message hinting to possible problems
    # (unspecified missing value codes, limits far away from observed values)
    # TODO: util_looks_like_missing works only for numeric variables. Does
    # as.numeric for datetime variables work here as intended?
    likely <- x[intersect(
      # search for typical patterns for missing value codes
      which(util_looks_like_missing(as.numeric(x))),
      # restrict these to values equal to or outside expected ranges
      # (If limits had not been specified in the metadata, max_within and
      # min_within will be the highest and lowest observed data value.)
      which(x >= max_within | x <= min_within))]
    if (length(likely) == 0) {
      likely <- c(max(x), min(x))
    }
    util_warning(
      c("The number of bins in the histogram were reduced below %d bins.",
        "Possible reasons for an excessive number of bins could be unspecified",
        "missing codes (perhaps %s?) or misspecified limits in the metadata."
      ),
      nbins_max,
      paste0(dQuote(likely), collapse = ", "),
      applicability_problem = FALSE
    )
  }

  # - calculate the number of bins below and above limits
  nbins_below <- ceiling(dif_below / byX)
  nbins_above <- ceiling(dif_above / byX)
  #   Due to the forced breaks at the limits, we could have now a few bins more
  #   than nbins_max despite adjusting the bandwidth. If this happens, we will
  #   increase the bandwidth slightly.
  if (!is.null(nbins_max) &&
      nbins_below + nbins_within + nbins_above > nbins_max) {
    # increase the minimum bin width arbitrarily to get less bins
    bw_min <- (as.numeric(max_plot) - as.numeric(min_plot)) / (nbins_max - 6)
    nbins_within <- floor(dif / bw_min)
    if (nbins_within == 0) {
      nbins_within <- 1
      byX <- bw_min
    } else {
      byX <- dif / nbins_within
    }
    nbins_below <- ceiling(dif_below / byX)
    nbins_above <- ceiling(dif_above / byX)
  }

  # step 3: get the position of bin breaks -------------------------------------
  if (nbins_within == 1) {
    breaks_within <- c(min_within, max_within)
  } else {
    breaks_within <- seq(from = min_within,
                         to = max_within,
                         by = byX)
  }
  breaks_below <- seq(from = min_within - nbins_below * byX,
                      to = min_within,
                      by = byX)
  # If 'dif_below' is 0, 'nbins_below' will be 0 too, and breaks_below will
  # contain only the value of 'min_within'. The same holds for values "above"
  # the upper limit:
  breaks_above <- seq(from = max_within,
                      to = max_within + nbins_above * byX,
                      by = byX)

  return(breaks = list("below" = breaks_below,
                       "within" = breaks_within,
                       "above" = breaks_above))
}
