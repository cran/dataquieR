#' Utility function to compute and optimize bin breaks for histograms
#'
#' @param x a vector of data values (numeric or datetime)
#' @param interval_freedman_diaconis range of values which should be included to
#'               calculate the Freedman-Diaconis bandwidth (e.g., for
#'               `con_limit_deviations` only values within limits) in interval
#'               notation (e.g., `[0;100]`)
#' @param nbins_max the maximum number of bins for the histogram. Strong
#'                  outliers can cause too many narrow bins, which might be
#'                  even to narrow to be plotted. This also results in large
#'                  files and rendering problems. So it is sensible to limit
#'                  the number of bins. The function will produce a message if
#'                  it reduces the number of bins in such a case. Reasons
#'                  could be unspecified missing value codes, or minimum or
#'                  maximum values far away from most of the data values, a few
#'                  number of unique values, or (for `con_limit_deviations`)
#'                  no or few values within limits.
#' @param cuts a vector of values at which breaks between bins should occur
#'
#' @return a list with bin breaks, if needed separated for each segment
#'         of the plot
#'
#' @family figure_functions
#' @concept figure
#' @noRd
util_optimize_histogram_bins <- function(x, interval_freedman_diaconis = NULL,
                                         nbins_max = 100, cuts = NULL) {
  # check and prepare input ----------------------------------------------------
  if (inherits(x, "POSIXlt")) x <- util_parse_date(x)
  util_expect_scalar(x, allow_more_than_one = TRUE, allow_na = TRUE,
                     check_type = function(x) {
    is.numeric(x) || "POSIXct" %in% class(x) || inherits(x, "hms")
  })
  if (any(is.infinite(x))) {
    x[is.infinite(x)] <- NA
  }
  if (length(x) == 0 || all(util_empty(x))) {
    util_error("Nothing to plot!", applicability_problem = TRUE)
  }

  x <- x[!is.na(x)]
  min_x <- min(x)
  max_x <- max(x)
  if (inherits(x, "hms")) {
    min_x <- hms::as_hms(min_x)
    max_x <- hms::as_hms(max_x)
  }

  if (inherits(cuts, "POSIXlt")) cuts <- util_parse_date(cuts)
  util_expect_scalar(cuts, allow_null = TRUE, allow_more_than_one = TRUE,
                     check_type = function(x) {
                       is.numeric(x) || "POSIXct" %in% class(x)
                     })

  util_expect_scalar(interval_freedman_diaconis,
                     allow_null = TRUE, allow_more_than_one = TRUE)
  if (is.null(interval_freedman_diaconis)) {
    #interval_freedman_diaconis <- util_parse_interval(
    #  paste0("[", min_x, ";", max_x, "]"))
    interval_freedman_diaconis <-
      redcap_env$interval(inc_l = TRUE, inc_u = TRUE,
                          low = min_x, upp = max_x)
  } else {
    if (!("interval" %in% class(interval_freedman_diaconis))) {  #TOFIX: if the interval is 110; Inf it stops with an error
      interval_freedman_diaconis <-
        util_parse_interval(interval_freedman_diaconis)
    }
    if (!("interval" %in% class(interval_freedman_diaconis))) {
      util_error("The interval is incorrect.")
    }
    cuts <- c(cuts, interval_freedman_diaconis$low,
              interval_freedman_diaconis$upp)
  }

  min_x <- as.numeric(min_x)
  max_x <- as.numeric(max_x)
  cuts <- as.numeric(cuts)
  cuts <- cuts[which(!is.na(cuts) & !is.infinite(cuts))]
  cuts <- sort(unique(cuts))
  if (length(cuts) > 1) {
    # omit 'cuts' at (practically) the same value (not caught by 'unique')
    cuts <- cuts[cuts[-1] - cuts[-length(cuts)] > .Machine$double.eps]
  }
  if (length(cuts) > 0) {
    if (cuts[1] - min_x >= .Machine$double.eps) {
      cuts <- c(-Inf, cuts)
    }
    if (max_x - cuts[length(cuts)] >= .Machine$double.eps) {
      cuts <- c(cuts, Inf)
    }
  } # Note: 'cuts' can still be null

  min_plot <- min(c(min_x, cuts[!is.infinite(cuts)]))
  max_plot <- max(c(max_x, cuts[!is.infinite(cuts)]))

  util_expect_scalar(nbins_max,
                     check_type = util_is_numeric_in(min = 1, max = 500,
                                                     whole_num = TRUE))

  # step 1: Freedman-Diaconis bandwidth ----------------------------------------
  x_within <- x[redcap_env$`in`(x, interval_freedman_diaconis)]
  # If there are no values within the specified interval, we discard the
  # interval and consider all values instead.
  if (length(x_within) == 0) {
    x_within <- x
    min_within <- min_x
    max_within <- max_x
  } else {
    min_within <- min(x_within)
    if (!is.infinite(interval_freedman_diaconis$low)) {
      min_within <- min(min_within, interval_freedman_diaconis$low)
    }
    min_within <- as.numeric(min_within)
    max_within <- max(x_within)
    if (!is.infinite(interval_freedman_diaconis$upp)) {
      max_within <- max(max_within, interval_freedman_diaconis$upp)
    }
    max_within <- as.numeric(max_within)
  }
  # - calculate span
  dif <- max_within - min_within
  # - calculate bandwidth according to Freedman-Diaconis:
  #   (2 * IQR(data) / length(data)^(1/3))
  iqr_bw <- IQR(x_within)
  if (is.na(iqr_bw)) iqr_bw <- 0
  n_bw <- length(x_within)
  if (n_bw == 0) n_bw <- 1
  bw <- 2 * iqr_bw / n_bw^(1 / 3)
  if (bw == 0) bw <- 1

  # step 2: adjust bandwidth ---------------------------------------------------
  # - calculate the number of bins between the lower and upper limit of the
  #   interval (or between the minimum and maximum value, see above), based on
  #   the calculated bandwidth
  # - round the number of bins within limits to an integer value to obtain bin
  #   breaks at the limits
  # - get the new bandwidth for the rounded number of bins within limits
  nbins_within <- round(dif / bw)
  if (nbins_within == 0) nbins_within <- 1
  byX <- dif / nbins_within

  # - calculate a lower limit for the bandwidth based on the maximum number of
  #   bins
  bw_min <- (max_x - min_x) / nbins_max
  # - for constant variables: dif = 0 => byX = 0 and also bw_min = 0
  # - in this case, we have to provide a good default value for bw_min
  # - for this, we can consider the distances between segments (if any), or
  #   use the 'pretty' function
  if (length(cuts) > 1) {
    # calculate span for each segment
    dif_seq <- cuts[-1] - cuts[-(length(cuts))]
  } else {
    dif_seq <- max_plot - min_plot
  }
  if (bw_min == 0) {
    # consider distance between cuts
    min_dist <- suppressWarnings(
      min(dif_seq[which(!is.infinite(dif_seq) & dif_seq > 0)]))
    # consider proposal from 'pretty' function
    pp <- pretty(c(min_plot, max_plot), min.n = 1)
    bw_min <- min(c(min_dist, pp[2] - pp[1]))
  }
  # - check whether the bandwidth `byX` is not below the minimum bandwidth.
  #   If it is, use 'floor' with the minimum bandwidth to calculate and round
  #   the number of bins within limits.
  #   This approach will fail in keeping the bandwidth at or above 'bw_min',
  #   if 'dif' < 'bw_min' (i.e., the minimum possible bin width is larger than
  #   the range within limits, or, respectively, larger than the range from the
  #   minimum value to the maximum value). In this case, we have to switch to
  #   different bin sizes for the segments and will allow to have a smaller bin
  #   within limits, if needed. For constant variables, the single bar of the
  #   histogram will have width 'bw_min'.
  if (byX < bw_min) {
    nbins_within <- floor(dif / bw_min)
    if (nbins_within == 0) {
      byX <- bw_min
    } else {
      byX <- dif / nbins_within
    }
  }
  # - if all values are integer values, then we have to ensure that the
  #   bandwidth `byX` is an integer >= 1
  if (all(util_is_integer(x))) {
    byX <- max(round(byX), 1)
  }
  # - calculate the number of bins for each segment
  nbins_seq <- vapply(dif_seq, FUN.VALUE = numeric(1), function(dd) {
    ceiling(dd / byX)
  })
  # Note: Due to the forced breaks at the limits, we could have now a few bins
  # more than 'nbins_max'.

  # The forced breaks can change the bandwidth in each segment. In case there
  # is a segment with an open upper/lower limits, we could use the average of
  # the bandwidths from the fixed segments for it, to improve the overall look
  # of the segmented histogram.
  if (length(cuts) > 1) {
    byX_mean <- dif_seq / nbins_seq
    byX_mean <- byX_mean[is.finite(byX_mean)]
    if (length(byX_mean) > 0) {
      byX_mean <- sum(byX_mean) / length(byX_mean)
      if (byX_mean > bw_min) {
        byX <- byX_mean
      }
    }
    if (is.infinite(nbins_seq[1])) {
      nbins_seq[1] <- ceiling(abs(cuts[2] - min_plot) / byX)
    }
    if (is.infinite(nbins_seq[length(nbins_seq)])) {
      nbins_seq[length(nbins_seq)] <-
        ceiling(abs(max_plot - cuts[length(cuts) - 1]) / byX)
    }
  }

  # step 3: get the position of bin breaks -------------------------------------
  if (length(cuts) <= 1) {
    all_breaks <- list(pretty(c(min_plot, max_plot),
                              n = max(nbins_seq, 1), min.n = 1))
    if ("POSIXct" %in% class(x)) {
      all_breaks[[1]] <- util_parse_date(all_breaks[[1]])
    }
  } else {
    all_breaks <- vector(mode = "list", length = length(nbins_seq))
    for (i in seq_along(nbins_seq)) {
      if (is.infinite(cuts[i])) {
        # We ensured by construction that cuts[i] and cuts[i+1] can not both
        # be infinite.
        all_breaks[[i]] <- seq(from = cuts[i+1] - nbins_seq[i] * byX,
                               to = cuts[i+1],
                               length.out = nbins_seq[i] + 1)
      } else if (is.infinite(cuts[i+1])) {
        all_breaks[[i]] <- seq(from = cuts[i],
                               to = cuts[i] + nbins_seq[i] * byX,
                               length.out = nbins_seq[i] + 1)
      } else if (nbins_seq[i] <= 1) {
        all_breaks[[i]] <- c(cuts[i], cuts[i+1])
      } else {
        all_breaks[[i]] <- seq(from = cuts[i],
                               to = cuts[i+1],
                               length.out = nbins_seq[i] + 1)
      }
      if ("POSIXct" %in% class(x)) {
        all_breaks[[i]] <- util_parse_date(all_breaks[[i]])
      }
    }
  }

  return(breaks = all_breaks)
}
