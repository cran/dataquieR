#' Utility function to distribute points across a time variable
#'
#' @param time_var_data vector of the data points of the time variable
#' @param n_points      maximum number of points to distribute across the
#'                      time variable (minimum: 3)
#' @param prop_grid     proportion of points given in `n_points` that should be
#'                      distributed in an equally spaced grid across the time
#'                      variable (minimum: 0.1, maximum: 1). The remaining
#'                      proportion of points will be spaced according to the
#'                      distribution of the time variable's data points.
#'
#' @return a sequence of points in datetime format
#' @keywords internal
util_optimize_sequence_across_time_var <- function(time_var_data,
                                                   n_points,
                                                   prop_grid = 0.5) {
  util_expect_scalar(n_points,
                     check_type = util_is_numeric_in(min = 3,
                                                     whole_num = TRUE,
                                                     finite = TRUE))
  util_expect_scalar(prop_grid,
                     check_type = util_is_numeric_in(min = 0.1, max = 1))

  time_var_data <- na.omit(time_var_data)
  tp_seq <- unique(time_var_data)
  tp_seq <- sort(tp_seq)

  if (length(tp_seq) < 2) {
    util_error("Internal error in sequence optimization.") # should not occur
  }

  if (n_points < length(tp_seq)) { # reduce time points
    # distribute some points in a fixed grid
    n_grid <- max(floor(prop_grid * n_points), 3)
    # at least three points in a grid
    # compute number of seconds between two consecutive time points
    period <- (max(tp_seq) - min(tp_seq)) / (n_grid - 1)
    secs <- suppressWarnings(as.integer(as.double(period, units = "secs")))
    if (secs <= 0) {
      secs <- 1
    }
    # This defines a new sequence of equally spaced time points ranging from
    # the earliest observed time point to the last one.
    # This sequence is stored in `tp_round_seq`. The number of time points
    # equals the number given in `n_equ_spaced`.
    tp_round_seq <- suppressWarnings(as.POSIXct(
      seq(from = min(tp_seq),
          to = max(tp_seq),
          by = secs)))
    tp_round_seq[length(tp_round_seq)] <- max(tp_seq) # can otherwise deviate
    # due to rounding/numerical errors
    # distribute remaining points (if any) according to the distribution of
    # the data
    if (n_points - n_grid > 0) {
      tp_tab <- util_table_of_vct(
        cut(time_var_data,
            breaks = c(tp_round_seq[1:(length(tp_round_seq) - 1)],
                       tp_round_seq[length(tp_round_seq)] + 1)))
      tp_tab$weight <- tp_tab$Freq/sum(tp_tab$Freq)
      tp_tab$n_add <- floor(tp_tab$weight * (n_points - n_grid))
      tp_round_seq <- c(do.call(c,
        lapply(seq_along(tp_round_seq[-1]), function(tp_i) {
          if (tp_tab$n_add[tp_i] > 0) {
            period <- (tp_round_seq[tp_i + 1] - tp_round_seq[tp_i]) /
              (tp_tab$n_add[tp_i] + 1)
            secs <- suppressWarnings(as.integer(
              as.double(period, units = "secs")))
            if (secs <= 0) {
              secs <- 1
            }
            suppressWarnings(as.POSIXct(
              tp_round_seq[tp_i] + c(0, seq_len(tp_tab$n_add[tp_i])) * secs))
          } else {
            tp_round_seq[tp_i]
          }
        })
      ), max(tp_seq))
    }
  } else {
    tp_round_seq <- tp_seq
  }

  return(tp_round_seq)
}
