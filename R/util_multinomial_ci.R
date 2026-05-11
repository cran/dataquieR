#' Simultaneous confidence intervals for multinomial proportions
#'
#' Sison & Glaz style implementation with the same user-facing API as
#' MultinomialCI::multinomialCI(x, alpha, verbose = FALSE).
#'
#' This implementation follows the published Sison & Glaz / May & Johnson
#' formulas and is written independently in R. It is suitable as a permissively
#' licensed replacement. If you redistribute it, keep the attribution note below.
#'
#' Attribution note:
#'   The algorithmic structure and formulas used here are consistent with the
#'   Sison-Glaz method as described in:
#'   - Sison, C.P. and Glaz, J. (1995)
#'   - May, W.L. and Johnson, W.D. (1997, 2000)
#'   and with the BSD-3 licensed statsmodels implementation documentation.
#'
#' @param x Integer vector of counts.
#' @param alpha Significance level in `[0, 1]`.
#' @param verbose Logical.
#'
#' @return Numeric matrix with two columns: lower and upper.
#' @noRd
util_multinomial_ci <- function(x, alpha, verbose = FALSE) {
  .util_multinomial_ci_validate_input(x = x, alpha = alpha, verbose = verbose)

  x <- as.numeric(x)
  n <- sum(x)
  k <- length(x)

  if (n <= 0) {
    util_error("sum(x) must be positive.", call. = FALSE)
  }

  p_hat <- x / n

  # Keep the documented [0, 1] boundary behavior user-friendly.
  if (alpha <= 0) {
    out <- cbind(lower = rep(0, k), upper = rep(1, k))
    return(unname(out))
  }
  if (alpha >= 1) {
    out <- cbind(lower = p_hat, upper = p_hat)
    return(unname(out))
  }

  .poisson_interval <- function(interval, lambda) {
    lower <- interval[1]
    upper <- interval[2]

    prob <- stats::ppois(upper, lambda = lambda) -
      stats::ppois(lower - 1, lambda = lambda)

    if (lambda == 0 && is.nan(prob)) {
      return(as.numeric(lower - 1 < 0))
    }

    prob
  }

  .truncated_poisson_factorial_moment <- function(interval, r, lambda) {
    lower <- interval[1]
    upper <- interval[2]

    denom <- .poisson_interval(c(lower, upper), lambda)
    if (denom <= 0) {
      return(0)
    }

    lambda^r * (
      1 - (
        (.poisson_interval(c(upper - r + 1, upper), lambda) -
           .poisson_interval(c(lower - r, lower - 1), lambda)) / denom
      )
    )
  }

  .edgeworth <- function(intervals, counts) {
    mu_r <- lapply(
      1:4,
      function(r) {
        vapply(
          seq_along(counts),
          function(i) .truncated_poisson_factorial_moment(intervals[[i]], r, counts[i]),
          numeric(1)
        )
      }
    )

    mu_r1 <- mu_r[[1]]
    mu_r2 <- mu_r[[2]]
    mu_r3 <- mu_r[[3]]
    mu_r4 <- mu_r[[4]]

    mu  <- mu_r1
    mu2 <- mu_r2 + mu - mu^2
    mu3 <- mu_r3 + mu_r2 * (3 - 3 * mu) + mu - 3 * mu^2 + 2 * mu^3
    mu4 <- mu_r4 +
      mu_r3 * (6 - 4 * mu) +
      mu_r2 * (7 - 12 * mu + 6 * mu^2) +
      mu - 4 * mu^2 + 6 * mu^3 - 3 * mu^4

    sum_mu2 <- sum(mu2)
    if (sum_mu2 <= 0) {
      return(0)
    }

    g1 <- sum(mu3) / (sum_mu2^(3 / 2))
    g2 <- (sum(mu4) - 3 * sum(mu2^2)) / (sum_mu2^2)

    x_std <- (n - sum(mu)) / sqrt(sum_mu2)
    phi <- exp(-(x_std^2) / 2) / sqrt(2 * pi)

    H3 <- x_std^3 - 3 * x_std
    H4 <- x_std^4 - 6 * x_std^2 + 3
    H6 <- x_std^6 - 15 * x_std^4 + 45 * x_std^2 - 15

    f <- phi * (1 + g1 * H3 / 6 + g2 * H4 / 24 + g1^2 * H6 / 72)
    f / sqrt(sum_mu2)
  }

  .approximated_multinomial_interval <- function(intervals, counts, n) {
    probs <- vapply(
      seq_along(counts),
      function(i) .poisson_interval(intervals[[i]], counts[i]),
      numeric(1)
    )

    if (any(probs <= 0)) {
      return(0)
    }

    ew <- .edgeworth(intervals, counts)
    if (!is.finite(ew) || ew <= 0) {
      return(0)
    }

    exp(sum(log(probs)) + log(ew) - stats::dpois(n, lambda = n, log = TRUE))
  }

  .nu <- function(c_val, counts, n) {
    intervals <- lapply(
      counts,
      function(count_i) c(max(count_i - c_val, 0), min(count_i + c_val, n))
    )
    .approximated_multinomial_interval(intervals = intervals, counts = counts, n = n)
  }

  c_val <- 1
  nu_c <- .nu(c_val, x, n)
  nu_cp1 <- .nu(c_val + 1, x, n)

  while (!(nu_c <= (1 - alpha) && (1 - alpha) < nu_cp1)) {
    if (verbose) {
      util_message(paste0(
        "c = ", c_val,
        ", nu(c) = ", format(nu_c, digits = 8),
        ", nu(c+1) = ", format(nu_cp1, digits = 8)
      ))
    }

    if (c_val > n) {
      util_error("Could not find a value c satisfying nu(c) <= 1 - alpha < nu(c + 1).",
           call. = FALSE)
    }

    c_val <- c_val + 1
    nu_c <- nu_cp1
    nu_cp1 <- .nu(c_val + 1, x, n)
  }

  g <- (1 - alpha - nu_c) / (nu_cp1 - nu_c)

  if (verbose) {
    util_message(paste0(
      "Final: c = ", c_val,
      ", nu(c) = ", format(nu_c, digits = 10),
      ", nu(c+1) = ", format(nu_cp1, digits = 10),
      ", gamma = ", format(g, digits = 10)
    ))
  }

  lower <- pmax(p_hat - c_val / n, 0)
  upper <- pmin(p_hat + (c_val + 2 * g) / n, 1)

  out <- cbind(lower = lower, upper = upper)
  unname(out)
}

.util_multinomial_ci_validate_input <- function(x, alpha, verbose) {
  if (missing(x)) {
    util_error("Argument 'x' is missing.", call. = FALSE)
  }
  if (!is.numeric(x) || !is.vector(x) || length(x) < 2L) {
    util_error("'x' must be a numeric vector of length >= 2.", call. = FALSE)
  }
  if (any(!is.finite(x))) {
    util_error("'x' must contain only finite values.", call. = FALSE)
  }
  if (any(x < 0)) {
    util_error("'x' must contain non-negative counts.", call. = FALSE)
  }
  if (any(abs(x - round(x)) > sqrt(.Machine$double.eps))) {
    util_error("'x' must contain integer counts.", call. = FALSE)
  }

  if (missing(alpha) || !is.numeric(alpha) || length(alpha) != 1L ||
      !is.finite(alpha) || alpha < 0 || alpha > 1) {
    util_error("'alpha' must be a single real number in [0, 1].", call. = FALSE)
  }

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    util_error("'verbose' must be TRUE or FALSE.", call. = FALSE)
  }
}

#' Simple binomial confidence intervals for `multinomial` counts
#'
#' Computes independent binomial confidence intervals for each
#' category proportion using the same interface as
#' `MultinomialCI::multinomialCI()`.
#'
#' @param x Integer vector of counts.
#' @param alpha Significance level in `[0, 1].`
#' @param verbose Logical (unused; kept for API compatibility).
#'
#' @return Numeric matrix with columns: lower and upper.
#' @noRd
util_binomial_ci <- function(x, alpha, verbose = FALSE) {

  .util_multinomial_ci_validate_input(
    x = x,
    alpha = alpha,
    verbose = verbose
  )

  x <- as.numeric(x)

  n <- sum(x)

  out <- t(vapply(
    x,
    function(xi) {

      ci <- stats::prop.test(
        x = xi,
        n = n,
        conf.level = 1 - alpha
      )$conf.int

      c(
        lower = ci[1],
        upper = ci[2]
      )
    },
    numeric(2)
  ))

  unname(out)
}
