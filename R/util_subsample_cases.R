#' Simple subsampling for plotting
#'
#' @param data [data.frame] containing the variables to be used for selection.
#' @param x [character] naming the primary variable. In univariate mode,
#'          this is the variable used for histogram-oriented selection. In
#'          bivariate mode, this is the predictor variable used in the
#'          scatterplot-oriented selection and in the linear regression `y ~ x`.
#' @param y [character] optional. Names the second variable.
#'                      If `NULL`, the function runs in univariate mode.
#'                      If provided, the function runs in bivariate mode.
#' @param nmax [integer] Target maximum number of observations to include in
#'                       the selected subset.
#'                       If the number of eligible observations is less than or
#'                       equal to `nmax`, all eligible observations are
#'                       returned.
#' @param pinc [numeric] Proportion controlling the grid-based coverage
#'                       component:
#'                         - In the univariate case, `ceiling(nmax * pinc)`
#'                           bins are created over the range of the unique
#'                           observed values of x.
#'                         - In the bivariate case, `ceiling(nmax * pinc)`
#'                           approximately determines the number of 2D grid
#'                           cells used for support coverage of the `(x, y)`
#'                           distribution.
#'                       Typical values are small, for example `0.02` to `0.10`.
#' @param resids [numeric] Proportion controlling the residual-based inclusion
#'                         component in the bivariate case. The number of
#'                         additional observations selected on the basis of
#'                         large absolute studentized residuals is approximately
#'                         `ceiling(nmax * resids)`.
#'                         This argument is ignored in the univariate case.
#' @param random [logical] flag controlling whether only simple random sampling
#'                         is used:
#'                           - `FALSE`: use the structured subsampling algorithm
#'                           - `TRUE`: use only simple random sampling of size
#'                                     `nmax`

#' @param case_id [character] Optional. Names a variable in `data`
#'                            containing unique case identifiers.
#'                            If `NULL`, row numbers of `data` are used as
#'                            case identifiers.
#' @param seed [integer] Optional. Used to set the random seed before selection.
#'                       This allows reproducible random subsampling.
#' @description
#'
#' # Univariate mode:
#' - create one-row-per-unique-x dataset
#' - create ceiling(nmax * pinc) bins over full x range
#' - select the maximum x from each occupied bin
#' - also include the minimum x
#' - draw ceiling(nmax * (1 - pinc)) random cases
#'   from cases not already selected by the grid step
#'
#' # Bivariate mode:
#' - create one-row-per-unique-(x,y) dataset
#' - create a 2D grid with about ceiling(nmax * pinc) cells
#' - select one case from each occupied cell
#' - also include min/max x and min/max y
#' - compute y ~ x on all complete cases
#' - include the top ceiling(nmax * resids) absolute
#'   studentized residuals
#' - draw ceiling(nmax * (1 - pinc - resids)) random cases
#'   from cases not already selected
#'
#' @returns
#'   vector of case identifiers to include
#'
#' @noRd
util_subsample_cases <- function(
    data,
    x,
    y = NULL,
    nmax = 5000L,
    pinc = 0.05,
    resids = 0.01,
    random = FALSE,
    case_id = NULL,
    seed = NULL) {

  # ---------- checks ----------
  util_expect_data_frame(data)
  ds1 <- data

  meta_data <- prep_study2meta(ds1, level = VARATT_REQUIRE_LEVELS$REQUIRED,
                               convert_factors = FALSE,
                               cumulative = TRUE,
                               guess_missing_codes = FALSE,
                               guess_character = FALSE)

  util_correct_variable_use(x)
  if (!missing(y)) {
    util_correct_variable_use(y)
  }
  util_expect_scalar(nmax,
                     check_type = util_is_numeric_in(min = 1,
                                                     whole_num = TRUE,
                                                     finite = TRUE),
                     error_message = "'nmax' must be a positive integer >= 1.")

  util_expect_scalar(pinc,
                     check_type = util_is_numeric_in(min = 0,
                                                     max = 1,
                                                     whole_num = FALSE,
                                                     finite = TRUE),
                     error_message =
                       "'pinc' must be a single number in [0, 1].")

  util_expect_scalar(resids,
                     check_type = util_is_numeric_in(min = 0,
                                                     max = 1,
                                                     whole_num = FALSE,
                                                     finite = TRUE),
                     error_message =
                       "'resids' must be a single number in [0, 1].")

  util_expect_scalar(random, check_type = is.logical)

  if (!missing(case_id)) {
    util_correct_variable_use(case_id)
  }

  nmax <- as.integer(round(nmax))

  if (!is.null(seed)) withr::local_seed(seed)

  ids_all <- if (is.null(case_id)) seq_len(nrow(data)) else data[[case_id]]

  simple_sample_ids <- function(ids, size) {
    if (size <= 0L || length(ids) == 0L) return(ids[FALSE])
    if (length(ids) <= size) return(ids)
    sample(ids, size = size, replace = FALSE)
  }

  # ---------- eligible data ----------
  needed_vars <- c(x, y)
  needed_vars <- needed_vars[!is.null(needed_vars)]

  cc <- stats::complete.cases(data[, needed_vars, drop = FALSE])
  d <- data[cc, , drop = FALSE]
  d$.case_id <- ids_all[cc]

  nobs <- nrow(d)

  if (nobs == 0L) return(ids_all[FALSE])
  if (nobs <= nmax) return(d$.case_id)

  # ---------- random-only mode ----------
  if (random) {
    return(simple_sample_ids(d$.case_id, nmax))
  }

  # ============================================================
  # A. UNIVARIATE
  # ============================================================
  if (is.null(y)) {
    xvals <- d[[x]]

    # one row per unique x
    ord_unique <- !duplicated(xvals)
    d_unique <- d[ord_unique, , drop = FALSE]
    x_unique <- d_unique[[x]]

    # number of bins
    B <- max(1L, as.integer(ceiling(nmax * pinc)))

    # grid selection
    if (length(x_unique) == 1L || max(x_unique, na.rm = TRUE) ==
        min(x_unique, na.rm = TRUE)) {
      i_grid_case <- d_unique$.case_id[1L]
    } else {
      breaks <- seq(min(x_unique, na.rm = TRUE),
                    max(x_unique, na.rm = TRUE),
                    length.out = B + 1L)

      bin_id <- cut(x_unique,
                    breaks = breaks,
                    include.lowest = TRUE,
                    right = TRUE,
                    labels = FALSE)

      split_idx <- split(seq_along(x_unique), bin_id, drop = TRUE)

      max_idx_local <- vapply(split_idx, function(idx) {
        idx[which.max(x_unique[idx])]
      }, integer(1L))

      i_grid_case <- d_unique$.case_id[max_idx_local]
    }

    # make sure minimum x is included
    min_case <- d_unique$.case_id[which.min(x_unique)[1L]]
    i_case <- unique(c(i_grid_case, min_case))

    # random fill
    nrand <- as.integer(ceiling(nmax * (1 - pinc)))
    remaining <- d[!(d$.case_id %in% i_case), , drop = FALSE]
    srand <- simple_sample_ids(remaining$.case_id, nrand)

    return(unique(c(i_case, srand)))
  }

  # ============================================================
  # B. BIVARIATE
  # ============================================================
  xvals <- d[[x]]
  yvals <- d[[y]]

  # one row per unique (x, y)
  ord_unique <- !duplicated(d[, c(x, y), drop = FALSE])
  d_unique <- d[ord_unique, , drop = FALSE]
  x_unique <- d_unique[[x]]
  y_unique <- d_unique[[y]]

  # target number of grid cells
  B <- max(1L, as.integer(ceiling(nmax * pinc)))

  # choose a roughly square grid
  nx <- max(1L, floor(sqrt(B)))
  ny <- max(1L, ceiling(B / nx))

  # grid selection
  if ((length(x_unique) == 1L || max(x_unique, na.rm = TRUE) == min(x_unique, na.rm = TRUE)) &&
      (length(y_unique) == 1L || max(y_unique, na.rm = TRUE) == min(y_unique, na.rm = TRUE))) {

    i_grid_case <- d_unique$.case_id[1L]

  } else if (max(x_unique, na.rm = TRUE) == min(x_unique, na.rm = TRUE)) {

    # x constant -> bin only on y
    breaks_y <- seq(min(y_unique, na.rm = TRUE),
                    max(y_unique, na.rm = TRUE),
                    length.out = ny + 1L)

    by <- cut(y_unique,
              breaks = breaks_y,
              include.lowest = TRUE,
              right = TRUE,
              labels = FALSE)

    split_idx <- split(seq_along(y_unique), by, drop = TRUE)
    rep_idx <- vapply(split_idx, function(idx) idx[1L], integer(1L))
    i_grid_case <- d_unique$.case_id[rep_idx]

  } else if (max(y_unique, na.rm = TRUE) == min(y_unique, na.rm = TRUE)) {

    # y constant -> bin only on x
    breaks_x <- seq(min(x_unique, na.rm = TRUE),
                    max(x_unique, na.rm = TRUE),
                    length.out = nx + 1L)

    bx <- cut(x_unique,
              breaks = breaks_x,
              include.lowest = TRUE,
              right = TRUE,
              labels = FALSE)

    split_idx <- split(seq_along(x_unique), bx, drop = TRUE)
    rep_idx <- vapply(split_idx, function(idx) idx[1L], integer(1L))
    i_grid_case <- d_unique$.case_id[rep_idx]

  } else {

    breaks_x <- seq(min(x_unique, na.rm = TRUE),
                    max(x_unique, na.rm = TRUE),
                    length.out = nx + 1L)
    breaks_y <- seq(min(y_unique, na.rm = TRUE),
                    max(y_unique, na.rm = TRUE),
                    length.out = ny + 1L)

    bx <- cut(x_unique,
              breaks = breaks_x,
              include.lowest = TRUE,
              right = TRUE,
              labels = FALSE)
    by <- cut(y_unique,
              breaks = breaks_y,
              include.lowest = TRUE,
              right = TRUE,
              labels = FALSE)

    cell_id <- paste(bx, by, sep = "_")
    split_idx <- split(seq_along(x_unique), cell_id, drop = TRUE)
    rep_idx <- vapply(split_idx, function(idx) idx[1L], integer(1L))
    i_grid_case <- d_unique$.case_id[rep_idx]
  }

  # make sure min/max x and min/max y are included
  boundary_cases <- c(
    d_unique$.case_id[which.min(x_unique)[1L]],
    d_unique$.case_id[which.max(x_unique)[1L]],
    d_unique$.case_id[which.min(y_unique)[1L]],
    d_unique$.case_id[which.max(y_unique)[1L]]
  )

  i_case <- unique(c(i_grid_case, boundary_cases))

  # residual-extreme cases from y ~ x
  ir_case <- d$.case_id[FALSE]

  regression_feasible <-
    resids > 0 &&
    nrow(d) >= 3L &&
    is.finite(stats::var(d[[x]], na.rm = TRUE)) &&
    stats::var(d[[x]], na.rm = TRUE) > 0

  if (regression_feasible) {
    fit <- try(stats::lm(stats::as.formula(paste(y, "~", x)), data = d), silent = TRUE)

    if (!util_is_try_error(fit)) {
      rstud <- try(abs(stats::rstudent(fit)), silent = TRUE)

      if (!util_is_try_error(rstud) &&
          is.numeric(rstud) &&
          length(rstud) == nrow(d) &&
          any(is.finite(rstud))) {

        rstud[!is.finite(rstud)] <- -Inf
        nres <- as.integer(ceiling(nmax * resids))

        if (nres > 0L) {
          ord <- order(rstud, decreasing = TRUE)
          keep_n <- min(nres, sum(is.finite(rstud)))
          keep_local <- ord[seq_len(keep_n)]
          ir_case <- d$.case_id[keep_local]
        }
      }
    }
  }

  i_case <- unique(c(i_case, ir_case))

  # random fill
  nrand <- as.integer(ceiling(nmax * (1 - pinc - resids)))
  if (nrand < 0L) nrand <- 0L

  remaining <- d[!(d$.case_id %in% i_case), , drop = FALSE]
  srand <- simple_sample_ids(remaining$.case_id, nrand)

  unique(c(i_case, srand))
}
