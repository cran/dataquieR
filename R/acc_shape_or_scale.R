#' Function to compare observed versus expected distributions
#'
#' @description
#' This implementation contrasts the empirical distribution of a measurement
#' variables against assumed distributions. The approach is adapted from the
#' idea of rootograms (Tukey 1977) which is also applicable for count data
#' (Kleiber and Zeileis 2016).
#'
#' @details
#' # ALGORITHM OF THIS IMPLEMENTATION:
#'   - This implementation is restricted to data of type float or integer.
#'   - Missing codes are removed from resp_vars (if defined in the metadata)
#'   - The user must specify the column of the metadata containing probability
#'     distribution (currently only: normal, uniform, gamma)
#'  - Parameters of each distribution can be estimated from the data or are
#'    specified by the user
#'  - A histogram-like plot contrasts the empirical vs. the technical
#'    distribution
#'
#' @param resp_vars [variable] the name of the continuous measurement variable
#' @param dist_col [variable attribute] the name of the variable attribute in
#'                                      meta_data that provides the expected
#'                                      distribution of a study variable
#' @param guess [logical] estimate parameters
#' @param par1 [numeric] first parameter of the distribution if applicable
#' @param par2 [numeric] second parameter of the distribution if applicable
#' @param end_digits [logical] internal use. check for end digits preferences
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#'
#' @return a list with:
#'   - `SummaryData`: [data.frame] underlying the plot
#'   - `SummaryPlot`: [ggplot2] probability distribution plot
#'   - `SummaryTable`: [data.frame] with the columns `Variables` and `GRADING`
#'
#' @export
#' @importFrom MASS fitdistr
#' @importFrom MultinomialCI multinomialCI
#' @importFrom ggplot2 ggplot theme_minimal geom_bar aes scale_fill_manual
#'                     geom_line geom_hline xlab scale_color_manual
#'                     geom_errorbar
#' @importFrom stats dunif dgamma dnorm punif  pgamma pnorm median
#' @seealso
#' [Online Documentation](
#' https://dataquality.ship-med.uni-greifswald.de/VIN_acc_impl_shape_or_scale.html
#' )
acc_shape_or_scale <- function(resp_vars, dist_col, guess, par1, par2,
                               end_digits, label_col, study_data, meta_data) {

  ###########################
  # STOPS, PREPS AND CHECKS #
  ###########################

  # map meta to study
  util_prepare_dataframes()

  # correct variable use?
  util_correct_variable_use("resp_vars")

  if (missing(dist_col)) {
    dist_col <- DISTRIBUTION
    util_warning(
      c("A column of the metaddata specifying the distributions has",
      "not been specified. Trying the default %s."),
      dQuote(DISTRIBUTION), applicability_problem = TRUE)
  }

  if (!(dist_col %in% colnames(meta_data))) {
    util_error("Did not find variable attribute %s in the meta_data",
                       dist_col, applicability_problem = TRUE)
  }

  if (missing(guess) || is.na(guess[[1]])) {
    if (!missing(guess) && length(guess) > 1) {
      util_warning("Have more than one value for guess, use the first one only",
                   applicability_problem = TRUE)
    }
    if (!missing(par1) && !missing(par2)) {
      guess <- FALSE
      util_warning("Since parameters were specified: 'guess' is set to false",
                   applicability_problem = TRUE)
    } else {
      guess <- TRUE
    }
  }

  if (!(missing(end_digits)) && length(end_digits) > 1) {
    util_warning(
      c("end_digits should be a scalar logical value. Have more than one",
        "value, use the first one only"),
      applicability_problem = TRUE)
    end_digits <- end_digits[[1]]
  }

  if (!missing(guess) && length(guess) > 1) {
    util_warning(
      c("guess should be a scalar logical value.",
        "Have more than one value, use the first one only"),
      applicability_problem = TRUE)
    guess <- guess[[1]]
  }

  if (!missing(par1) && length(par1) > 1) {
    util_warning(
      c("par1 should be a scalar numeric value. Have more than one value,",
        "use the first one only"),
      applicability_problem = TRUE)
    par1 <- par1[[1]]
  }

  if (!missing(par2) && length(par2) > 1) {
    util_warning(
      c("par2 should be a scalar numeric value.",
        "Have more than one value, use the first one only"),
      applicability_problem = TRUE)
    par2 <- par2[[1]]
  }

  if (!missing(guess) && !all(is.logical(guess))) {
    guess <- suppressWarnings(as.logical(guess))
    if ((!all(is.logical(guess))) || any(is.na(guess))) {
      util_error("guess should be a logical value",
                 applicability_problem = TRUE)
    }
  }

  if (!missing(end_digits) && !all(is.logical(end_digits))) {
    end_digits <- suppressWarnings(as.logical(end_digits))
    if ((!all(is.logical(end_digits))) || any(is.na(end_digits))) {
      util_error("end_digits should be a logical value",
                 applicability_problem = TRUE)
    }
  }

  if (!missing(par1) && !all(is.numeric(par1))) {
    par1 <- suppressWarnings(as.numeric(par1))
    if ((!all(is.numeric(par1))) || any(is.na(par1))) {
      util_error("par1 should be a numeric value",
                 applicability_problem = TRUE)
    }
  }

  if (!missing(par2) && !all(is.numeric(par2))) {
    par2 <- suppressWarnings(as.numeric(par2))
    if ((!all(is.numeric(par2))) || any(is.na(par2))) {
      util_error("par2 should be a numeric value",
                 applicability_problem = TRUE)
    }
  }

  if (length(ds1[[resp_vars]]) == 0L || mode(ds1[[resp_vars]]) != "numeric") {
    util_error("resp_vars == '%s' must be a non-empty numeric variable",
               resp_vars, applicability_problem = TRUE)
  }

  # missings in resp_vars?
  n_prior <- dim(ds1)[1]
  ds1 <- ds1[!(is.na(ds1[[resp_vars]])), ]
  n_post <- dim(ds1)[1]

  if (n_post < n_prior) {
    util_warning(paste0("Due to missing values in ", resp_vars, " ",
                        n_prior - n_post,
      " observations were deleted.",
      collapse = " "
    ), applicability_problem = FALSE)
  }

  if (guess == FALSE && (missing(par1) || missing(par2) ||
                         !is.finite(par1) || !is.finite(par2))) {
    util_error(c("Since 'guess' is not true finite numerical",
                 "parameters must be prespecified"),
               applicability_problem = TRUE)
  }

  if (missing(end_digits)) {
    end_digits <- FALSE
  }

  if (
    !is.character(meta_data[meta_data[[label_col]] == resp_vars, dist_col]) ||
    is.na(meta_data[meta_data[[label_col]] == resp_vars, dist_col]) ||
    trimws(meta_data[meta_data[[label_col]] == resp_vars, dist_col]) == "") {
    util_error(paste0("No distribution specified for ", resp_vars, " in ",
                      dist_col, collapse = ""),
               applicability_problem = TRUE)
  }

  dist <- paste0(meta_data[meta_data[[label_col]] == resp_vars, dist_col])

  if (end_digits == TRUE) {
    dist <- "uniform"
  }

  if (guess && dist == "uniform") {
    par1 <- min(ds1[[resp_vars]], na.rm = TRUE)
    par2 <- max(ds1[[resp_vars]], na.rm = TRUE)
  }

  if (guess && dist %in% c("normal", "gamma")) {
    suppressWarnings(par1 <- fitdistr(ds1[[resp_vars]],
                                      densfun = dist)$estimate[1])
    suppressWarnings(par2 <- fitdistr(ds1[[resp_vars]],
                                      densfun = dist)$estimate[2])
  }

  # assign densities/probability arguments
  d.fun <- switch(dist, uniform = dunif, gamma = dgamma, normal = dnorm, NULL)
  p.fun <- switch(dist, uniform = punif, gamma = pgamma, normal = pnorm, NULL)

  if (is.null(d.fun)) {
    util_error(sprintf("This distribution '%s' is not supported yet...", dist),
               applicability_problem = TRUE)
  }

  # create histogram
  h1 <- graphics::hist(ds1[[resp_vars]], plot = FALSE)
  nobs <- sum(h1$counts)

  if (dist == "uniform" && (par1 < min(h1$breaks) | par2 > max(h1$breaks))) {
    breaks <- seq(min(par1, min(h1$breaks)), max(par2, max(h1$breaks)), by = 1)
    h1 <- graphics::hist(ds1[[resp_vars]], breaks = breaks, plot = FALSE)
    nobs <- sum(h1$counts)
  }
  # browser()
  if (end_digits == FALSE) {
    if (all(util_is_integer(ds1[[resp_vars]])) &
        length(unique(ds1[[resp_vars]])) < 20 & dist == "uniform") {
      df1 <- data.frame(
        INTERVALS = factor(unique(ds1[[resp_vars]])[
          order(unique(ds1[[resp_vars]]))]),
        COUNT = as.vector(table(ds1[[resp_vars]])),
        PROB = as.vector(table(ds1[[resp_vars]])) / nobs,
        EXP_PROB = round(1 / length(unique(ds1[[resp_vars]])), digits = 2),
        EXP_COUNT = nobs * 1 / length(unique(ds1[[resp_vars]])),
        LOWER_CL = multinomialCI(as.vector(table(ds1[[resp_vars]])),
                                 alpha = 0.05 / (length(h1$mids) - 1))[, 1],
        UPPER_CL = multinomialCI(as.vector(table(ds1[[resp_vars]])),
                                 alpha = 0.05 / (length(h1$mids) - 1))[, 2]
      )
    } else {
      df1 <- data.frame(
        INTERVALS = h1$mids,
        COUNT = h1$counts,
        PROB = h1$counts / nobs,
        EXP_PROB = min(diff(h1$mids)) * d.fun(h1$mids, par1, par2),
        EXP_COUNT = nobs * diff(p.fun(h1$breaks, par1, par2)),
        LOWER_CL = multinomialCI(h1$counts,
                                 alpha = 0.05 / (length(h1$mids) - 1))[, 1],
        UPPER_CL = multinomialCI(h1$counts,
                                 alpha = 0.05 / (length(h1$mids) - 1))[, 2]
      )
    }
  } else {
    ds1[[resp_vars]] <- factor(ds1[[resp_vars]], levels = 0:9)
    df1 <- data.frame(
      INTERVALS = factor(0:9),
      COUNT = as.vector(table(ds1[[resp_vars]])),
      PROB = as.vector(table(ds1[[resp_vars]])) / nobs,
      EXP_PROB = 0.1,
      EXP_COUNT = nobs * 0.1,
      LOWER_CL = multinomialCI(as.vector(table(ds1[[resp_vars]])),
                               alpha = 0.05 / (length(h1$mids) - 1))[, 1],
      UPPER_CL = multinomialCI(as.vector(table(ds1[[resp_vars]])),
                               alpha = 0.05 / (length(h1$mids) - 1))[, 2]
    )
  }

  df1$GRADING <- factor(ifelse(df1$EXP_PROB < df1$UPPER_CL &
                                 df1$EXP_PROB > df1$LOWER_CL, 0, 1))

  df1$PROB <- round(df1$PROB, digits = 2)
  df1$EXP_PROB <- round(df1$EXP_PROB, digits = 2)
  df1$EXP_COUNT <- round(df1$EXP_COUNT, digits = 2)
  df1$LOWER_CL <- round(df1$LOWER_CL, digits = 2)
  df1$UPPER_CL <- round(df1$UPPER_CL, digits = 2)


  x2 <- seq(floor(min(h1$breaks)), ceiling(max(h1$breaks)),
            length.out = length(h1$breaks) * 100)
  y_line <- min(diff(h1$breaks)) * d.fun(x2, par1, par2)

  if (end_digits) {
    y_line <- rep(0.1, length = length(x2))
  }

  if (all(util_is_integer(ds1[[resp_vars]])) &
      length(unique(ds1[[resp_vars]])) < 20 & dist == "uniform") {
    y_line <- rep(round(1 / length(unique(ds1[[resp_vars]])), digits = 2),
                  length = length(x2))
  }
  # browser()
  df2 <- data.frame(
    x2 = x2,
    y_line = y_line
  )

  grading_cols <- c("#2166AC", "#B2182B")
  names(grading_cols) <- c("0", "1")

  p1 <- ggplot(df1, aes(x = INTERVALS, y = PROB)) +
    theme_minimal() +
    geom_bar(aes(fill = GRADING), stat = "identity") +
    scale_fill_manual(values = grading_cols, guide = "none") +
    geom_errorbar(aes(ymin = LOWER_CL, ymax = UPPER_CL), width = 0.1) + {
      if (!(end_digits) & dist != "uniform") {
        geom_line(data = df2, aes(x = x2, y = y_line, color = "#E69F00"),
                  linewidth = 2)
      }
    } + {
      if (!(end_digits) & dist == "uniform") {
        geom_hline(yintercept = unique(y_line), color = "#E69F00",
                   linewidth = 2)
      }
    } + {
      if (!(end_digits) & dist == "uniform") {
        xlab("Values")
      }
    } + {
      if (end_digits) geom_hline(yintercept = 0.1, color = "#E69F00",
                                 linewidth = 2)
    } + {
      if (end_digits) xlab("End digits")
    } + scale_color_manual(values = c("#E69F00"), guide = "none")


  p1 <- util_set_size(p1);

  return(list(
    SummaryData = df1, SummaryPlot = p1,
    SummaryTable = data.frame(
      Variables = resp_vars,
      GRADING = 1 - prod(1 - util_as_numeric(df1$GRADING))
    )
  ))
}
