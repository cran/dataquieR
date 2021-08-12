#' Function to plot histograms added by empirical cumulative distributions
#' for subgroups
#'
#' @description
#' Function to identify inadmissible measurements according to hard limits
#' (multiple variables)
#'
#' @details
#' # ALGORITHM OF THIS IMPLEMENTATION:
#'
#' - Select all variables of type float or integer in the study data
#' - Remove missing codes from the study data (if defined in the metadata)
#' - Remove measurements deviating from limits defined in the metadata
#' - Plot histograms
#'   - If group_vars is specified by the user, distributions within
#'     group-wise ecdf are presented.
#'
#' @export
#'
#' @param resp_vars [variable list] the names of the measurement variables
#' @param group_vars [variable list] the name of the observer, device or
#'                                   reader variable
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#'
#' @return A [list] with:
#'   - `SummaryPlots`: [list] of [ggplot]s for each response variable in
#'                     `resp_vars`.
#'
#' @importFrom ggpubr ggarrange
#' @importFrom ggplot2 ggplot aes_string geom_histogram coord_flip labs
#'                     theme_minimal theme element_text scale_color_manual
#'                     stat_ecdf labs element_blank geom_bar
#' @importFrom stats na.omit
#' @seealso
#' [Online Documentation](
#' https://dataquality.ship-med.uni-greifswald.de/VIN_acc_impl_distributions.html
#' )
acc_distributions <- function(resp_vars = NULL, label_col, group_vars = NULL,
                              study_data, meta_data) {

  # Preps ----------------------------------------------------------------------

  # map meta to study
  util_prepare_dataframes()

  util_correct_variable_use("resp_vars",
    allow_null = TRUE,
    allow_more_than_one = TRUE,
    allow_any_obs_na = TRUE,
    need_type = "integer | float"
  )

  util_correct_variable_use("group_vars",
    allow_null = TRUE,
    allow_more_than_one = FALSE,
    allow_any_obs_na = TRUE,
    need_type = "!float"
  )

  rvs <- resp_vars

  # no variables defined?
  if (length(rvs) == 0) {
    util_warning(
      paste0(
        "All variables defined to be integer or float in the metadata are used"
      ), applicability_problem = FALSE
    )
    rvs <- meta_data[[label_col]][meta_data$DATA_TYPE %in%
                                    c("integer", "float")]
    rvs <- intersect(rvs, colnames(ds1))
    if (length(rvs) == 0) {
      util_error("No suitable variables were defined.",
                 applicability_problem = TRUE)
    }
  }

  # Label assignment -----------------------------------------------------------
  # temporary study data

  if (length(group_vars) > 0) {

    # all labelled variables
    levlabs <- meta_data$VALUE_LABELS[meta_data[[label_col]] %in% group_vars]

    if (any(grepl("=", levlabs) | is.na(levlabs))) {
      # any variables without labels?
      if (any(is.na(levlabs))) {
        util_warning(paste0(
          "Variables: ", paste0(group_vars[is.na(levlabs)], collapse = ", "),
          " have no assigned labels and levels."
        ), applicability_problem = TRUE)
      }

      # only variables with labels
      gvs_ll <- group_vars[!is.na(levlabs)]
      gvs_ll <- gvs_ll[match(gvs_ll,
                             meta_data[[label_col]][
                               meta_data[[label_col]] %in% group_vars])]
      levlabs <- levlabs[!is.na(levlabs)]

      for (i in seq_along(gvs_ll)) {
        ds1[[gvs_ll[i]]] <- util_assign_levlabs(
          variable = ds1[[gvs_ll[i]]],
          string_of_levlabs = levlabs[i],
          splitchar = SPLIT_CHAR,
          assignchar = " = "
        )
      }
    }
  }

  # variables being not numeric ------------------------------------------------
  whicharenum <- vapply(FUN.VALUE = logical(1), ds1[, rvs, drop = FALSE],
                        function(x) is.numeric(x))
  if (any(!whicharenum)) {
    util_warning(paste0(
      "Variables ", paste0(rvs[!whicharenum], collapse = ", "),
      " are not of type float or integer and will be removed from analyses."
    ), applicability_problem = TRUE)
    rvs <- rvs[whicharenum]
    if (length(rvs) == 0) {
      util_warning("No variables left to analyse", applicability_problem = TRUE)
      return(list(SummaryPlots = list()))
    }
  }

  # variables being completely NA or character? --------------------------------
  which_all_na <- vapply(FUN.VALUE = numeric(1),
                       ds1[, rvs, drop = FALSE], util_only_NAs)
  which_all_na <- as.logical(which_all_na)

  if (any(which_all_na)) {
    util_warning(paste0(
      "Variables ", paste0(rvs[which_all_na], collapse = ", "),
      " contain NAs only and will be removed from analyses."
    ), applicability_problem = TRUE)
    rvs <- rvs[!which_all_na]
    if (length(rvs) == 0) {
      util_warning("No variables left to analyse",
                   applicability_problem = TRUE)
      return(list(SummaryPlots = list()))
    }
  }

  # variables with one unique value? -------------------------------------------
  whichunique <- vapply(FUN.VALUE = logical(1), ds1[, rvs, drop = FALSE],
                        util_check_one_unique_value)

  if (any(whichunique)) {
    util_warning(paste0(
      "Variables ", paste0(rvs[whichunique], collapse = ", "),
      " contain only one value and will be removed from analyses."
    ), applicability_problem = TRUE)
    rvs <- rvs[!whichunique]
    if (length(rvs) == 0) {
      util_warning("No variables left to analyse",
                   applicability_problem = TRUE)
      return(list(SummaryPlots = list()))
    }
  }

  # Output ---------------------------------------------------------------------
  plot_list <- lapply(setNames(nm = rvs), function(rv) {
    ds1 <- ds1[!(is.na(ds1[[rv]])), , FALSE]

    # if factor handle as numeric
    if (is.factor(ds1[[rv]])) {
      ds1[[rv]] <- util_as_numeric(ds1[[rv]])
    }

    # Calculation of values relevant for plot area ---------------------------
    ### Define bounds for graph
    minimum <- min(ds1[[rv]], na.rm = TRUE)
    maximum <- max(ds1[[rv]], na.rm = TRUE)

    # If study data are not empty: xlims are always defined and of length 2!!!
    xlims <- c(minimum, maximum)

    # expand plot area
    if (maximum > 0) {
      maxx <- 1.1 * xlims[2]
    } else {
      maxx <- 0.9 * xlims[2]
    }

    if (minimum > 0) {
      minx <- 0.9 * xlims[1]
    } else {
      minx <- 1.1 * xlims[1]
    }

    if (minimum == 0) {
      minx <- minimum - max(1, floor(0.1 * diff(xlims)))
    }

    # differentiate continuous from discrete variables -----------------------
    if (!(all(ds1[[rv]] %% 1 == 0, na.rm = TRUE)) |
      length(unique(ds1[[rv]])) > 30) { # continuous or integer with more than
                                        # 30 values

      # Freedman-Diaconis (2 * IQR(data) / length(data)^(1/3)): optimal width
      # restricted to the data within limits!
      thedata <- ds1[[rv]][ds1[[rv]] >= xlims[1] & ds1[[rv]] <= xlims[2]]

      bw <- (2 * IQR(thedata, na.rm = TRUE) / length(thedata) ^ (1 / 3))

      # steps within hard limits (rounded according modulo division to
      # meet limits)
      by_x <- diff(xlims) / (diff(xlims) %/% bw)

      # breaks must be within hard limits (old: breakswithin <-
      #                                          seq(xlims[1], xlims[2],
      #                                              by = by_x))
      breakswithin <- c(minimum - by_x,
                        seq(minimum, maximum, by = by_x), maximum + by_x)

      # breaks outside plausis (always the case since minx/maxx outside limits)
      breakslower <- seq(xlims[1], minx, by = -by_x)
      breaksupper <- seq(xlims[2], maxx, by = by_x)

      # rounding
      breaks_x <- round(unique(c(breakslower, breakswithin, breaksupper)), 3)
      # if no values below/above
      breaks_x <- breaks_x[!is.na(breaks_x)]
    } else {# so far no good solution, not used
      breaks_x <- unique(ds1[[rv]][!(is.na(ds1[[rv]]))])
      breaks_x <- breaks_x[order(breaks_x)]
    }

    if (length(unique(breaks_x)) > 10000) {
      likely1 <- ds1[util_looks_like_missing(ds1[[rv]]), rv, TRUE]
      likely2 <- c(max(ds1[[rv]],
                       na.rm = TRUE),
                   min(ds1[[rv]],
                       na.rm = TRUE))
      likely <- intersect(likely1, likely2)
      if (length(likely) == 0)
        likely <- likely2
      util_warning(
        c("For %s, I have %d breaks. Did you forget to specify some missing",
          "codes (%s)? Will arbitrarily reduce the number of breaks below",
          "10000 to avoid rendering problems."),
        dQuote(rv), length(unique(breaks_x)), paste0(dQuote(likely
        ), collapse = " or "),
        applicability_problem = FALSE
      )
      while (length(unique(breaks_x)) > 10000) {
        breaks_x <- breaks_x[!is.na(breaks_x)]
        breaks_x <- c(min(breaks_x), breaks_x[c(TRUE, FALSE)], max(breaks_x))
      }
      util_warning(
        paste0("For %s. Will arbitrarily reduced the number of breaks to ",
          "%d <= 10000 to avoid rendering problems.", collapse = ""),
        dQuote(rv), length(unique(breaks_x)),
        applicability_problem = FALSE)
    }

    # building the plot -------------------------------------------------------
    txtspec <- element_text(
      colour = "black", hjust = .5,
      vjust = .5, face = "plain"
    )
    hex_code <- c(
      "#000000", "#B0B0B0", "#E69F00", "#56B4E9", "#009E73",
      "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#8C510A"
    )

    if (!all(util_is_integer(ds1[[rv]]), na.rm = TRUE)) {
      p <- ggplot(data = ds1, aes_string(x = rv)) +
        geom_histogram(breaks = unique(breaks_x), fill = "#B0B0B0",
                       color = "lightgrey") +
        coord_flip(xlim = c(floor(minx), ceiling(maxx))) +
        labs(x = paste0(rv), y = "") +
        theme_minimal() +
        theme(
          title = txtspec,
          axis.text.x = txtspec,
          axis.text.y = txtspec,
          axis.title.x = txtspec,
          axis.title.y = txtspec
        )

      if (length(group_vars) > 0) {
        pp <- ggplot(data = ds1, aes_string(x = rv, colour = group_vars)) + {
            if (length(unique(ds1[[group_vars]])) <= 10)
              scale_color_manual(values = hex_code)
          } +
          stat_ecdf(geom = "step") +
          labs(x = "", y = paste0("ECDF: ", rv)) +
          theme_minimal() +
          theme(
            title = txtspec,
            axis.text.x = txtspec,
            axis.text.y = txtspec,
            axis.title.x = txtspec,
            axis.title.y = txtspec,
            legend.title = element_blank()
          )

        P <- ggarrange(p, pp, labels = c("A", "B"))
      } else {
        P <- p
      }
    }


    if (all(util_is_integer(ds1[[rv]]), na.rm = TRUE)) {
      p <- ggplot(ds1, aes_string(x = ds1[[rv]])) +
        geom_bar(fill = "gray") +
        coord_flip(xlim = c(floor(minx), ceiling(maxx))) +
        labs(x = paste0(rv), y = "") +
        theme_minimal() +
        theme(
          title = txtspec,
          axis.text.x = txtspec,
          axis.text.y = txtspec,
          axis.title.x = txtspec,
          axis.title.y = txtspec
        )

      if (length(group_vars) > 0) {
        pp <- ggplot(data = ds1, aes_string(x = rv, colour = group_vars)) + {
            if (length(unique(ds1[[group_vars]])) <= 10)
              scale_color_manual(values = hex_code)
          } +
          stat_ecdf(geom = "step") +
          labs(x = "", y = paste0("ECDF: ", rv)) +
          theme_minimal() +
          theme(
            title = txtspec,
            axis.text.x = txtspec,
            axis.text.y = txtspec,
            axis.title.x = txtspec,
            axis.title.y = txtspec,
            legend.title = element_blank()
          )

        P <- ggarrange(p, pp, labels = c("A", "B"))
      } else {
        P <- p
      }
    }

    util_set_size(P)
  })

  return(list(SummaryPlotList = plot_list))
}
