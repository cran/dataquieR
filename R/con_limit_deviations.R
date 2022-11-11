#' Detects variable values exceeding limits defined in metadata
#'
#' @description
#' ## APPROACH
#'
#' Inadmissible numerical values can be of type integer or float. This
#' implementation requires the definition of intervals in the metadata to
#' examine the admissibility of numerical study data.
#'
#' This helps identify inadmissible measurements according to
#' hard limits (for multiple variables).
#'
#' @details
#' ### ALGORITHM OF THIS IMPLEMENTATION:
#'
#'  - Remove missing codes from the study data (if defined in the metadata)
#'  - Interpretation of variable specific intervals as supplied in the metadata.
#'  - Identification of measurements outside defined limits. Therefore two
#'    output data frames are generated:
#'    - on the level of observation to flag each deviation, and
#'    - a summary table for each variable.
#'  - A list of plots is generated for each variable examined for limit
#'    deviations. The histogram-like plots indicate respective limits as well
#'    as deviations.
#'  - Values exceeding limits are removed in a data frame of modified study data
#'
#' For [con_detection_limits], The default for the limits argument differs and
#' is here "DETECTION_LIMITS"
#'
#' @export
#'
#' @param resp_vars [variable list] the name of the measurement variables
#' @param study_data [data.frame] the data frame that contains the measurements
#' @param meta_data [data.frame] the data frame that contains metadata
#'                               attributes of study data
#' @param label_col [variable attribute] the name of the column in the metadata
#'                                       with labels of variables
#' @param limits [enum] HARD_LIMITS | SOFT_LIMITS | DETECTION_LIMITS. what
#'                                             limits from metadata to check for
#'
#' @importFrom ggplot2 ggplot geom_histogram scale_fill_manual coord_flip labs
#'                     theme_minimal theme geom_bar geom_vline annotate
#' @importFrom stats setNames IQR
#'
#' @return a list with:
#'   - `FlaggedStudyData` [data.frame] related to the study data by a 1:1
#'                                   relationship, i.e. for each observation is
#'                                   checked whether the value is below or above
#'                                   the limits.
#'   - `SummaryTable` [data.frame] summarizes limit deviations for each
#'                                 variable.
#'   - `SummaryPlotList` [list] of [ggplot]s The plots for each variable are
#'                            either a histogram (continuous) or a
#'                            barplot (discrete).
#'   - `ModifiedStudyData` [data.frame]  If the function identifies limit
#'                                     deviations, the respective values are
#'                                     removed in `ModifiedStudyData.`
#'   - `ReportSummaryTable`: heatmap-like data frame about limit violations
#'
#' @seealso
#' - [con_detection_limits]
#' - [Online Documentation](
#' https://dataquality.ship-med.uni-greifswald.de/VIN_con_impl_limit_deviations.html
#' )
#'
#' @examples
#' load(system.file("extdata", "study_data.RData", package = "dataquieR"))
#' load(system.file("extdata", "meta_data.RData", package = "dataquieR"))
#'
#' # make things a bit more complicated for the function, giving datetimes
#' # as numeric
#' study_data[,
#'   vapply(study_data, inherits, "POSIXct", FUN.VALUE = logical(1))] <-
#'   lapply(study_data[, vapply(study_data, inherits, "POSIXct",
#'   FUN.VALUE = logical(1))], as.numeric)
#'
#' MyValueLimits <- con_limit_deviations(
#'   resp_vars = NULL,
#'   label_col = "LABEL",
#'   study_data = study_data,
#'   meta_data = meta_data,
#'   limits = "HARD_LIMITS"
#' )
#'
#' names(MyValueLimits$SummaryPlotList)
#'
#' MyValueLimits <- con_limit_deviations(
#'   resp_vars = c("QUEST_DT_0"),
#'   label_col = "LABEL",
#'   study_data = study_data,
#'   meta_data = meta_data,
#'   limits = "HARD_LIMITS"
#' )
#'
#' MyValueLimits$SummaryPlotList$QUEST_DT_0
con_limit_deviations <- function(resp_vars = NULL, label_col, study_data,
                                 meta_data, limits = c(
                                   "HARD_LIMITS", "SOFT_LIMITS",
                                   "DETECTION_LIMITS"
                                 )) {
  rvs <- resp_vars

  infix <- unlist(strsplit(limits, "_"))[1]

  # Preps ----------------------------------------------------------------------

  # map meta to study
  util_prepare_dataframes()

  util_correct_variable_use("resp_vars",
    allow_more_than_one = TRUE,
    allow_null = TRUE,
    allow_any_obs_na = TRUE,
    need_type = "integer|float|datetime"
  )

  # which limits?
  LIMITS <- toupper(match.arg(limits))

  # variables correct?
  # util_correct_variable_use("resp_variables", role = "response_vars")

  # no variables defined?
  if (length(rvs) == 0) {
    if (all(is.na(meta_data[[LIMITS]]))) {
      util_error(paste0("No Variables with defined ", LIMITS, "."),
                 applicability_problem = TRUE)
    } else {
      util_warning(paste0("All variables with ", LIMITS,
                          " in the metadata are used."),
                   applicability_problem = TRUE)
      rvs <- meta_data[[label_col]][!(is.na(meta_data[[LIMITS]]))]
    }
  } else {
    # limits defined at all?
    if (all(is.na(meta_data[[LIMITS]][meta_data[[label_col]] %in% rvs]))) {
      util_error(paste0("No Variables with defined ", LIMITS, "."),
                 applicability_problem = TRUE)
    }
    # no limits for some variables?
    rvs2 <- meta_data[[label_col]][!(is.na(meta_data[[LIMITS]])) &
                                     meta_data[[label_col]] %in% rvs]
    if (length(rvs2) < length(unique(rvs))) {
      util_warning(paste0("The variables ", rvs[!(rvs %in% rvs2)],
                          " have no defined limits."),
                   applicability_problem = TRUE)
    }
    rvs <- rvs2
  }

  datetime_vars <- vapply(
    rvs,
    function(rv) {
      meta_data[["DATA_TYPE"]][meta_data[[label_col]] == rv] ==
        DATA_TYPES$DATETIME
    },
    logical(1)
  )

  # conversion of numeric handling needs a bit more coding, since it needs
  # an origin then. in other cases (Date, character, ...),
  # origin will be ignored by as.POSIXct

  ds1[, rvs[datetime_vars]] <-
    lapply(ds1[, rvs[datetime_vars], drop = FALSE], as.POSIXct, origin =
             min(as.POSIXct(Sys.Date()), 0))


  # remove rvs with non-matching data type
  var_matches_datatype <-
    vapply(FUN.VALUE = logical(1), ds1[, rvs, drop = FALSE],
                        function(x) is.numeric(x) || inherits(x, "POSIXct"))

  if (!all(var_matches_datatype)) {
    util_warning(paste0(
      "Variables ", paste0(rvs[!var_matches_datatype], collapse = ", "),
      " are neither numeric nor datetime and will be removed from analyses."
    ), applicability_problem = TRUE)
    rvs <- rvs[var_matches_datatype]
  }

  if (length(rvs) == 0) {
    util_error("No variables left, no limit checks possible.",
               applicability_problem = TRUE)
  }

  # interpret limit intervals
  imdf <- util_interpret_limits(mdata = meta_data)

  fsd_list <- lapply(setNames(nm = rvs), function(rv) {
    fsd <- ds1[, rv, drop = FALSE]

    ds1[!is.finite(ds1[[rv]]), rv] <- NA

    # Extract and interpret available metadata -------------------------------
    LOWER <- paste0(infix, "_LIMIT_LOW")
    ll <- imdf[[LOWER]][imdf[[label_col]] == rv]
    ll <- ifelse(is.infinite(ll), NA, ll)
    UPPER <- paste0(infix, "_LIMIT_UP")
    lu <- imdf[[UPPER]][imdf[[label_col]] == rv]
    lu <- ifelse(is.infinite(lu), NA, lu)

    if ((datetime_vars[[rv]])) {
      ll <- as.POSIXct(ll, origin = min(as.POSIXct(Sys.Date()), 0))
      lu <- as.POSIXct(lu, origin = min(as.POSIXct(Sys.Date()), 0))
    }

    # Fill summary DFs -------------------------------------------------------
    BELOW <- paste0(rv, "_below_", infix)
    ABOVE <- paste0(rv, "_above_", infix)
    OUT <- paste0(rv, "_OUT_", infix)

    if (!(is.na(ll))) {
      fsd[[BELOW]][!(is.na(fsd[[rv]]))] <-
        ifelse(as.numeric(fsd[[rv]][!(is.na(fsd[[rv]]))]) < ll, 1, 0)
    } else {
      fsd[[BELOW]][!(is.na(fsd[[rv]]))] <- 0
    }
    if (!(is.na(lu))) {
      fsd[[ABOVE]][!(is.na(fsd[[rv]]))] <-
        ifelse(as.numeric(fsd[[rv]][!(is.na(fsd[[rv]]))]) > lu, 1, 0)
    } else {
      fsd[[ABOVE]][!(is.na(fsd[[rv]]))] <- 0
    }

    return(fsd)
  }) # end lapply fsd

  fsd <-
    do.call(cbind.data.frame, c(unname(fsd_list), list(
      stringsAsFactors = FALSE)))

  plot_list <- lapply(setNames(nm = rvs), function(rv) {

    # Fill summary DFs -------------------------------------------------------
    BELOW <- paste0(rv, "_below_", infix)
    ABOVE <- paste0(rv, "_above_", infix)
    OUT <- paste0(rv, "_OUT_", infix)

    # Combine flag for plot
    ds1[[OUT]] <- pmax(fsd[[BELOW]], fsd[[ABOVE]], na.rm = TRUE)


    ds1[!is.finite(ds1[[rv]]), rv] <- NA

    # Extract and interpret available metadata -------------------------------
    LOWER <- paste0(infix, "_LIMIT_LOW")
    ll <- imdf[[LOWER]][imdf[[label_col]] == rv]
    ll <- ifelse(is.infinite(ll), NA, ll)
    UPPER <- paste0(infix, "_LIMIT_UP")
    lu <- imdf[[UPPER]][imdf[[label_col]] == rv]
    lu <- ifelse(is.infinite(lu), NA, lu)

    if (datetime_vars[[rv]]) {
      ll <- as.POSIXct(ll, origin = min(as.POSIXct(Sys.Date()), 0))
      lu <- as.POSIXct(lu, origin = min(as.POSIXct(Sys.Date()), 0))
    }

    # Calculation of values relevant for plot area ---------------------------
    # data extrema
    max_data <- max(ds1[[rv]], na.rm = TRUE)
    min_data <- min(ds1[[rv]], na.rm = TRUE)

    ### Define bounds for graph
    minx <- min(c(min_data, ll), na.rm = TRUE)
    maxx <- max(c(max_data, lu), na.rm = TRUE)

    if (!datetime_vars[[rv]]) {
      # expand plot area
      inc <- floor(0.1 * abs(maxx - minx))
      if (inc < 1)
        inc <- 1
      minx <- minx - inc
      maxx <- maxx + inc
    }

    if (maxx == 0) {
      # expand plot area
      maxx <- (maxx + max(1, floor(0.1 * (maxx - minx))))
    }

    if (minx == 0) {
      # expand plot area
      minx <- (minx - max(1, floor(0.1 * (maxx - minx))))
    }

    # differentiate continuous from discrete variables -------------------------
    if (datetime_vars[[rv]] || !(all(ds1[[rv]] %% 1 == 0, na.rm = TRUE)) ||
      length(unique(ds1[[rv]])) > 20) {
      # continuous or integer with more than 20 values

      # Freedman-Diaconis (2 * IQR(data) / length(data)^(1/3)):
      # optimal width restricted to the data within limits!
      thedata <- ds1[[rv]]

      if (!is.na(ll)) {
        thedata <- thedata[ds1[[rv]] >= ll]
      }

      if (!is.na(lu)) {
        thedata <- thedata[ds1[[rv]] <= lu]
      }

      bw <- (2 * IQR(thedata, na.rm = TRUE) / length(thedata) ^ (1 / 3))
      if (bw == 0) bw <- 1

      # steps within hard limits
      # (rounded according modulo division to meet limits)
      dif <- as.numeric(maxx) - as.numeric(minx)
      byX <- dif / (dif %/% bw)

      # breaks must be within hard limits
      # (old: breakswithin <- seq(xlims[1], xlims[2], by = byX))
      breakswithin <- c(min_data - byX, seq(min_data, max_data, by = byX),
                        max_data + byX)

      # breaks outside plausis (always the case since minx/maxx outside limits)
      breakslower <- seq(minx, min_data, by = byX)
      breaksupper <- seq(max_data, maxx, by = byX)

      # rounding
      if (datetime_vars[[rv]]) {
        breaksX <- unique(c(breakslower, breakswithin, breaksupper))
      } else {
        breaksX <- round(unique(c(breakslower, breakswithin, breaksupper)), 3)
      }
      # if no values below/above
      breaksX <- unique(breaksX[!is.na(breaksX)])
    } else {
      breaksX <- unique(ds1[[rv]][!(is.na(ds1[[rv]]))])
    }

    breaksX <- sort(breaksX)

    if (length(unique(breaksX)) > 10000) {
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
        dQuote(rv), length(unique(breaksX)), paste0(dQuote(likely
                                                          ), collapse = " or "),
        applicability_problem = FALSE
      )
      while (length(unique(breaksX)) > 10000) {
        breaksX <- breaksX[!is.na(breaksX)]
        breaksX <- c(min(breaksX), breaksX[c(TRUE, FALSE)], max(breaksX))
      }
      util_warning(
        c("For %s. Will arbitrarily reduced the number of breaks to",
          "%d <= 10000 to avoid rendering problems."),
        dQuote(rv), length(unique(breaksX)),
        applicability_problem = FALSE)
    }

    # Generate ggplot-objects for placing and annotation of lines --------------
    # lower limit

    if (is.na(ll)) {
      # Create Line and Text
      lll <- geom_vline(
        xintercept = minx, color = "#999999", alpha = 1,
        linetype = "dotted"
      )
      tll <- annotate("text",
        x = minx, y = 0,
        label = paste0(
          "?lower ",
          tolower(infix),
          " limit?"
        ),
        color = "#999999", angle = 0, vjust = 1, hjust = 0
      )
    } else {
      # Detect number of cases below lower hard limit
      below_hl <- sum(ds1[[rv]] < ll, na.rm = TRUE)
      # Create Line and Text
      if (all(util_is_integer(ds1[[rv]]), na.rm = TRUE)) {
        xll <- ll - 0.5
      } else {
        xll <- ll
      }
      lll <- geom_vline(xintercept = xll, color = "#B2182B", alpha = 1)
      tll <- annotate("text",
        x = xll, y = 0,
        label = paste0(
          "limit ",
          tolower(infix),
          " low=", ll, "; Obs < LHL: ", below_hl
        ),
        color = "#B2182B", angle = 0, vjust = 1.5, hjust = 0
      )
    }

    # Upper limit
    if (is.na(lu)) {
      # Create Line and Text
      llu <- geom_vline(
        xintercept = maxx, color = "#999999", alpha = 1,
        linetype = "dotted"
      )
      tlu <- annotate("text",
        x = maxx, y = 0,
        label = paste0(
          "?upper ",
          tolower(infix),
          " limit?"
        ),
        color = "#999999", angle = 0, vjust = 1, hjust = 0
      )
    } else {
      # Detect number of cases above upper hard limit
      above_hl <- sum(ds1[[rv]] > lu, na.rm = TRUE)
      if (all(util_is_integer(ds1[[rv]]), na.rm = TRUE)) {
        xlu <- lu + 0.5
      } else {
        xlu <- lu
      }
      # Create Line and Text
      llu <- geom_vline(xintercept = xlu, color = "#B2182B", alpha = 1)
      tlu <- annotate("text",
        x = xlu, y = 0,
        label = paste0(
          "limit ",
          tolower(infix),
          " up=", lu, "; Obs > UHL: ", above_hl
        ),
        color = "#B2182B", angle = 0, vjust = -0.5, hjust = 0
      )
    }

    # building the plot --------------------------------------------------------
    txtspec <- element_text(
      colour = "black", # size = 16,
      hjust = .5, vjust = .5, face = "plain"
    ) # angle = 0,

    out_cols <- c("#2166AC", "#B2182B")
    names(out_cols) <- c("0", "1")

    if (datetime_vars[[rv]] || !(all(ds1[[rv]] %% 1 == 0, na.rm = TRUE)) ||
      length(unique(ds1[[rv]])) > 20) {
      # continuous or integer with more than 20 values
      #          if (!all(util_is_integer(ds1[[rv]]), na.rm = TRUE) || ) {
      breaks <- unique(breaksX)
      if (!datetime_vars[[rv]]) {
        myxlim <- c(floor(minx), ceiling(maxx))
      } else {
        myxlim <- c(minx, maxx)
        breaks <- as.POSIXct(breaks, origin = min(as.POSIXct(Sys.Date()), 0))
        myxlim <- as.POSIXct(myxlim, origin = min(as.POSIXct(Sys.Date()), 0))
      }
      # ds1 <- ds1[!is.na(ds1[[rv]]), , FALSE] -- too slow; NA observations
      # removal postponed
      p <- ggplot(data = ds1, aes(x = .data[[rv]], fill =
                                           factor(.data[[OUT]]))) +
        geom_histogram(breaks = breaks) +
        scale_fill_manual(values = out_cols, guide = "none") +
        coord_flip(xlim = myxlim) +
        labs(x = "", y = paste0(rv)) +
        theme_minimal() +
        theme(
          title = txtspec,
          axis.text.x = txtspec,
          axis.text.y = txtspec,
          axis.title.x = txtspec,
          axis.title.y = txtspec
        ) +
        # add line/text for lower limit
        lll +
        tll +
        # add line/text for upper limit
        llu +
        tlu
    } else {
      ds1 <- ds1[!is.na(ds1[[rv]]), , FALSE]
      p <- ggplot(ds1, aes(x = .data[[rv]], fill = factor(.data[[OUT]]))) +
        geom_bar() +
        scale_fill_manual(values = out_cols, guide = "none") +
        coord_flip(xlim = c(floor(minx), ceiling(maxx))) +
        labs(x = "", y = paste0(rv)) +
        theme_minimal() +
        theme(
          title = txtspec,
          axis.text.x = txtspec,
          axis.text.y = txtspec,
          axis.title.x = txtspec,
          axis.title.y = txtspec
        ) +
        # add line/text for lower limit
        lll +
        tll +
        # add line/text for upper limit
        llu +
        tlu
    }

    suppressWarnings({
      # NA observations can be removed, but should not be checked twice, see
      # above.
      # https://stackoverflow.com/a/51795017
      bp <- ggplot_build(p)
      w <- 2 * length(bp$layout$panel_params[[1]]$x$get_labels())
      if (w == 0) {
        w <- 10
      }
      w <- w + 10 +
        max(nchar(bp$layout$panel_params[[1]]$y$get_labels()),
            na.rm = TRUE)
      h <- 2 * length(bp$layout$panel_params[[1]]$y$get_labels())
      if (h == 0) {
        h <- 10
      }
      h <- h + 20

      p <- util_set_size(p, width_em = w, height_em = h)
    })

    return(p)
  }) # end lapply plot_list

  # remove violations of value limits
  msdf <- ds1

  for (current_rv in rvs) {
    if (HARD_LIMITS %in% names(imdf)) {
      # values below hard limit?
      minx1 <- imdf[[HARD_LIMIT_LOW]][imdf[[label_col]] == current_rv]
      minx2 <- suppressWarnings(min(msdf[[current_rv]], na.rm = TRUE))

      if (!is.na(minx1) & minx1 > minx2) {
        n_below <- sum(msdf[[current_rv]] < minx1, na.rm = TRUE)
        msdf[[current_rv]][msdf[[current_rv]] < minx1] <- NA
        util_warning(paste0("N = ", n_below, " values in ", current_rv,
                            " have been below %s and were removed."),
                     HARD_LIMIT_LOW,
                     applicability_problem = FALSE)
      }

      # values above hard limit?
      maxx1 <- imdf[[HARD_LIMIT_UP]][imdf[[label_col]] == current_rv]
      maxx2 <- suppressWarnings(max(msdf[[current_rv]], na.rm = TRUE))

      if (!is.na(maxx1) & maxx1 < maxx2) {
        n_above <- sum(msdf[[current_rv]] > maxx1, na.rm = TRUE)
        msdf[[current_rv]][msdf[[current_rv]] > maxx1] <- NA
        util_warning(paste0("N = ", n_above, " values in ", current_rv,
                            " have been above %s and were removed."),
                     HARD_LIMIT_UP,
                     applicability_problem = FALSE)
      }
    }
  }

  # add Summary Table with GRADING column
  name_bel <- paste0("Below ", infix, " (N)")
  name_abo <- paste0("Above ", infix, " (N)")

  sumtab <- lapply(setNames(nm = rvs), function(rv) {
    BELOW <- paste0(rv, "_below_", infix)
    ABOVE <- paste0(rv, "_above_", infix)

    r <- list(
      rv,
      sum(fsd[[BELOW]], na.rm = TRUE),
      round(sum(fsd[[BELOW]], na.rm = TRUE) / sum(!(is.na(fsd[[rv]]))) * 100,
            digits = 2),
      sum(fsd[[ABOVE]], na.rm = TRUE),
      round(sum(fsd[[ABOVE]], na.rm = TRUE) / sum(!(is.na(fsd[[rv]]))) * 100,
            digits = 2)
    )
    r <- as.data.frame(r, stringsAsFactors = FALSE)
    colnames(r) <- c(
      "Variables",
      paste0("Below ", infix, " (N)"),
      paste0("Below ", infix, " (%)"),
      paste0("Above ", infix, " (N)"),
      paste0("Above ", infix, " (%)")
    )
    r
  })

  sumtab <- do.call(rbind.data.frame, c(sumtab, stringsAsFactors = FALSE,
                                        deparse.level = 0, make.row.names =
                                          FALSE))

  sumtab$GRADING <- ifelse((sumtab[[name_bel]] > 0) | (sumtab[[name_abo]] > 0),
                           1, 0)

  heatmap_tab <- sumtab
  to_remove <- grep('%', colnames(heatmap_tab), fixed = TRUE)
  heatmap_tab <- heatmap_tab[, -to_remove]
  heatmap_tab$GRADING <- NULL
  colnames(heatmap_tab) <- gsub(" (N)", "", colnames(heatmap_tab),
                                fixed = TRUE)
  heatmap_tab$N <- vapply(rvs, function(rv) sum(!(is.na(fsd[[rv]]))),
                          FUN.VALUE = integer(1))

  class(heatmap_tab) <- union("ReportSummaryTable", class(heatmap_tab))

  return(list(FlaggedStudyData = fsd, SummaryTable = sumtab,
              ReportSummaryTable = heatmap_tab,
              SummaryPlotList =
                plot_list, ModifiedStudyData = msdf))
}

#' con_detection_limits
#' @inherit con_limit_deviations
#' @export
#' @seealso
#' - [con_limit_deviations]
#' - [Online Documentation](
#' https://dataquality.ship-med.uni-greifswald.de/VIN_con_impl_limit_deviations.html
#' )

con_detection_limits <- function(resp_vars = NULL, label_col, study_data,
                                 meta_data, limits = c(
                                   "DETECTION_LIMITS",
                                   "HARD_LIMITS", "SOFT_LIMITS"
                                 )) {
  con_limit_deviations(
    resp_vars = resp_vars, label_col = label_col, study_data = study_data,
    meta_data = meta_data, limits = match.arg(limits)
  )
}
