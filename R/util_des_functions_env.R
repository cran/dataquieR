# This file creates an environment with functions, that are used in the
# function `des_summary`

#' `util_des_functions_env` -- an environment for `des_summary` functions
#' @name util_des_functions_env
#' @noRd
util_des_functions_env <- new.env(parent = emptyenv())

#' Compute the median for categorical variables
#'
#' @name util_compute_median_cat
#' @param x a vector
#'
#' @seealso [`StackOverflow`](https://stackoverflow.com/a/7925162)
#' @author Richie Cotton
#'
#' @return median for categorical variables
#' @noRd
util_des_functions_env$util_compute_median_cat <- function(x) {
  levs <- levels(x)
  m <- median(as.integer(x), na.rm = TRUE)
  if(floor(m) != m)
  {
    util_message("Median is between two values; using the first one")
    m <- floor(m)
  }
  ordered(m, labels = levs, levels = seq_along(levs))
}


#' Compute mode for categorical variables
#'
#' @name util_compute_mode_cat
#' @param x a vector
#'
#' @return the first 3 mode and the number of the remaining ones
#' @noRd
util_des_functions_env$util_compute_mode_cat <- function(x) {
  x <- x[!is.na(x)]
  mode_value <-  unique(x)[tabulate(match(x, unique(x))) ==
                             max(tabulate(match(x, unique(x))))]
  if (length(mode_value) > 3) {
    l1 <- length(mode_value) - 3
    mode_value <- paste0(paste(c(head(mode_value, 3)), collapse = " "),
                         " and other ",
                         l1, " categories")
  } else if (length(mode_value) > 1 && length(mode_value) < 4) {
    mode_value <- paste(mode_value,collapse = " ")
  }
  mode_value <- format(mode_value)
  return(mode_value)
}


#' Compute mode for continuous variables (excluding datetime)
#'
#' @name util_compute_mode_contin
#' @param x a numeric vector
#'
#' @return the mode
#' @noRd
util_des_functions_env$util_compute_mode_contin<- function(x) {
  x <- x[!is.na(x)]
  mode_value <-  unique(x)[tabulate(match(x, unique(x))) ==
                             max(tabulate(match(x, unique(x))))]
  if(length(mode_value)==1) {
    r <- format(mode_value)
  } else  if(length(mode_value)==2) {
    mode_value <- paste(mode_value,collapse = " ")
    r <- format(mode_value)
  } else if (length(mode_value)>2) {
    n_mode <- length(mode_value)
    mode_value <- mode_value[1:2]
    n_mode_removed <- n_mode - length(mode_value)
    mode_value <- paste(mode_value,collapse = " ")
    r <- paste0(format(mode_value), " and other ",
                n_mode_removed ," values")
  }
  return(r)
}


#' Compute mode for datetime variables
#'
#' @name util_compute_mode_datetime
#' @param x a vector containing date time values
#'
#' @return the first 2 mode as date time and the number of the remaining ones
#' @noRd
util_des_functions_env$util_compute_mode_datetime <- function(x) {
  x <- x[!is.na(x)]
  mode_value <-  unique(x)[tabulate(match(x, unique(x))) ==
                             max(tabulate(match(x, unique(x))))]
  if(length(mode_value)==1) {
    r <- format(mode_value, usetz = TRUE)
  } else if(length(mode_value)==2) {
    mode_value <- format(mode_value, usetz = TRUE)
    mode_value <- paste(mode_value,collapse = " ")
    r <- format(mode_value, usetz = TRUE)
  } else if (length(mode_value)>2) {
    mode_value <- format(mode_value, usetz = TRUE)
    n_mode <- length(mode_value)
    mode_value<- mode_value[1]
    n_mode_removed <- n_mode - length(mode_value)
    mode_value <- paste(mode_value,collapse = " ")
    r <- paste0(format(mode_value, usetz = TRUE), " and other ",
                n_mode_removed ," dates")
  }
  return(r)
}


#' Compute the interquartile range (IQR), the first quartile (Q1) and the
#' third quartile (Q3) for ordinal variables
#'
#' @name util_compute_IQR_ord
#' @param x a vector
#'
#' @return a [list] with:
#'   - `IQR`: a string containing the range
#'   - `IQR_q1_q3`: a string containing Q1 and Q3
#'
#'
#'  a list containing the  and a string containing Q1 and Q3
#' @noRd
util_des_functions_env$util_compute_IQR_ord <- function(x) {
  #Only for ORDINAL vars
  q <- quantile(x, na.rm = TRUE, names = TRUE, type = 1)
  names(q) <- paste0("Q", seq_len(length(q)) - 1)
  q <- q[c("Q1", "Q3")]
  r <- prep_deparse_assignments(labels = as.character(q),
                                codes = names(q),
                                mode = "string_codes")
  iqr <- IQR(na.rm = TRUE, x = x, type = 1)
  r <- paste0(format(iqr), " (", r, ")")
  return(list("IQR" = iqr,
              "IQR_q1_q3" = r))
}


#' Compute the interquartile range (IQR), the first quartile (Q1) and the
#' third quartile (Q3) for numerical variables
#'
#' @name util_compute_IQR_contin
#' @param x a vector
#'
#' @return a [list] with:
#'   - `IQR`: a numeric value indicating the interquartile range
#'   - `IQR_q1_q3`: a string containing Q1 and Q3
#' @noRd
util_des_functions_env$util_compute_IQR_contin <- function (x) {
  q <- quantile(x, na.rm = TRUE, names = FALSE)
  names(q) <- paste0("Q", seq_len(length(q)) - 1)
  q <- q[c("Q1", "Q3")]
  r <- prep_deparse_assignments(labels = as.character(q),
                                codes = names(q),
                                mode = "string_codes")
  iqr <- IQR(na.rm = TRUE, x = x, type = 7)
  r<- paste0(format(iqr),
             " (", r, ")")
  rm(q)
  return(list("IQR" = iqr,
              "IQR_q1_q3" = r))
}


#' Compute the interquartile range (IQR), the first quartile (Q1) and
#' the third quartile (Q3) for datetime variables
#'
#' @name util_compute_IQR_datetime
#' @param x a vector
#'
#' @return a [list] with:
#'   - `IQR`: the interquartile range in seconds
#'   - `IQR_q1_q3`: a string containing Q1 and Q3
#' @noRd
util_des_functions_env$util_compute_IQR_datetime <- function(x) {
  q <- quantile(x, na.rm = TRUE, names = FALSE)
  names(q) <- paste0("Q", seq_len(length(q)) - 1)
  q <- q[c("Q1", "Q3")]
  r <- prep_deparse_assignments(labels = format(q, usetz = TRUE),
                                codes = names(q),
                                mode = "string_codes")
  iqr <- IQR(na.rm = TRUE, x = x,
             type = 7)
  #iqr1 <- format(as.difftime(iqr, units = "secs"), usetz = TRUE)


  iqr1 <- as.difftime(iqr, units = "secs")

#total_seconds <- lubridate::as.duration(
#  as.numeric(iqr1, units = "secs"))
#iqr1 <- util_des_functions_env$util_format_duration_human(start = total_seconds)

  iqr1 <-
    util_des_functions_env$util_compute_difftime_auto(iqr1)
  iqr1 <- format(iqr1, usetz = TRUE)

  r<- paste0(iqr1, " (", r, ")")
  rm(q)
  return(list("IQR" = iqr1,
              "IQR_q1_q3" = r))
}


#' Compute the Skewness
#'
#' @name util_compute_skewness
#' @param x a numeric vector
#'
#' @return the Skewness
#' @noRd
util_des_functions_env$util_compute_skewness <- function(x) {
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  skewness <- (sum((x - mean_x)^3, na.rm = TRUE) / (n * sd_x^3))
  return(skewness)
}


#' Compute SE.Skewness
#'
#' @name util_compute_SE_skewness
#' @param x a numeric vector
#'
#' @return the standard error of skewness
#' @noRd
util_des_functions_env$util_compute_SE_skewness <- function(x) {
  n <- length(x)
  se_skewness <- sqrt((6 * n * (n - 1)) /
                        ((n - 2) * (n + 1) * (n + 3)))
  return(se_skewness)
}


#' Compute Kurtosis
#'
#' @name util_compute_kurtosis
#' @param x a numeric vector
#'
#' @return the Kurtosis
#' @noRd
util_des_functions_env$util_compute_kurtosis <- function(x) {
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  kurtosis <- sum((x - mean_x)^4, na.rm = TRUE) / (n * sd_x^4) - 3
  return(kurtosis)
}


#' Compute the frequency of each category
#'
#' @name util_compute_frequency_table
#' @param x a vector
#'
#' @return a string of the categories and their frequency, separated by a pipe symbol
#' @noRd
util_des_functions_env$util_compute_frequency_table <- function(x) {
  # create a dataframe from the vector calculating the frequency of each value
  if(nrow(table(x)) > 1) {
    cont_tab <- as.data.frame(sort(table(x),
                                   decreasing = TRUE,
                                   na.last = FALSE))

  } else {
    cont_tab <- as.data.frame(table(x), na.last = FALSE)
  }

  cont_tab[[1]] <- sQuote(cont_tab[[1]]) # to prevent values from being treated as numeric
  # rename the table
  colnames(cont_tab) <- c("Value", "Freq")
  r <- prep_deparse_assignments(labels = cont_tab$Value,
                                cont_tab$Freq,
                                mode = "string_codes")
  r
}


#' Compute a distribution plot for categorical variables
#'
#' @name util_compute_graph_cat
#' @param x a vector
#'
#' @return  the plot in html code
#' @noRd
util_des_functions_env$util_compute_graph_cat <- function(x) {
  mrg <- 2
  r <- htmltools::plotTag({
    withr::local_par(list(
      mar = rep(mrg, 4),
      oma = rep(0, 4)
    ))
    barplot(table(x), main = NULL,
            xlab = NULL,
            ylab = NULL)},
    width = 250,
    height = 200,
    alt = paste("Histogram"))
  r <- as.character(r)
  r <- paste0('<div style="min-width: 300px">', r, "</div>")
  r
}


#' Compute a distribution plot for datetime variables
#'
#' @name util_compute_graph_datetime
#' @param x a vector
#'
#' @return  the plot in html code
#' @noRd
util_des_functions_env$util_compute_graph_datetime <- function(x) {
  mrg <- 2

  is_time_only <- inherits(x, "hms")
  x0 <- x[!is.na(x)]

  if (length(x0) == 0L) {
    return('<div style="min-width: 300px"><em>no data</em></div>')
  }

  if (is_time_only) {
    # ---- hms: in Sekunden seit Mitternacht, Achse selbst zeichnen ----
    sec <- as.numeric(x0) %% 86400

    br_raw <- suppressMessages(util_optimize_histogram_bins(x = sec,
                                                            nbins_max = 100))
    br <- unique(sort(unlist(br_raw)))
    if (length(br) < 2L) br <- pretty(sec, n = 10)

    # hübsche Ticks (in Sekunden), später als hms formatieren
    at <- pretty(range(br, na.rm = TRUE))

    r <- htmltools::plotTag({
      withr::local_par(list(mar = rep(mrg, 4), oma = rep(0, 4)))

      hist(sec,
           main = NULL, xlab = NULL, ylab = NULL,
           breaks = br, freq = TRUE, xaxt = "n")

      axis(1, at = at, labels = format(hms::as_hms(at)))
    },
    width = 250, height = 200, alt = "Histogram")

    r <- as.character(r)
    paste0('<div style="min-width: 300px">', r, "</div>")

  } else {
    # ---- POSIXct: echte Zeitachse, axis.POSIXct nutzen ----
    if (!inherits(x0, "POSIXt")) {
      util_error(
        "x must be POSIXct/POSIXlt or hms. Internal error, sorry, please report.")
    }

    breaks <- unlist(suppressMessages(util_optimize_histogram_bins(
      x = x,
      nbins_max = 100
    )))
    breaks <- util_parse_date(breaks)
    breaks <- unique(sort(breaks))
    breaks <- scales::pretty_breaks()(breaks)
    r <- htmltools::plotTag({
      withr::local_par(list(
        mar = rep(mrg, 4),
        oma = rep(0, 4)
      ))

      hist(x, main = NULL,
           xlab = NULL,
           ylab = NULL,
           breaks = breaks,
           freq = TRUE)},
      width = 250,
      height = 200,
      alt = paste("Histogram")) #, labs[[rv]]))
    r <- as.character(r)
    paste0('<div style="min-width: 300px">', r, "</div>")
  }
}



#' Compute a distribution plot for continuous variables (excluding datetime)
#'
#' @name util_compute_graph_cont
#' @param x a vector
#'
#' @return  the plot in html code
#' @noRd
util_des_functions_env$util_compute_graph_cont <- function(x) {
  mrg <- 2
  r <- htmltools::plotTag({
    withr::local_par(list(
      mar = rep(mrg, 4),
      oma = rep(0, 4)
    ))
    hist(x, main = NULL,
         xlab = NULL,
         ylab = NULL)},
    width = 250,
    height = 200,
    alt = paste("Histogram")) # of", labs[[rv]]))
  r <- as.character(r)
  r <- paste0('<div style="min-width: 300px">', r, "</div>")
  r

}


#' Move the first row of a data frame to its column names
#'
#' @name util_first_row_to_colnames
#' @param dfr [data.frame]
#'
#' @return [data.frame] with first row as column names
#' @noRd
util_des_functions_env$util_first_row_to_colnames <- function(dfr) {
  colnames(dfr) <- dfr[1, , TRUE]
  dfr <- tail(dfr, -1)
  dfr
}


#' Compute a plot containing the frequency table for categorical variables
#'
#' @name util_compute_graph_frequency_table
#' @param x a vector
#'
#' @return  the plot
#' @noRd
util_des_functions_env$util_compute_graph_frequency_table <- function(x) {
  # create a data frame from the vector calculating the frequency of each value
  if(nrow(table(x)) > 1) {
    cont_tab <- as.data.frame(sort(table(x),
                                   decreasing = TRUE,
                                   na.last = FALSE))

  } else {
    cont_tab <- as.data.frame(table(x), na.last = FALSE)
  }
  # to prevent values from being treated as numeric
  cont_tab[[1]] <- sQuote(cont_tab[[1]])
  # rename the table
  colnames(cont_tab) <- c("Value", "Freq")

  # in case of tables with more than 5 rows, sum last values (less frequent)
  if (nrow(cont_tab) > 5) {
    foot <- tail(cont_tab, -5)
    foot <- c("Others", sum(foot[[2]], na.rm = TRUE))
    cont_tab <- rbind(head(cont_tab, 5), foot)
  }
  # save the formatted table in r
  r <- paste0(util_formattable(util_des_functions_env$util_first_row_to_colnames(
    as.data.frame(t(cont_tab))),
    min_color = c(235, 235, 235),
    max_color = c(20, 20, 235)))
  rm(cont_tab)
  return(r)
}


#' A function to reduce a vector to the only one value that is not NA
#'
#' @name util_combine_cols_content
#' @param rw a vector, e.g., the row of a data frame with only one value and all other NAs
#' @param subject what the data content refers to
#'
#' @return  a value
#' @noRd
util_des_functions_env$util_combine_cols_content <- function(rw, subject) {
  to_keep <- !is.na(rw)
  if (sum(to_keep) > 1) {
    util_warning(c("It should not be possible for",
                   "%s to have more than one",
                   "result per variable -- internal error, please report."),
                 subject)
    NA
  } else {
    result <- rw[to_keep]
    names(result) <- NULL
    if(length(result)==0) {
      result <-  ""
    }
    result
  }
}

#' A function to format from secs to appropriate readable time
#'
#' @name util_compute_difftime_auto
#' @param x a vector
#'
#' @return  a string
#' @noRd
#
util_des_functions_env$util_compute_difftime_auto <- function(time_diff) {
  # check if it is an acceptable object
  if (!inherits(time_diff, "difftime")) {
    util_error(
      "Internal error, sorry. Please report.Input must be a 'difftime' object.")
  }
  # Convert to seconds
  total_seconds <- as.numeric(time_diff, units = "secs")

  # Define thresholds for switching units
  seconds_in_minute <- 60
  seconds_in_hour <- 60 * seconds_in_minute
  seconds_in_day <- 24 * seconds_in_hour
  seconds_in_year <- 365.25 * seconds_in_day
  seconds_in_month <- seconds_in_year / 12

  if (abs(total_seconds) < seconds_in_hour) {
    # If less than an hour, show in minutes (or seconds if very small)
    if (abs(total_seconds) < seconds_in_minute * 2) { # If less than 2 minutes
      return(paste(round(total_seconds, 2), "seconds"))
    } else {
      minutes <- as.numeric(time_diff, units = "mins")
      return(paste(round(minutes, 2), "minutes"))
    }
  } else if (abs(total_seconds) < seconds_in_day * 2) { # If less than 2 days
    hours <- as.numeric(time_diff, units = "hours")
    return(paste(round(hours, 2), "hours"))
  } else if (abs(total_seconds) < seconds_in_month * 2) { # If less than 2 months
    days <- as.numeric(time_diff, units = "days")
    return(paste(round(days, 2), "days"))
  } else if (abs(total_seconds) < seconds_in_year * 2) { # If less than 2 years
    # For months, lubridate's period is better for accuracy if specific dates are involved.
    # But for general display, we can approximate:
    months <- total_seconds / seconds_in_month
    return(paste(round(months, 2), "months"))
  } else {
    years <- total_seconds / seconds_in_year
    return(paste(round(years, 2), "years"))
  }
}


#' A function to format from two dates interval to appropriate readable time
#'
#' @name util_format_duration_human
#' @param x a vector
#'
#' @return  a string
#' @noRd
util_des_functions_env$util_format_duration_human <-
  function(start, end = NULL,
           units = list(
             year    = c("year", "years"),
             month   = c("month", "months"),
             week    = c("week", "weeks"),
             day     = c("day", "days"),
             hour    = c("hour", "hours"),
             minute  = c("minute", "minutes"),
             second  = c("second", "seconds"),
             and     = "and"
           )) {

    # Eingabeinterpretation
    if (!is.null(end)) {
      if (inherits(start, "hms")) {
        intv <- lubridate::interval(start, end, tzone = "UTC")
      } else {
        intv <- lubridate::interval(start, end)
      }
      p <- lubridate::as.period(intv)
    } else {
      if (inherits(start, "difftime")) {
        p <- lubridate::as.period(as.duration(start))
      } else if (inherits(start, "Duration")) {
        p <- lubridate::as.period(start)
      } else if (inherits(start, "Period")) {
        p <- start
      } else {
        util_error(c(
          "Internal error, sorry. Please report: Invalid input type. Provide",
          "two time points or a difftime/duration/period object."))
      }
    }

    # Zerlegung
    years   <- lubridate::year(p)
    months  <- lubridate::month(p)
    days    <- lubridate::day(p)
    hours   <- lubridate::hour(p)
    minutes <- lubridate::minute(p)
    seconds <- lubridate::second(p)
    #round seconds when there are more than 2 decimals
    if (seconds != 0) {
       seconds <- round(seconds, digits = 2)
    }


    # Wochen extrahieren
    weeks <- days %/% 7
    days  <- days %% 7

    # Pluralisierung
    pluralize <- function(value, labels) {
      if (value == 1) paste(value, labels[1])
      else if (value > 0) paste(value, labels[2])
      else NULL
    }

    # Liste bauen
    parts <- c(
      pluralize(years,   units$year),
      pluralize(months,  units$month),
      pluralize(weeks,   units$week),
      pluralize(days,    units$day),
      pluralize(hours,   units$hour),
      pluralize(minutes, units$minute),
      pluralize(seconds, units$second)
    )

    # Ausgabeformat
    if (length(parts) > 1) {
#      paste(paste(parts[-length(parts)], collapse = ", "), units$and, parts[length(parts)])
      paste(paste(parts, collapse = ", "))
    } else if (length(parts) == 1) {
      parts
    } else {
      paste("0", units$second[2])
    }
  }


