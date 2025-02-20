#' Function that calculated height and width values for `script_iframe`
#'
#' @param sizing_hints [list] containing information for setting
#'                           the size of the `iframe`
#'
#' @return a list with figure_type_id, w, and h; sizes are as `CSS`, existing
#'         elements are kept, `w_in_cm` and `h_in_cm` are estimates for the
#'         size in centimeters on a typical computer display (in 2024)
#' @keywords internal
util_finalize_sizing_hints <- function(sizing_hints) {
  original_sizing_hint <- sizing_hints
  if (!is.list(original_sizing_hint)) {
    original_sizing_hint <- list()
  }
  sizing_hints <- try(.util_finalize_sizing_hints(sizing_hints = sizing_hints))
  if (util_is_try_error(sizing_hints)) {
    sizing_hints <- list()
  }
  .w <- 14
  .uw <- "cm"
  .h <- 10
  .uh <- "cm"

  if (is.null(sizing_hints$w)) {
    sizing_hints$w <- sprintf("%s%s", as.character(.w), .uw)
  }

  if (is.null(sizing_hints$h)) {
    sizing_hints$h <- sprintf("%s%s", as.character(.h), .uh)
  }

  normalize_unit <- function(css_value) {
    ..v <- css_value
    is_sreen_rel <- FALSE
    if (all(grepl("^\\s*calc\\s*\\(.+\\)\\s*;?\\s*$", ..v))) {
      ..v <- gsub("^\\s*calc\\s*\\(", "", ..v)
      ..v<- gsub("\\)\\s*;?\\s*$", "", ..v)
      # use typical screen size
      ..v_old <- ..v
      ..v <- gsub("var\\(\\s*--screen-width\\s*\\)", "1920", ..v)
      ..v <- gsub("var\\(\\s*--screen-height\\s*\\)", "1080", ..v)
      if (..v != ..v_old) {
        is_sreen_rel <- TRUE
      }
    }
    ..v <- gsub(";\\s*$", "", ..v)
    .units <- c("cm", "mm", "in", "px", "pt", "pc", "em", "ex", "ch",
                "rem", "vw", "vh", "vmin", "vmax", "%")
    for (.unit in .units) {
      # 1px = 1/96th of 1in
      pattern <- paste0("((?:\\d*\\.)?\\d+)(", .unit, ")")
      ready <- FALSE
      while (!ready) {
        matches <- gregexpr(pattern, ..v)
        ready <- length(matches) == 0 || matches[[1]] == -1
        if (!ready) {
          e <- matches[[1]]
          new_val <- substr(..v, e, e+attr(e, "match.length")-1)
          match <- regexec(pattern, new_val)[[1]]
          from_number <- match[2]
          to_number <- from_number + attr(match, "match.length")[2]
          from_unit <- match[3]
          to_unit <- from_unit + attr(match, "match.length")[3]
          number <- as.numeric(substr(
            new_val, from_number, to_number - 1))
          unit <- .unit
          if (unit == "px") { # assume pixels according to https://www.w3schools.com/cssref/css_units.php
            if (!is_sreen_rel)
              util_message(c("Internal error, sorry: dataquieR",
                             "developers should never specify sizes in",
                             "pixels, because the size depends on the",
                             "output resolution, then. Please report."))
            unit <- "in"
            number <- number / 96
          } else if (unit == "pt") {
            unit <- "in"
            number <- number / 72
          } else if (unit == "pc") {
            unit <- "in"
            number <- number * 12 / 72
          } else if (unit == "em") { # relative units from here
            unit <- "in"
            number <- number * 12 / 72 # assuming a font size of 12pt
          } else if (unit == "ex") {
            unit <- "in"
            number <- number * 12 / 72 # assuming a font size of 12pt
          } else if (unit == "ch") {
            unit <- "in"
            number <- number * 12 / 72 # assuming a font size of 12pt
          } else if (unit == "rem") {
            unit <- "in"
            number <- number * 12 / 72 # assuming a font size of 12pt
          } else if (unit == "vw") {
            unit <- "in"
            number <- number * 1920 / 100
          } else if (unit == "vh") {
            unit <- "in"
            number <- number * 1080 / 100
          } else if (unit == "vmin") {
            unit <- "in"
            number <- number * 1080 / 100
          } else if (unit == "vmax") {
            unit <- "in"
            number <- number * 1920 / 100
          } else if (unit == "%") {
            unit <- "in"
            number <- number * 1920 / 100
          }

          number <- units::set_units(number, unit, mode = "standard")
          cm <- "dummy" # avoid notes about cm
          units(number) <- units::make_units(cm)
          ..v <- paste0(
            substring(..v, 1, e-1),
            units::drop_units(number),
            substring(..v, e + attr(e, "match.length"), 99999)
          )
        }
      }
    }
    if (length(..v) != 1 ||
        !grepl("^[0-9\\.\\*\\+\\-\\/ ]+$", ..v, perl = TRUE)) {
      util_error(
        c(
          "Internal error in util_finalize_sizing_hints, parsing final sizing",
          "hints, sorry. Please report: ",
          "Could not parse unit: %s"
        ),
        dQuote(css_value)
      )
    } else {
      ..v <- try(eval(parse(text = ..v), envir = baseenv()),
                 silent = TRUE)
      if (util_is_try_error(..v)) {
        util_error(
          c(
            "Internal error in util_finalize_sizing_hints, parsing final sizing",
            "hints, sorry. Please report: ",
            "Could not compute unit: %s"
          ),
          dQuote(css_value)
        )

      }
    }
    ..v
  }
  # "calc(var(--screen-height) * 0.65 * 1px);"
  # css does not allow space between number and unit; allowed units according to https://www.w3schools.com/cssref/css_units.php
  # cm, mm, in, px, pt, pc // relative: em, ex, ch, rem, vw, vh, vmin, vmax, %
  # relative, unsupported by the units package: px, pt, pc, em, ex, ch, rem, vw, vh vmin, vmax, %

  sizing_hints$w_in_cm <- normalize_unit(sizing_hints$w)
  sizing_hints$h_in_cm <- normalize_unit(sizing_hints$h)

  new_hints <- setdiff(
    names(original_sizing_hint),
    names(sizing_hints))
  sizing_hints[new_hints] <-
    original_sizing_hint[new_hints]

  sizing_hints
}

.util_finalize_sizing_hints<- function(sizing_hints) {

  ##CASE 1: bar_chart
  if (!is.null(sizing_hints$figure_type_id) &&
      sizing_hints$figure_type_id == "bar_chart") {

    #In case of missing hints, give an error
    if (is.null(sizing_hints$number_of_bars) ||
        is.null(sizing_hints$range)) {
      util_error(c("Internal error, sorry, please report:",
                 "Missing sizing hint values for %s"),
                 dQuote(sizing_hints$figure_type_id))
    }

   # BAR CHART CASE1: it is rotated (bars are horizontal)
    if (!is.null(sizing_hints$rotated) &&
        sizing_hints$rotated == TRUE) {
      if (!is.null(sizing_hints$number_of_bars) &&
          sizing_hints$number_of_bars < 15) {
        h <- 300
      } else if (!is.null(sizing_hints$number_of_bars) &&
                 sizing_hints$number_of_bars >= 15 &&
                 sizing_hints$number_of_bars < 101) {
        h <-  sizing_hints$number_of_bars * 22
      } else {
        h <- "calc(var(--screen-height) * 0.5 * 1px);"
      }
      w <- 600

      # final height in cm
      if(is.numeric(h)) {
        h <- round(h * 0.0264583333, digits = 4)
        h <- paste0(h, "cm;")
      }

      if (length(sizing_hints$no_char_numbers) != 1 ||
          is.na(sizing_hints$no_char_numbers) ||
          sizing_hints$no_char_numbers < 4) {
        sizing_hints$no_char_numbers <- 4
      }
      if (length(sizing_hints$no_char_vars) != 1 ||
          is.na(sizing_hints$no_char_vars) ||
          sizing_hints$no_char_vars < 4) {
        sizing_hints$no_char_vars <- 4
      }

      #final width in cm
      if(is.numeric(w)) {
        w <- w + 16 * sizing_hints$no_char_vars
        if(w > 1200) {
          w <- "calc(var(--screen-width) * 0.9 * 1px);"
        } else {
          w <- round(w * 0.0264583333, digits = 4)
          w <- paste0(w, "cm;")
        }
      }
    } else {
      # BAR CHART CASE2: it is NOT rotated (bars are vertical)
      if (!is.null(sizing_hints$number_of_bars) &&
          sizing_hints$number_of_bars < 16) {
        w <- 500
      } else if (!is.null(sizing_hints$number_of_bars) &&
                 sizing_hints$number_of_bars > 15 &&
                 sizing_hints$number_of_bars < 31) {
        w <- 800
      } else {
        w <- "calc(var(--screen-width) * 0.6* 1px);"
      }
      if (sizing_hints$range < 301 ||
          sizing_hints$number_of_bars < 21) {
        h <- 350
      } else if (sizing_hints$range > 300 &&
                 sizing_hints$range < 501) {
        h <- 450
      } else {
        h <- "calc(var(--screen-height) * 0.5 * 1px);"
      }

    if(length(sizing_hints$no_char_x) != 1 || sizing_hints$no_char_x < 4) {
      sizing_hints$no_char_x <- 4
    }
    if(length(sizing_hints$no_char_y) != 1 || sizing_hints$no_char_y < 4) {
      sizing_hints$no_char_y <- 4
    }
    # final height in cm
    if(is.numeric(h)) {
      h <- h + 16 * sizing_hints$no_char_y
      h <- round(h * 0.0264583333, digits = 4)
      h <- paste0(h, "cm;")
    }
    # final width in cm
    if(is.numeric(w)) {
      w <- w + 16 * sizing_hints$no_char_x
      w <- round(w * 0.0264583333, digits = 4)
      w <- paste0(w, "cm;")
    }
    }
    ##CASE 2: dot_mat
  } else if (!is.null(sizing_hints$figure_type_id) &&
             sizing_hints$figure_type_id == "dot_mat") {
    #In case of missing hints, give an error
    if (is.null(sizing_hints$number_of_vars) ||
        is.null(sizing_hints$number_of_cat)) {
      util_error(c("Internal error, sorry, please report:",
                   "Missing sizing hint values for %s"),
                 dQuote(sizing_hints$figure_type_id))
    }
    # DOT_MAT CASE1: it is rotated
    # (vars on the y axis, and categories of
    # missing or levels of groups are on the x axis)
    if (!is.null(sizing_hints$rotated) &&
        sizing_hints$rotated == TRUE) {
      if (sizing_hints$number_of_vars < 21) {
        h <- 250
      } else {
        h <- sizing_hints$number_of_vars * 12
      }

      if (sizing_hints$number_of_cat < 7) {
        w <- 300
      } else if (sizing_hints$number_of_cat > 6 &&
                 sizing_hints$number_of_cat < 16) {
        w <-  sizing_hints$number_of_cat * 45
      } else {
        w <- sizing_hints$number_of_cat * 35
      }

      if(sizing_hints$no_char_vars < 8) {
        sizing_hints$no_char_vars <- 8
      }
      if(sizing_hints$no_char_cat < 4) {
        sizing_hints$no_char_cat <- 4
      }
      # final height in cm
      if(is.numeric(h)) {
        h <- h + 16 * sizing_hints$no_char_cat
        h <- round(h * 0.0264583333, digits = 4)
        h <- paste0(h, "cm;")
      }
      # final width in cm
      if(is.numeric(w)) {
        w <- w + 16 * sizing_hints$no_char_vars
        w <- round(w * 0.0264583333, digits = 4)
        w <- paste0(w, "cm;")
      }
    } else {
      # DOT_MAT CASE2: it is NOT rotated
      # (vars on the x axis, and categories of
      # missing or levels of groups are on the y axis)
      if (sizing_hints$number_of_cat < 4) {
        h <- 150
      } else if (sizing_hints$number_of_cat >3 &&
                sizing_hints$number_of_cat < 15) {
        h <- 250
      } else {
        h <- 100 + sizing_hints$number_of_cat * 16
      }
      if (sizing_hints$number_of_vars < 7) {
        w <- 300
      } else if (6 < sizing_hints$number_of_vars &&
                 sizing_hints$number_of_vars < 15) {
        w <-  sizing_hints$number_of_vars * 48
      } else {
        w <- sizing_hints$number_of_vars * 35
      }

      if(sizing_hints$no_char_vars < 4) {
        sizing_hints$no_char_vars <- 4
      }
      if(sizing_hints$no_char_cat < 4) {
        sizing_hints$no_char_cat <- 4
      }
      # final height in cm
      if(is.numeric(h)) {
        h <- h + 16 * sizing_hints$no_char_vars
        h <- round(h * 0.0264583333, digits = 4)
        h <- paste0(h, "cm;")
      }
      # final width in cm
      if(is.numeric(w)) {
        w <- w + 16 * sizing_hints$no_char_cat
        w <- round(w * 0.0264583333, digits = 4)
        w <- paste0(w, "cm;")
      }
    }
    ##CASE 3: scatt_plot, only used now for univariate outliers
  }  else if (!is.null(sizing_hints$figure_type_id) &&
              sizing_hints$figure_type_id == "scatt_plot") {
    #In case of missing hints, give an error
    if (is.null(sizing_hints$range) ||
        is.null(sizing_hints$number_of_vars)) {
      util_error(c("Internal error, sorry, please report:",
                   "Missing sizing hint values for %s"),
                 dQuote(sizing_hints$figure_type_id))
    }
    if (sizing_hints$range < 20) {
      h <- 200
    } else if (sizing_hints$range > 19 &&
               sizing_hints$range < 101) {
      h <- 350
    } else if (sizing_hints$range > 100 &&
               sizing_hints$range < 251) {
      h <- 400
    } else {
      h <- 600
    }
    if (sizing_hints$number_of_vars == 1) {
      w <- 750
    } else {
      # should be never reached as univariate outliers always have 1 var
      w <- "calc(var(--screen-width) * 0.5 * 1px);"
    }

    if(sizing_hints$no_char_y < 4) {
      sizing_hints$no_char_y <- 4
    }
    # final height in cm
    if(is.numeric(h)) {
      h <- round(h * 0.0264583333, digits = 4)
      h <- paste0(h, "cm;")
    }
    # final width in cm
    if(is.numeric(w)) {
      w <- w + 16 * sizing_hints$no_char_y
      w <- round(w * 0.0264583333, digits = 4)
      w <- paste0(w, "cm;")
    }
    ##CASE 4: bar_limit
  } else if (!is.null(sizing_hints$figure_type_id) &&
             sizing_hints$figure_type_id == "bar_limit") {
    #In case of missing hints, give an error
    if (is.null(sizing_hints$range) ||
        is.null(sizing_hints$no_bars_in_all_w)) {
      util_error(c("Internal error, sorry, please report:",
                   "Missing sizing hint values for %s"),
                 dQuote(sizing_hints$figure_type_id))
    }
    # here rotated would be always FALSE, as the plots are always with vertical
    # bars
    if (sizing_hints$no_bars_in_all_w < 31) {
      w <- 500
    } else if (sizing_hints$no_bars_in_all_w > 30 &&
               sizing_hints$no_bars_in_all_w < 41) {
      w <- sizing_hints$no_bars_in_all_w * 20
    } else {
      w <- "calc(var(--screen-width) * 0.6 * 1px);"
    }
      if (sizing_hints$range < 301 ||
          sizing_hints$no_bars_in_all_w < 21) {
        h <- 350
      } else if (sizing_hints$range > 300 &&
                 sizing_hints$range < 501) {
        h <- 450
      } else {
        h <- "calc(var(--screen-height) * 0.5 * 1px);"
      }

    if(sizing_hints$no_char_y < 4) {
      sizing_hints$no_char_y <- 4
    }
    # final height in cm
    if(is.numeric(h)) {
      h <- round(h * 0.0264583333, digits = 4)
      h <- paste0(h, "cm;")
    }
    # final width in cm
    if(is.numeric(w)) {
      w <- w + 16 * sizing_hints$no_char_y
      w <- round(w * 0.0264583333, digits = 4)
      w <- paste0(w, "cm;")
    }
    ##CASE 5: dot_loess
  } else if (!is.null(sizing_hints$figure_type_id) &&
             sizing_hints$figure_type_id == "dot_loess") {
    #In case of missing hints, give an error
    if (is.null(sizing_hints$n_groups)) {
      util_error(c("Internal error, sorry, please report:",
                   "Missing sizing hint values for %s"),
                 dQuote(sizing_hints$figure_type_id))
    }
    # always with time on the x axis
    if (sizing_hints$n_groups < 21) {
      h <- 400
      w <- "calc(var(--screen-width) * 0.5 * 1px);"
    } else if (sizing_hints$n_groups > 20) {
      h <- "calc(var(--screen-height) * 0.65 * 1px);"
      w <- "calc(var(--screen-width) * 0.9 * 1px);"
    }

    if(sizing_hints$no_char_y < 4) {
      sizing_hints$no_char_y <- 4
    }
    # final height in cm
    if(is.numeric(h)) {
      h <- round(h * 0.0264583333, digits = 4)
      h <- paste0(h, "cm;")
    }
    # final width in cm
    if(is.numeric(w)) {
      w <- w + 16 * sizing_hints$no_char_y
      w <- round(w * 0.0264583333, digits = 4)
      w <- paste0(w, "cm;")
    }
    ##CASE 6: marg_plot
  } else if (!is.null(sizing_hints$figure_type_id) &&
             sizing_hints$figure_type_id == "marg_plot") {
    #In case of missing hints, give an error
    if (is.null(sizing_hints$n_groups) ||
        is.null(sizing_hints$type_plot)) {
      util_error(c("Internal error, sorry, please report:",
                   "Missing sizing hint values for %s"),
                 dQuote(sizing_hints$figure_type_id))
    }
    # there are 3 options: count_plot that always vary from 0 to 1,
    # violin_plot, and rotated_plot that include 2 different plot with the axis inverted
    if (sizing_hints$type_plot == "count_plot") {
      # MARG_PLOT CASE1: COUNT_PLOT
      h <- 400
      if (sizing_hints$n_groups < 11) {
        w <- 400
      } else if (sizing_hints$n_groups >10 &&
                 sizing_hints$n_groups < 21) {
        w <-  600
      } else {
        w <- sizing_hints$n_groups * 30
      }
      if(sizing_hints$no_char_y < 4) {
        sizing_hints$no_char_y <- 4
      }
      # final height in cm
      if(is.numeric(h)) {
        h <- h + 16 * sizing_hints$no_char_x
        h <- round(h * 0.0264583333, digits = 4)
        h <- paste0(h, "cm;")
      }
      # final width in cm
      if(is.numeric(w)) {
        w <- w + 16 * sizing_hints$no_char_y
        w <- round(w * 0.0264583333, digits = 4)
        w <- paste0(w, "cm;")
      }
    } else if (sizing_hints$type_plot == "violin_plot") {
      # MARG_PLOT CASE2: VIOLIN_PLOT
      h <- 400
      if (sizing_hints$n_groups < 7) {
        w <- 600
      } else if (sizing_hints$n_groups > 6 &&
                 sizing_hints$n_groups < 21) {
        w <- 800
      } else {
        w = sizing_hints$n_groups * 40
      }

      if(length(sizing_hints$no_char_y) != 1 || sizing_hints$no_char_y < 4) {
        sizing_hints$no_char_y <- 4
      }
      # final height in cm
      if(is.numeric(h)) {
        h <- h + 16 * sizing_hints$no_char_x
        h <- round(h * 0.0264583333, digits = 4)
        h <- paste0(h, "cm;")
      }
      # final width in cm
      if(is.numeric(w)) {
        w <- w + 16 * sizing_hints$no_char_y
        w <- round(w * 0.0264583333, digits = 4)
        w <- paste0(w, "cm;")
      }
    } else if (sizing_hints$type_plot == "rotated_plot") {
      # MARG_PLOT CASE3: ROTATED_PLOT
      w <- 500
      if (sizing_hints$n_groups < 11) {
        h <- 400
      } else if (sizing_hints$n_groups >10 &&
                 sizing_hints$n_groups < 21) {
        h <- 600
      } else {
        h <- "calc(var(--screen-height) * 0.5 * 1px);"
      }

      if(sizing_hints$no_char_y < 4) {
        sizing_hints$no_char_y <- 4
      }
      # final height in cm
      if(is.numeric(h)) {
        h <- h + 16 * sizing_hints$no_char_y
        h <- round(h * 0.0264583333, digits = 4)
        h <- paste0(h, "cm;")
      }
      # final width in cm
      if(is.numeric(w)) {
        w <- w + 16 * sizing_hints$no_char_x
        w <- round(w * 0.0264583333, digits = 4)
        w <- paste0(w, "cm;")
      }
    }
    ##CASE 7: pairs_plot in des_scatterplot_matrix
  } else if (!is.null(sizing_hints$figure_type_id) &&
             sizing_hints$figure_type_id == "pairs_plot") {
    #In case of missing hints, give an error
    if (is.null(sizing_hints$number_of_vars)) {
      util_error(c("Internal error, sorry, please report:",
                   "Missing sizing hint values for %s"),
                 dQuote(sizing_hints$figure_type_id))
    }

    if (sizing_hints$number_of_vars < 6) {
      h <- 200 * sizing_hints$number_of_vars
      w <- 300 * sizing_hints$number_of_vars
    } else {
      h <- "calc(var(--screen-height) * 0.65 * 1px);"
      w <- "calc(var(--screen-width) * 0.9 * 1px);"
    }

    # final height in cm
    if(is.numeric(h)) {
      h <- round(h * 0.0264583333, digits = 4)
      h <- paste0(h, "cm;")
    }
    # final width in cm
    if(is.numeric(w)) {
      w <- round(w * 0.0264583333, digits = 4)
      w <- paste0(w, "cm;")
    }
    ##CASE 8: connected points with lines in multivariate outliers
    } else if (!is.null(sizing_hints$figure_type_id) &&
               sizing_hints$figure_type_id == "multivar_plot") {
      #In case of missing hints, give an error
      if (is.null(sizing_hints$number_of_vars) ||
          is.null(sizing_hints$range)) {
        util_error(c("Internal error, sorry, please report:",
                     "Missing sizing hint values for %s"),
                   dQuote(sizing_hints$figure_type_id))
      }

      if (sizing_hints$number_of_vars < 7) {
        w <- 100 + 200 * sizing_hints$number_of_vars
      } else {
        w <- "calc(var(--screen-width) * 0.9 * 1px);"
      }

      if (sizing_hints$range < 101) {
        h <- 400
      } else if (sizing_hints$range > 100 &&
                 sizing_hints$range < 401) {
        h <- 600
      } else  {
        h <- "calc(var(--screen-height) * 0.65 * 1px);"
      }

      if(sizing_hints$no_char_y < 4) {
        sizing_hints$no_char_y <- 4
      }

      # final height in cm
      if(is.numeric(h)) {
        h <- round(h * 0.0264583333, digits = 4)
        h <- paste0(h, "cm;")
      }
      # final width in cm
      if(is.numeric(w)) {
        w <- w + 16 * sizing_hints$no_char_y
        w <- round(w * 0.0264583333, digits = 4)
        w <- paste0(w, "cm;")
      }
    ##CASE 999: test only
    } else if (!is.null(sizing_hints$figure_type_id) &&
               sizing_hints$figure_type_id == "pass_through") {
      w <- sizing_hints$w
      h <- sizing_hints$h
    } else {
    # none of the plots before
    w <- NULL
    h <- NULL
  }
  return(list(figure_type_id = sizing_hints$figure_type_id,
              w = w,
              h = h))
}

#' Ensure, sizing hint sticks at the `dqr`, only
#'
#' @param dqr a `dataquieR` result
#' @param x a plot object
#'
#' @return a list with `dqr` and `x`, but fixed
#' @keywords internal
util_fix_sizing_hints <- function(dqr, x) {
  if (!is.null(attr(x, "sizing_hints"))) {
    if (!is.null(attr(dqr, "sizing_hints"))) {
      if (!identical(attr(x, "sizing_hints"),
                     attr(dqr, "sizing_hints"))) # FIXME: This is not numerically stable
        util_error(c("Internal error: two different",
                     "sizing_hints should not be,",
                     "sorry. Please report this error."))
    }
    attr(dqr, "sizing_hints") <- attr(x, "sizing_hints")
    attr(x, "sizing_hints") <- NULL
  }
  return(list(dqr = dqr, x = x))
}
