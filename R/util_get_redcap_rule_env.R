#' an environment with functions available for `REDcap` rules
#'
#' @return environment
util_get_redcap_rule_env <- function() {
  redcap_env
}

redcap_env <- (function() {
  penv <- new.env(parent = emptyenv())
# TODO: strcat?

  ### generic handling of redcap NAs ####
  # see gen_op below

  R_na2redcap_na <- function(x) {
    if (!is.vector(x)) {
      util_error("no complex arguemnts supported, esp. no %s",
                 dQuote(class(x)),
                 applicability_problem = TRUE)
    }
    if (is.factor(x) && !("" %in% levels(x))) {
      levels(x) <- c(levels(x), "")
    }
    if (prep_dq_data_type_of(x) != DATA_TYPES$DATETIME) { # TODO datetime handling may be read as character? see also tag: 1893839
      if (any(is.na(x))) x[is.na(x)] <- ""
    }
    x
  }

  redcap_na2R_na <- function(x) {
    if (!is.vector(x)) {
      util_error("no complex arguemnts supported, esp. no %s",
                 dQuote(class(x)),
                 applicability_problem = TRUE)
    }
    if (is.factor(x) && !("" %in% levels(x))) {
      levels(x) <- c(levels(x), "")
    }
    # if (prep_dq_data_type_of(x) == DATA_TYPES$DATETIME) {
    #   res <- x
    #   # resrna <- is.na(res)
    #   # if (identical(op, .Primitive("!="))) { # TODO: generalize
    #   #   if (any(resrna)) {
    #   #     res[resrna] <- TRUE
    #   #     res[resrna & is.na(x) & is.na(y)] <- FALSE
    #   #   }
    #   # } else if (identical(op, .Primitive("=="))) {
    #   #   if (any(resrna)) {
    #   #     res[resrna] <- FALSE
    #   #     res[resrna & is.na(x) & is.na(y)] <- TRUE
    #   #   }
    #   # } else {
    #   #   if (any(resrna)) res[resrna] <- ""
    #   # }
    # } else {
    #   xrcna <- is.na(x) | x == ""
    #   if (any(xrcna)) x[is.na(x)] <- ""
    #
    # }
    if (prep_dq_data_type_of(x) != DATA_TYPES$DATETIME) { # TODO datetime handling may be read as character? see also tag: 1893839
      if (any(is.na(x))) x[is.na(x)] <- ""
    }
    x
  }

  decorate_fkt_redcap_na <- function(fkt) {
    function(...) {
      args <- list(...)
      args <- lapply(args, R_na2redcap_na)
      r <- do.call(fkt, args)
      redcap_na2R_na(r)
    }
  }

  gen_op <- function(op) {
    # support REDcap NAs in operators; see decorate_fkt_redcap_na
    function(x, y) {
      if (is.factor(x) && !("" %in% levels(x))) {
        levels(x) <- c(levels(x), "")
      }
      if (is.factor(y) && !("" %in% levels(y))) {
        levels(y) <- c(levels(y), "")
      }
      if (prep_dq_data_type_of(x) == DATA_TYPES$DATETIME &&
          prep_dq_data_type_of(y) == DATA_TYPES$STRING) {
        yrcna <- y == ""
        if (any(yrcna)) y[[yrcna]] <- NA
      }
      if (prep_dq_data_type_of(x) == DATA_TYPES$STRING &&
          prep_dq_data_type_of(y) == DATA_TYPES$DATETIME) {
        xrcna <- x == ""
        if (any(xrcna)) x[[x == ""]] <- NA
      }
      if (prep_dq_data_type_of(x) == DATA_TYPES$DATETIME ||
          prep_dq_data_type_of(y) == DATA_TYPES$DATETIME) {
        res <- op(x, y)
        resrna <- is.na(res)
        if (identical(op, .Primitive("!="))) {
          if (any(resrna)) {
            res[resrna] <- TRUE
            res[resrna & is.na(x) & is.na(y)] <- FALSE
          }
        } else if (identical(op, .Primitive("=="))) {
          if (any(resrna)) {
            res[resrna] <- FALSE
            res[resrna & is.na(x) & is.na(y)] <- TRUE
          }
        } else {
          if (any(resrna)) res[resrna] <- ""
        }
      } else {
        xrcna <- is.na(x) | x == ""
        yrcna <- is.na(y) | y == ""
        if (any(xrcna)) x[is.na(x)] <- ""
        if (any(yrcna)) y[is.na(y)] <- ""
        res <- op(x, y)
      }
      res
    }
  }

  ### opreators/functions ####

  # avoid operator precedence by R
  penv[["or"]] <- gen_op(base::`|`)
  attr(penv[["or"]], "prio") <- -2
  penv[["and"]] <- gen_op(base::`&`)
  attr(penv[["and"]], "prio") <- -1

  penv[["("]] <- base::`(`


  penv[["**"]] <- gen_op(base::`^`) #  must not be in prio=3 if prio 3 has no order
  attr(penv[["**"]], "order") <- -1
  attr(penv[["**"]], "prio") <- 3

  penv[["^"]] <- gen_op(base::`^`)
  attr(penv[["^"]], "prio") <- 3

  penv[["+"]] <- gen_op(base::`+`)
  attr(penv[["+"]], "prio") <- 1
  penv[["-"]] <- gen_op(base::`-`)
  attr(penv[["-"]], "prio") <- 1
  penv[["*"]] <- gen_op(base::`*`)
  attr(penv[["*"]], "prio") <- 2
  penv[["/"]] <- gen_op(base::`/`)
  attr(penv[["/"]], "prio") <- 2

  penv[["prod"]] <- decorate_fkt_redcap_na(base::`prod`)
  penv[["sum"]] <- decorate_fkt_redcap_na(base::`sum`)

  make_num <- function(number) {
    number[trimws(number) == ""] <- NA
    numnumber <- as.numeric(number)
    if (suppressWarnings((!all(is.na(numnumber) == is.na(number)))))
      util_error("%s not numeric",
                 dQuote(util_deparse1(substitute(number))))
    numnumber
  }

  # All functions from ####
  # https://www.ctsi.ufl.edu/files/2017/06/Calculated-Fields-%E2%80%93-REDCap-How.pdf

  penv[["round"]] <- function(number, decimal_places = 0) {
    numnumber <- make_num(number)
    decimal_places <- make_num(decimal_places)
    redcap_na2R_na(round(numnumber, decimal_places))
  }
  attr(penv[["round"]], "order") <- 99

  penv[["roundup"]] <- function(number, decimal_places = 0) {
    number <- make_num(number)
    decimal_places <- make_num(decimal_places)
    redcap_na2R_na(ceiling(number * (10^(decimal_places))) / (10^(decimal_places)))
  }

  penv[["rounddown"]] <- function(number, decimal_places = 0) {
    number <- make_num(number)
    decimal_places <- make_num(decimal_places)
    redcap_na2R_na(floor(number * (10^(decimal_places))) / (10^(decimal_places)))
  }

  penv[["sqrt"]] <- function(number) {
    number <- make_num(number)
    redcap_na2R_na(sqrt(number))
  }

  penv[["abs"]] <- function(number) {
    number <- make_num(number)
    redcap_na2R_na(abs(number))
  }

  penv[["min"]] <- function(...) {
    x <- make_num(c(...))
    redcap_na2R_na(min(x, na.rm = TRUE))
  }

  penv[["max"]] <- function(...) {
    x <- make_num(c(...))
    redcap_na2R_na(max(x, na.rm = TRUE))
  }

  penv[["mean"]] <- function(...) {
    x <- make_num(c(...))
    redcap_na2R_na(mean(x, na.rm = TRUE))
  }

  penv[["median"]] <- function(...) {
    x <- make_num(c(...))
    redcap_na2R_na(median(x, na.rm = TRUE))
  }

  penv[["stdev"]] <- function(...) {
    x <- make_num(c(...))
    redcap_na2R_na(sd(x, na.rm = TRUE))
  }

  penv[["as.POSIXct"]] <- as.POSIXct
  attr(penv[["as.POSIXct"]], "internal") <- TRUE

  penv[["datediff"]] <- function(date1, # TODO: if used with vectors of posixct, this crashes: util_eval_rule(util_parse_redcap_rule('datediff(set("2020-11-01", "", "2020-11-10"), "2020-01-01", "y", "Y-M-D", true)'), ds1 = ds1, meta_data = meta_data, use_value_labels = FALSE)
                                 date2,
                                 units = "",
                                 date_format = "Y-M-D",
                                 Return_Signed_Value = FALSE) {

    util_expect_scalar(arg_name = units,
                       allow_na = FALSE, allow_null = FALSE,
                       check_type = function(x) x %in% c("y", "M", "d",
                                           "h", "m", "s", ""))

    util_expect_scalar(arg_name = date_format,
                       allow_na = FALSE, allow_null = FALSE,
                       check_type = function(x) x %in% c("Y-M-D",
                                                         "M-D-Y",
                                                         "D-M-Y"))

    try_formats <- list(
      `Y-M-D` = list("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d"),
      `M-D-Y` = list("%m-%d-%Y %H:%M:%OS", "%m-%d-%Y"),
      `D-M-Y` = list("%d-%m-%Y %H:%M:%OS", "%d-%m-%Y")
    )[[date_format]]

    util_expect_scalar(arg_name = Return_Signed_Value,
                       allow_na = FALSE, allow_null = FALSE,
                       check_type = is.logical)

    util_expect_scalar(arg_name = date1,
                       allow_na = TRUE,
                       allow_null = FALSE,
                       allow_more_than_one = TRUE,
                       check_type = function(x) {
                         prep_dq_data_type_of(x) %in%
                           c(DATA_TYPES$DATETIME,
                             DATA_TYPES$STRING)
                       })

    util_expect_scalar(arg_name = date2,
                       allow_na = TRUE,
                       allow_null = FALSE,
                       allow_more_than_one = TRUE,
                       check_type = function(x) {
                         prep_dq_data_type_of(x) %in%
                           c(DATA_TYPES$DATETIME,
                             DATA_TYPES$STRING)
                       })

    if (prep_dq_data_type_of(date1) == DATA_TYPES$STRING) {
      try({
        date1 <- as.POSIXct(date1, tryFormats = unlist(try_formats))
      }, silent = TRUE)
      if (prep_dq_data_type_of(date1) != DATA_TYPES$DATETIME) {
        date1 <- NA
      }
    }
    if (prep_dq_data_type_of(date2) == DATA_TYPES$STRING) {
      try({
        date2 <- as.POSIXct(date2, tryFormats = unlist(try_formats))
      }, silent = TRUE)
      if (prep_dq_data_type_of(date2) != DATA_TYPES$DATETIME) {
        date2 <- NA
      }
    }

    if (units == "") units <- "a"

    u <-
      c("a" = "auto",
        "s" = "secs",
        "m" = "mins",
        "h" = "hours",
        "d" = "days",
        "M" = "months",
        "y" = "years")[[units]]

    if (units %in% c("M", "y")) {
      u <- "days"
    }

    res <- as.numeric(difftime(date1, date2,
                               units = u))

    if (units == "M") {
      res <- res / 30.44
    } else if (units == "y") {
      res <- res / 365.2425
    }

    if (!Return_Signed_Value) {
      res <- abs(res)
    }

    redcap_na2R_na(res)
  }

  # but use a new rule for this, it collides with REDcapexpression, use sth. like concat(keyword("if"),  charParser("("), logical_expression(), ...))
  # if ([xx] < 99, [xx], "NaN")
  # penv[["="]] <- base::`==`
  # penv[["=="]] <- base::`==`
  # penv[["!="]] <- base::`!=`
  # penv[["<>"]] <- base::`!=`
  # penv[[">="]] <- base::`>=`
  # penv[["<="]] <- base::`<=`
  # penv[["<"]] <- base::`<`
  # penv[[">"]] <- base::`>`
  # penv[["if"]] <- base::`ifelse`

  # Note: For each name, ensure, that no op function with the same prefix is in the same prio class
  # So, attr(., "order") is needed. Default order will be 0
  penv[["="]] <- gen_op(`==`)
  attr(penv[["="]], "prio") <- 0
  penv[["=="]] <- gen_op(`==`) #  must not be in prio=0 if prio 0 has no order
  attr(penv[["=="]], "order") <- -1
  attr(penv[["=="]], "prio") <- 0
  penv[["!="]] <- gen_op(`!=`)
  attr(penv[["!="]], "prio") <- 0
  penv[["<>"]] <- gen_op(`!=`) #  must not be in prio=0 if prio 0 has no order
  attr(penv[["<>"]], "order") <- -1
  attr(penv[["<>"]], "prio") <- 0
  penv[["<"]] <- gen_op(`<`)
  attr(penv[["<"]], "prio") <- 0
  penv[[">"]] <- gen_op(`>`)
  attr(penv[[">"]], "prio") <- 0
  penv[["<="]] <- gen_op(`<=`)
  attr(penv[["<="]], "prio") <- 0
  attr(penv[["<="]], "order") <- -1
  penv[[">="]] <- gen_op(`>=`)
  attr(penv[[">="]], "prio") <- 0
  attr(penv[[">="]], "order") <- -1

  penv[["set"]] <- function(...) {
    base::unique(base::c(...))
  }

  penv[["interval"]] <- function(inc_l, low, upp, inc_u) {
    util_expect_scalar(inc_l, check_type = base::is.logical)
    util_expect_scalar(inc_u, check_type = base::is.logical)
    if (inherits(low, "POSIXct")) {
      util_expect_scalar(low, check_type = function(x) {
        inherits(low, "POSIXct")
      })
    } else {
      util_expect_scalar(low, check_type = base::is.numeric)
    }
    if (inherits(upp, "POSIXct")) {
      util_expect_scalar(upp, check_type = function(x) {
        inherits(upp, "POSIXct")
      })
    } else {
      util_expect_scalar(upp, check_type = base::is.numeric)
    }
    i <- list(inc_l = inc_l, low = low, upp = upp, inc_u = inc_u)
    class(i) <- "interval"
    i
  }
  attr(penv[["interval"]], "order") <- -1 # befor in

  my_in <- gen_op(`%in%`)

  penv[["in"]] <- function(x, table) {
    if (inherits(table, "interval")) {
      r <- rep(TRUE, length(x))
      inc_l <- table$inc_l
      if (is.infinite(table$low) && table$low < 0) { # -Inf
        inc_l <- TRUE # despite the math rules, we have to "include" Inf, so
        # that -Inf in (-Inf; 0) works
      }
      inc_u <- table$inc_u
      if (is.infinite(table$upp) && table$upp > 0) { # +Inf
        inc_u <- TRUE # despite the math rules, we have to "include" Inf, so
        # that Inf in (Inf; Inf) works
      }
      if (inc_l) {
        r <- r & (x >= table$low)
      } else {
        r <- r & (x > table$low)
      }
      if (inc_u) {
        r <- r & (x <= table$upp)
      } else {
        r <- r & (x < table$upp)
      }
      r
    } else {
      my_in(x, table)
    }
  }
  attr(penv[["in"]], "prio") <- -1

  penv[["not in"]] <- function(x, table) {
    !penv[["in"]](x, table)
  }
  attr(penv[["not in"]], "prio") <- -1
  attr(penv[["not in"]], "order") <- -1

  penv[["if"]] <- decorate_fkt_redcap_na(function(test, yes, no) {
    ifelse(test, yes, no)
  })

  penv[["not"]] <- decorate_fkt_redcap_na(function(x0) {
    x <- suppressWarnings(as.logical(x0))
    if (any(is.na(x) != is.na(x0))) {
      util_warning("Not a logical value: %s. %s returns %s",
                   dQuote(x),
                   sQuote("not"),
                   dQuote(""))
      NA
    } else {
      !x
    }
  })

  `in` <- function(...) {
    `%in%`(...)
  }

#  penv[["id"]] <- function(x) x

  makeActiveBinding("today", function() { as.POSIXct(Sys.Date()) }, penv)

  penv
})()
