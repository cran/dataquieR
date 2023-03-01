#' Produce a condition function
#'
#' @param .condition_type [character] the type of the conditions being created
#'   and signaled by the function, "error", "warning", or "message"
util_condition_constructor_factory <- function(
                       .condition_type = c("error", "warning", "message")) {

  .condition_type <- match.arg(.condition_type)

  .signal_fkt <- switch (.condition_type,
                         error = stop,
                         warning = warning,
                         message = message)

  .cond_constructor <-
    switch (.condition_type,
            error = simpleError,
            warning = simpleWarning,
            message = simpleMessage)

  .caller_control_att <- paste0("dataquieR.",
                                toupper(.condition_type),
                                "S_WITH_CALLER")
  .caller_control_att_default <- get(paste0(.caller_control_att, "_default"))

  function(m, ..., applicability_problem = NA,
           integrity_indicator = "none", level = 0) {
    if (identical(getOption("dataquieR.debug", FALSE), TRUE)) {
      browser()
    }
    m_args <- eval(quote(force(list(...))))
    # shows some false positive note on possible misplaced
    # ...
    # m_args <- lapply(
    #   rlang::call_args(rlang::call_match(dots_expand = FALSE))[["..."]], eval,
    #   envir = parent.frame())
    util_expect_scalar(integrity_indicator, allow_na = TRUE,
                       check_type = is.character)
    if (!(integrity_indicator %in% c(na.omit(subset(util_get_concept_info("dqi"),
                                                    get("Dimension") == "Integrity",
                                                    select = "abbreviation",
                                                    drop = TRUE)), "none"))) {
      util_error(
        "Internal error: %s is not a supported %s. Did you update %s?",
        dQuote(integrity_indicator),
        sQuote("integrity_indicator"),
        sQuote("dqi.rds"))
    }
    if (integrity_indicator == "none") {
      integrity_indicator <- NA_character_
    }
    util_stop_if_not(length(applicability_problem) == 1 &&
                       is.logical(applicability_problem))
    start_from_call <- util_find_first_externally_called_functions_in_stacktrace()
    start_from_call <- length(sys.calls()) - start_from_call # refers to reverted sys.calls, so mirror the number
    caller. <- sys.call(1)
    calling <- character(0)
    if (!is.na(start_from_call)) {
      try(silent = TRUE, {
        caller. <- sys.call(start_from_call)
        calling <- util_deparse1(sys.call(start_from_call + 1))
        calling <- paste("when calling", calling)
      })
    } else {
      start_from_call <- 1
    }

    # https://stat.ethz.ch/pipermail/r-help/2011-November/295273.html
    str <- vapply(FUN.VALUE = character(1),
                  rev(sys.calls()), function(sc)
                    paste0(deparse(sc, nlines = 2),
                           collapse = "\n"))[
                             -seq_len(start_from_call)]
    if (!!length(str)) {
      stacktrace <- (paste0(paste0("> ", str),
                            collapse = "\n"))
    } else {
      stacktrace <- character(0)
    }

    if (identical(as.logical(getOption("dataquieR.CONDITIONS_WITH_STACKTRACE", FALSE)), FALSE)) {
      stacktrace <- ""
    } else {
    }
    if (identical(as.logical(getOption(.caller_control_att, .caller_control_att_default)), FALSE)) {
      caller. <- NULL
    }
    if (inherits(m, "condition")) {
      m <- conditionMessage(m)
      if (m == "") {
        m <- "Error"
      }
      ec <-
        .cond_constructor(paste(c(m, calling, stacktrace), collapse = "\n"), call = caller.)

    } else {
      mm <- paste0(m,
                   collapse =
                     " ")
      if (nchar(mm) > 8192) {
        mm <- substr(mm, 1, 8192)
        mm <- sub("(%[^%]$)", "\\1", mm, perl = TRUE)
      }
      ec <-
        .cond_constructor(paste0(c(do.call("sprintf", c(
          list(fmt = mm),
          m_args)),
                                   calling, stacktrace), collapse = "\n"),
                    call = caller.)
    }
    attr(ec, "applicability_problem") <- applicability_problem
    attr(ec, "integrity_indicator") <- integrity_indicator
    if (level >= getOption("dataquieR.CONDITIONS_LEVEL_TRHESHOLD",
                  dataquieR.CONDITIONS_LEVEL_TRHESHOLD_default) ||
        inherits(ec, "error")) {
      # .signal_fkt(ec)
      rlang::cnd_signal(ec)
    }
    invisible(ec)
  }
}
