#' Combine results for Single Variables
#'
#' to, e.g., a data frame with one row per variable or a similar heat-map,
#' see [print.ReportSummaryTable()].
#'
#' @param all_of_f all results of a function
#'
#' @return row-bound combined results
#'
#' @keywords internal
util_combine_res <- function(all_of_f) {

  cn <- unique(unlist(lapply(all_of_f, attr, "cn")))
  if (length(cn) != 1) {
    cn <- ""
  }

  all_of_f <- lapply(all_of_f, function(x) {
    class(x) <- setdiff(class(x), "dataquieR_result")
    x
  })

  # combine results for the indicator functions related overview

  nms <- sub("^([^\\.]*).*$", "\\1", names(all_of_f))

  util_stop_if_not(length(unique(nms)) == 1)
  cll <- nms[[1]]
  fkt <- util_map_by_largest_prefix(
    cll,
    haystack = util_all_ind_functions())
  # try to combine results, mostly relevant for indicator function outputs (where single variable is false)
  # check the results for the existence of certain output types
  # each result is a logical vector
  PLOTs <- !vapply(lapply(all_of_f, `[[`, "SummaryPlot"), is.null,
                   FUN.VALUE = logical(1))
  PLOT_LISTSs <- !vapply(lapply(all_of_f, `[[`, "SummaryPlotList"), is.null,
                         FUN.VALUE = logical(1))

  STs <- !vapply(lapply(all_of_f, `[[`, "SummaryTable"), is.null,
                 FUN.VALUE = logical(1)) # TODO: Do the same for segment and data frame level output
  SDs <- !vapply(lapply(all_of_f, `[[`, "SummaryData"), is.null,
                 FUN.VALUE = logical(1)) # TODO: Do the same for segment and data frame level output
  RSTs <- !vapply(lapply(all_of_f, `[[`, "ReportSummaryTable"), is.null,
                  FUN.VALUE = logical(1))
  NULLs <- vapply(all_of_f, inherits, "dataquieR_NULL",
                  FUN.VALUE = logical(1))

  # TODO: Add "VariableGroupTable", "VariableGroupData",

  ERRORs <- util_collapse_msgs("error", all_of_f)
  WARNINGs <- util_collapse_msgs("warning", all_of_f)
  MESSAGEs <- util_collapse_msgs("message", all_of_f)

  if ((fkt %in% c(
    "con_limit_deviations",  # check if we are working with a limits function
    "con_hard_limits",
    "con_soft_limits",
    "con_detection_limits"
    )) && (any(PLOT_LISTSs) || any(PLOTs))) {
    return(all_of_f) # use the limits plots, if available, not the ReportSummaryTable if we are working with a limits function
    # check if we have to combine some single variable results to a multivariable result
  } else if ((!any(RSTs)) && (any(PLOT_LISTSs) || any(PLOTs))) {
    return(all_of_f) # use the plots, if available
    # otherwise, use the ReportSummaryTable, SummaryData, or SummaryTable, in that order of preference
    # and combine the results (rbind) using util_combine_res
  } else if (any(RSTs)) {
    RESs <- RSTs
    slot <- "ReportSummaryTable"
  } else if (any(SDs)) {
    RESs <- SDs
    slot <- "SummaryData"
  } else if (any(STs)) {
    RESs <- STs
    slot <- "SummaryTable"
  } else { # TODO: Add "VariableGroupTable", "VariableGroupData",
    return(all_of_f)
  }

  util_stop_if_not(all(RESs | NULLs)) # NULLs contains all results that are NULL, check that all results can be combined, except missing results

  # extract all call attributes to combine them
  clls <- lapply(all_of_f[RESs & !NULLs], attr, "call")
  clls <- lapply(clls, deparse)
  clls <- vapply(clls, paste0, collapse = "\n",
                 FUN.VALUE = character(1))
  clls <- paste0(clls, "$", slot, collapse = ", \n\t")
  clls <- paste0("rbind(\n\t", clls, "\n)")

  # for ReportSummaryTables: extract and combine VAR_NAMES attributes
  vns <- NULL
  if (any(RSTs)) {
    vns <- unlist(lapply(unname(lapply(all_of_f[RESs & !NULLs], `[[`, slot)), attr, "VAR_NAMES"))
  }

  # copy hover text for table headers
  description <- lapply(lapply(all_of_f[RESs & !NULLs], `[[`, slot), attr, "description")
  description <- unique(description)


  # rescue plain label attributes if assigned for single result tables
  plain_label_atts <- lapply(lapply(lapply(all_of_f[RESs & !NULLs],
                                           `[[`,
                                           slot), `[[`, "Variables"), attr, "plain_label")
  if (all(vapply(plain_label_atts, length, FUN.VALUE = integer(1)) %in% 0:1) &&
      all(vapply(plain_label_atts, is.character, FUN.VALUE = logical(1)) == TRUE) &&
      !any(vapply(plain_label_atts, identical, NA_character_, FUN.VALUE = logical(1)))) {
    plain_label_atts <- unname(unlist(plain_label_atts))
  } else {
    if (!is.null(plain_label_atts) &&
        !all(vapply(plain_label_atts, length, FUN.VALUE = integer(1)) == 0)) {
      util_error(
        "Internal error, sorry, please report: invalid plain label atts")
    }
    plain_label_atts <- NULL
  }
  # combine results (ReportSummaryTable and tables)
  # select results according to the logical vectors, extract the corresponding slots, and then bind by row
  # then write the combined result to all_of_f keeping its original structure (a list of encapsulated lists)
  all_of_f <- list(setNames(list(do.call(util_rbind, lapply(all_of_f[RESs & !NULLs],
                                                       `[[`,
                                                       slot))), nm = slot))
  if (!is.null(all_of_f[[1]][[slot]][["Variables"]])) {
    attr(all_of_f[[1]][[slot]][["Variables"]], "plain_label") <-
      plain_label_atts
  }
  attr(all_of_f[[1]], "call") <- clls
  if (!is.null(vns)) { # attach VAR_NAMES for ReportSummaryTables
    attr(all_of_f[[1]][[slot]], "VAR_NAMES") <- vns
  }

  # add attribute "description" for hover text
  if (length(description)>1) {
    util_warning(c("Internal error: sorry please report to the developers",
                   " - util_combine_res incompatible results"))
  } else if (length(description)==1) {
    attr(all_of_f[[1]][[slot]], "description") <- description[[1]]
  }


  # reattach the error/message/warning attributes but now using the combined version (in ERRORs/WARNINGs,...)
  if (any(trimws(ERRORs) != ""))
    attr(all_of_f[[1]], "error") <- list(simpleError(paste(ERRORs, collapse = "\n")))
  else
    attr(all_of_f[[1]], "error") <- list()
  if (any(trimws(WARNINGs) != ""))
    attr(all_of_f[[1]], "warning") <- list(simpleWarning(paste(WARNINGs, collapse = "\n")))
  else
    attr(all_of_f[[1]], "warning") <- list()
  if (any(trimws(MESSAGEs) != ""))
    attr(all_of_f[[1]], "message") <- list(simpleMessage(paste(MESSAGEs, collapse = "\n")))
  else
    attr(all_of_f[[1]], "message") <- list()
  names(all_of_f) <- cll

  attr(all_of_f[[1]], "cn") <- cn

  return(all_of_f)
}
