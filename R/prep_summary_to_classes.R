#' Classify metrics from a report summary table
#'
#' @param report_summary [list()] as returned by
#'   [prep_extract_summary()]
#'
#' @return [data.frame] classes for the report summary table, long format
#' @family summary_functions
#' @export
prep_summary_to_classes <- function(report_summary) {
  te <- topenv(parent.frame(1)) # see https://stackoverflow.com/a/27870803
  if (!(isNamespace(te) && getNamespaceName(te) == "dataquieR")) {
    lifecycle::deprecate_soft("2.1.0.9007",
                              "prep_summary_to_classes()")
  }

  if (!inherits(report_summary, "dq_report2_summary") &&
      all(c("Data", "Table", "meta_data") %in% names(report_summary))){
    util_error(c("%s must be returned by %s or this function",
                 "i.e. feature %s, %s and %s"),
               sQuote("report_summary"),
               sQuote("prep_extract_summary"),
               sQuote("Data"), sQuote("Table"), sQuote("meta_data"))
  }

  report_summary_table <- report_summary$Table
  meta_data <- report_summary$meta_data

  metrices_maybe_with_call_name <-
    setNames(nm = setdiff(colnames(report_summary_table),
                          c(STUDY_SEGMENT, VAR_NAMES)))

  metrices <- gsub("^.*\\.", "", metrices_maybe_with_call_name)

  have_at_least_some_call_names <-
    any(grepl(".", metrices_maybe_with_call_name, fixed = TRUE), na.rm = TRUE)

  all_thresholds <- lapply(setNames(nm = metrices), function(metric) {
    r <- util_get_thresholds(indicator_metric = metric, meta_data = meta_data)
    lapply(r, lapply, util_parse_interval) # TODO: Maybe NA?
  })

  all <- lapply(metrices_maybe_with_call_name,
                function(metric_maybe_with_call_name) {
                  metric <- metrices[[metric_maybe_with_call_name]]
                  thresholds <- all_thresholds[[metric]]
                  col <- setNames(as.numeric(report_summary_table[[metric_maybe_with_call_name]]),
                                  nm = report_summary_table[[VAR_NAMES]])

                  classes <- lapply(setNames(nm = report_summary_table[[VAR_NAMES]]),
                                    FUN = function(vn) {
                                      setNames(mapply(
                                        SIMPLIFY = FALSE,
                                        USE.NAMES = FALSE,
                                        clss = names(thresholds[[vn]]),
                                        int = thresholds[[vn]],
                                        FUN = function(clss, int) {
                                          if (is.na(col[vn]) || !inherits(int, "interval"))
                                            return(NA)
                                          redcap_env$`in`(col[vn], int)
                                        }), nm = names(thresholds[[vn]]))
                                    })

                  values <- report_summary$Data[[metric_maybe_with_call_name]]
                  values_raw <- report_summary$Table[[metric_maybe_with_call_name]]

                  classes <- lapply(classes, vapply, FUN.VALUE = logical(1), identity)
                  classes <- lapply(classes, which)
                  classes_nrs <- lapply(classes, unname)
                  classes_nrs <- unlist(classes_nrs)
                  classes <- lapply(classes, names)
                  missing <- vapply(classes, length, FUN.VALUE = integer(1)) ==
                    0
                  classes[missing] <- NA
                  classes <- unlist(classes) # TODO: split it up here, return this first, then run the aggregation on it later
                  data.frame(class = classes,
                             VAR_NAMES = names(classes),
                             indicator_metric = rep(metric_maybe_with_call_name, length(classes)),
                             value = values,
                             values_raw = values_raw,
                             n_classes =
                               vapply(FUN.VALUE = integer(1),
                                      names(classes),
                                      function(vn) {
                                        r <- suppressWarnings(max(as.integer(names(thresholds[[vn]])),
                                            na.rm = TRUE))
                                        if (!is.finite(r)) {
                                          NA_integer_
                                        } else {
                                          r
                                        }
                                      })
                  )
                })
  all <- util_rbind(data_frames_list = all)
  rownames(all) <- NULL
  if (!prod(dim(all))) {
    all <- report_summary_table[, c(VAR_NAMES, STUDY_SEGMENT)]
  } else {
    all <- merge(all, report_summary_table[, c(VAR_NAMES, STUDY_SEGMENT)], by = VAR_NAMES, all.x = TRUE)
    if (have_at_least_some_call_names) {
      splitted <- all$indicator_metric # not yet splitted
      splitted[util_empty(splitted)] <- ""
      splitted <- strsplit(splitted, ".", fixed = TRUE)
      tuple_sizes <- vapply(splitted, length, FUN.VALUE = integer(1)) # should be always 1 or 2 according to function name and result column conventions
      valid_splitted <- tuple_sizes %in% 1:2
      if (!all(valid_splitted)) {
        util_warning(c("For some reason, the result names in this summary",
                       "feature columns with more than one . or empty names,",
                       "I'll ignore these. Internal error, please report"))
      }
      call_names <- rep("", nrow(all))
      indicator_metric <- rep("", nrow(all))
      call_names[tuple_sizes == 2] <-
        vapply(splitted[tuple_sizes == 2], `[[`, 1, FUN.VALUE = character(1))
      indicator_metric[tuple_sizes == 2] <-
        vapply(splitted[tuple_sizes == 2], `[[`, 2, FUN.VALUE = character(1))
      indicator_metric[tuple_sizes == 1] <-
        vapply(splitted[tuple_sizes == 1], `[[`, 1, FUN.VALUE = character(1))
      all$indicator_metric <- indicator_metric
      all$call_names <- call_names
    } else {
      all$call_names <- ""
    }
  }
  class(all) <- c("dq_report2_summaryclasses", "data.frame")
  return(all)
}


util_metrics_to_classes <- function(rs_table_long, meta_data, entity = "ITEM") {
  used_metrics <- setNames(nm = unique(rs_table_long[["indicator_metric"]]))

  all_thresholds <- lapply(used_metrics,
                           function(metric) {
    r <- util_get_thresholds(indicator_metric = metric, meta_data = meta_data)
    lapply(r, lapply, util_parse_interval)
  })

  all <- lapply(used_metrics,
                function(metric) {
                  curr_metric_rows <- rs_table_long$indicator_metric ==
                    metric
                  if (startsWith(metric, "MSG_")) {
                    res <- rs_table_long[curr_metric_rows, , FALSE]
                    res$class <- res$values_raw
                    return(res)
                  }
                  rs_table_long[curr_metric_rows, "values_raw"][rs_table_long[
                    curr_metric_rows, "values_raw"] == "TRUE"] <- 1
                  rs_table_long[curr_metric_rows, "values_raw"][rs_table_long[
                    curr_metric_rows, "values_raw"] == "FALSE"] <- 0
                  col <- suppressWarnings(as.numeric(
                    rs_table_long[curr_metric_rows, "values_raw"]))
                  if (entity == "ITEM") {
                    names(col) <- paste0(rs_table_long[curr_metric_rows, "call_names"],
                                         ".",
                                         rs_table_long[curr_metric_rows, VAR_NAMES])
                  } else { # TODO: Support other entities better by metadata model
                    names(col) <- paste0(rs_table_long[curr_metric_rows, "call_names"],
                                         seq_len(length(col))) # FIXME: so far, use first variables metric, which is not correct
                  }
                  thresholds <- all_thresholds[[metric]]

                  classes <- lapply(setNames(nm = names(col)),
                                    FUN = function(cn.vn) {
                                      if (entity == "ITEM") {
                                        vn <- util_sub_string_right_from_.(cn.vn)
                                      } else {
                                        vn <- names(thresholds)[[1]] # FIXME: so far, use first variables metric, which is not correct
                                      }
                                      cl_res <- setNames(mapply(
                                        SIMPLIFY = FALSE,
                                        USE.NAMES = FALSE,
                                        clss = names(thresholds[[vn]]),
                                        int = thresholds[[vn]],
                                        FUN = function(clss, int) {
                                          if (is.na(col[cn.vn]) || !inherits(int, "interval"))
                                            return(NA)
                                          valraw <- col[cn.vn]
                                          redcap_env$`in`(valraw, int)
                                        }), nm = names(thresholds[[vn]]))
                                      # if (metric == "FLG_acc_ud_prop" &&
                                      #     cn.vn == "acc_distributions_prop.v01002") browser()
                                      return(cl_res)
                                    })
# if (metric == "FLG_acc_ud_prop") browser()
                  classes <- lapply(classes, vapply, FUN.VALUE = logical(1),
                                    identity)
                  classes <- lapply(classes, which)
                  if (
                    any(vapply(classes, length, FUN.VALUE = integer(1))) > 1) {
                    util_warning(
                      c("Detected invalid grading rule with",
                        "overlapping intervals. Choosing the worst class."),
                      applicability_problem = TRUE
                      )
                    classes <- lapply(classes, tail, 1)
                  }
                  classes_nrs <- lapply(classes, unname)
                  classes_nrs <- unlist(classes_nrs)
                  classes <- lapply(classes, names)
                  missing <- vapply(classes, length, FUN.VALUE = integer(1)) ==
                    0
                  classes[missing] <- NA
                  classes <- unlist(classes) # TODO: split it up here, return this first, then run the aggregation on it later
                  res <- rs_table_long[curr_metric_rows, , FALSE]
                  res$class <- as.character(util_as_integer_cat(
                    util_as_cat(classes)))
                  res$n_classes <- vapply(FUN.VALUE = integer(1),
                                          names(classes),
                                          function(vn) {
                                            r <- suppressWarnings(max(as.integer(names(thresholds[[vn]])),
                                                                      na.rm = TRUE))
                                            if (!is.finite(r)) {
                                              NA_integer_
                                            } else {
                                              r
                                            }
                                          })
                  res
                })
  all <- util_rbind(data_frames_list = all)
  rownames(all) <- NULL
  # fill up all missing metrics, if at least once, a function returned them
  metrics_known_to_be_in_functions <- tapply(rs_table_long,
                                             rs_table_long$function_name,
                                             function(x) {
                                               r <- unique(x$indicator_metric)
                                               r[!startsWith(r, "CAT_") &
                                                   ! startsWith(r, "MSG_")]
                                              },
                                             simplify = FALSE)

  what_i_need <- unique(all[, setdiff(colnames(all),
                                 c("indicator_metric", "n_classes",
                                   "class", "value", "values_raw"))])

  metrics_known_to_be_in_functions2 <- util_rbind(data_frames_list =
               lapply(names(metrics_known_to_be_in_functions),
                      function(function_name) {
    cbind.data.frame(
      function_name = rep(function_name,
                          length(metrics_known_to_be_in_functions[[function_name]])),
      indicator_metric =
        metrics_known_to_be_in_functions[[function_name]],
      stringsAsFactors = FALSE)
                        }
    ))

  if (!prod(dim(metrics_known_to_be_in_functions2))) {
    what_i_really_need <-
      what_i_need
  } else {
    what_i_really_need <-
      merge(what_i_need, metrics_known_to_be_in_functions2, by = "function_name")
  }

  to_add <- dplyr::anti_join(
    what_i_really_need,
    all[, colnames(what_i_really_need), drop = FALSE],
    by = colnames(what_i_really_need)
  )

  if (nrow(to_add) > 0) {
    to_add[, setdiff(colnames(all),
                     colnames(what_i_really_need))] <- rep(NA, nrow(to_add))

    to_add <- to_add[, colnames(all), drop = FALSE]

    all <- util_rbind(all, to_add)
  }

  class(all) <- c("dq_report2_summaryclasses", "data.frame")
  return(all)
}
